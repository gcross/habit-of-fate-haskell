{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements, text)

import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Catch
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LazyBS
import Data.CallStack
import Data.Int
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Network.HTTP.Client hiding (httpNoBody)
import Network.HTTP.Conduit (Response(..), responseStatus)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (ok200)
import Network.Wai.Handler.Warp
import System.Random (StdGen)
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Gen (chooseAny)
import Test.SmallCheck.Series (Serial(..), (\/), cons0, cons1, cons2, cons3)
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Data.Time.Zones.All
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag, parseTags)
import Web.Scotty (Parsable(..))

import HabitOfFate.API
import HabitOfFate.Data.Account
import HabitOfFate.Data.Age
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.InputHabit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import qualified HabitOfFate.Quests.Forest as Forest
import qualified HabitOfFate.Quests.Forest.Stories as Forest
import HabitOfFate.Quest.StateMachine as StateMachine
import HabitOfFate.Quests
import HabitOfFate.Server
import HabitOfFate.Story
import HabitOfFate.Substitution

dayHour d h = LocalTime (ModifiedJulianDay d) (TimeOfDay h 0 0)
day = flip dayHour 0

newtype LocalToSecond = LocalToSecond LocalTime deriving (Eq, Ord, Show)
instance Arbitrary LocalToSecond where
  arbitrary =
    LocalToSecond
      <$> (LocalTime
            <$> (ModifiedJulianDay <$> choose (0, 999999))
            <*> (TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> (fromIntegral <$> choose (0 ∷ Int, 59)))
          )

day_of_week_names ∷ [String]
day_of_week_names = ["M", "T", "W", "Th", "F", "Sat", "Sun"]

repeatDays ∷ DaysToRepeat → String
repeatDays days_to_repeat =
  intercalate "+"
    [ day_of_week_name
    | (days_to_repeat_lens_, day_of_week_name) ←
        zip (V.toList days_to_repeat_lenses) day_of_week_names
    , days_to_repeat ^# days_to_repeat_lens_
    ]

instance Arbitrary DaysToKeep where
  arbitrary = do
    n ← arbitrary `suchThat` (> 0)
    elements [KeepNumberOfDays, KeepDaysInPast] <&> ($ n)

instance Arbitrary DaysToRepeat where
  arbitrary =
    (DaysToRepeat
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    )
    `suchThat`
    (/= def)

instance Arbitrary Repeated where
  arbitrary = oneof [Daily <$> arbitrary, Weekly <$> arbitrary <*> arbitrary]

instance Arbitrary Frequency where
  arbitrary = oneof
    [ pure Indefinite
    , Once <$> arbitrary
    , Repeated <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Scale where arbitrary = elements [None, VeryLow .. VeryHigh]

instance Arbitrary α ⇒ Arbitrary (Tagged α) where
  arbitrary = Tagged <$> (Success <$> arbitrary) <*> (Failure <$> arbitrary)

instance Arbitrary UUID where arbitrary = chooseAny

instance Arbitrary α ⇒ Arbitrary (ItemsSequence α) where
  arbitrary = arbitrary <&> itemsFromList

instance Arbitrary Habit where
  arbitrary =
    Habit
      <$> (arbitrary <&> pack)
      <*> arbitrary
      <*> arbitrary
      <*> (arbitrary <&> setFromList)
      <*> arbitrary

instance Monad m ⇒ Serial m Forest.Label where
  series =
      cons0 Forest.GingerbreadHouse
   \/ cons0 Forest.FoundByCat
   \/ cons0 Forest.FoundByFairy
   \/ cons0 Forest.FairyCircle
   \/ cons0 Forest.Home

instance Monad m ⇒ Serial m Gender where
  series = cons0 Male \/ cons0 Female \/ cons0 Neuter

instance Monad m ⇒ Serial m Text where
  series = series <&> pack

instance Monad m ⇒ Serial m Gendered where
  series = cons2 Gendered

instance Monad m ⇒ Serial m (HashMap Text Gendered) where
  series = series <&> mapFromList

instance Monad m ⇒ Serial m (Forest.SearcherType) where
  series = cons0 Forest.Parent \/ cons0 Forest.Healer

instance Monad m ⇒ Serial m (Forest.Event) where
  series = cons0 Forest.GingerbreadHouseEvent
        \/ cons0 Forest.FoundEvent
        \/ cons0 Forest.FairyCircleEvent
        \/ cons0 Forest.HomeEvent

instance Monad m ⇒ Serial m (Forest.Internal) where
  series = cons3 Forest.Internal

instance (Serial m label, Serial m s, Monad m) ⇒ Serial m (StateMachine.State label s) where
  series = cons3 StateMachine.State

instance Monad m ⇒ Serial m CurrentQuestState where
  series = cons1 Forest

instance Monad m ⇒ Serial m Scale where
  series =
       cons0 None
    \/ cons0 VeryLow
    \/ cons0 Low
    \/ cons0 Medium
    \/ cons0 High
    \/ cons0 VeryHigh

instance Arbitrary Text where
  arbitrary = arbitrary <&> pack

instance Arbitrary Gender where
  arbitrary = elements [Male, Female, Neuter]

instance Arbitrary Gendered where
  arbitrary = Gendered <$> arbitrary <*> arbitrary

instance Arbitrary (HashMap Text Gendered) where
  arbitrary = arbitrary <&> mapFromList

instance Arbitrary Forest.Label where
  arbitrary = elements
    [ Forest.GingerbreadHouse
    , Forest.FoundByCat
    , Forest.FoundByFairy
    , Forest.FairyCircle
    , Forest.Home
    ]

instance Arbitrary Forest.SearcherType where
  arbitrary = elements [Forest.Parent, Forest.Healer]

instance Arbitrary Forest.Event where
  arbitrary = elements
    [ Forest.GingerbreadHouseEvent
    , Forest.FoundEvent
    , Forest.FairyCircleEvent
    ]

instance Arbitrary Forest.Internal where
  arbitrary = Forest.Internal <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary label, Arbitrary s) ⇒ Arbitrary (StateMachine.State label s) where
  arbitrary =
    StateMachine.State
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CurrentQuestState where
  arbitrary = oneof [Forest <$> arbitrary]

instance Arbitrary Configuration where
  arbitrary = Configuration <$> elements [minBound..]

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> (choose (0, 3600) <&> secondsToDiffTime)

instance Arbitrary StdGen where
  arbitrary = do
    x ∷ Positive Int ← arbitrary
    y ∷ Positive Int ← arbitrary
    pure $ read [i|#{getPositive x} #{getPositive y}|]

instance Eq StdGen where (==) x y = show x == show y

deriving instance Eq Account

instance Arbitrary SuccessOrFailureResult where
  arbitrary = elements [SuccessResult, FailureResult]

instance Arbitrary Deed where
  arbitrary = Deed <$> arbitrary <*> (arbitrary <&> pack) <*> arbitrary

instance Arbitrary Age where
  arbitrary = elements [Fantasy, Space]

instance Arbitrary Account where
  arbitrary =
    Account
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> ((setToList >>> setFromList) <$> (arbitrary ∷ Gen (Set Text)))
      <*> ((mapToList >>> mapFromList) <$> (arbitrary ∷ Gen (Map (Gender, Age) [Text])))

stackString ∷ HasCallStack ⇒ String
stackString = case reverse callStack of
  [] → "No stack."
  (function_name, _):rest →
    intercalate "\n" $
    "Stack:"
    :
    (
      mapAccumL
        (\function_name (next_function_name, SrcLoc{..}) →
          (next_function_name, [i|    #{srcLocFile}:#{srcLocEndLine}:#{function_name}|])
        )
        function_name
        rest
      &
      (snd >>> reverse)
    )

assertBool ∷ MonadIO m ⇒ String → Bool → m ()
assertBool message = HUnit.assertBool message >>> liftIO

assertFailure ∷ (HasCallStack, MonadIO m) ⇒ String → m α
assertFailure message = liftIO $
  HUnit.assertFailure (message ⊕ "\n\n" ⊕ stackString)

(@?=) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@?=) x y = liftIO $ (HUnit.@?=) x y

(@=?) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@=?) x y = liftIO $ (HUnit.@=?) x y

testCase ∷ String → IO () → TestTree
testCase name = HUnit.testCase name

type LazyByteString = LazyBS.ByteString
type Tags = [Tag Lazy.Text]

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary `suchThat` (> 0)

instance Arbitrary TimeOfDay where
  arbitrary = dayFractionToTimeOfDay <$> (toRational <$> choose (0 ∷ Float, 1) `suchThat` (< 1))

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

withApplication' action =
  (makeAppRunningInTestMode
    <$> newTVarIO mempty
    <*> newEmptyMVar
  )
  >>=
  flip withApplication action

withTestApp ∷ (Int → IO ()) → IO ()
withTestApp action =
  (makeAppRunningInTestMode
    <$> newTVarIO mempty
    <*> newEmptyMVar
  )
  >>=
  flip withApplication action

serverTestCase ∷ String → (Int → IO ()) → TestTree
serverTestCase test_name = withTestApp >>> testCase test_name

apiTestCase ∷ String → (SessionIO ()) → TestTree
apiTestCase test_name action =
  (
    createAccount "bitslayer" "password" Testing "localhost"
    >=>
    (fromMaybe (error "Unable to create account.") >>> runSessionT action)
  )
  |> serverTestCase test_name

test_account_id ∷ Text
test_account_id = "test"

test_account_id_1 ∷ Text
test_account_id_1 = "test-1"

test_account_id_2 ∷ Text
test_account_id_2 = "test-2"

test_group_id ∷ UUID
test_group_id = read "f5ccdfde-1776-483c-9140-385e9e75e31d"

test_group, test_group_2 ∷ Group
test_group = "group"
test_group_2 = "grouper"

test_habit = Habit "name" (Tagged (Success Low) (Failure Medium)) Indefinite [] Nothing
test_habit_1 = Habit "name1" (Tagged (Success VeryLow) (Failure Medium)) Indefinite [] Nothing
test_habit_2 = Habit "test" (Tagged (Success Medium) (Failure VeryHigh)) Indefinite [] Nothing
test_habit_once = Habit "once" (Tagged (Success Medium) (Failure Medium)) (Once $ Just $ day 0) [] Nothing
test_habit_daily = def & name_ .~ "daily" & frequency_ .~ (Repeated (KeepNumberOfDays 2) (dayHour 2 3) (Daily 2))
test_habit_weekly = def & name_ .~ "daily" & frequency_ .~ (Repeated (KeepDaysInPast 4) (dayHour 3 2) (Weekly 1 (def & tuesday_ .~ True & thursday_ .~ True)))
test_habit_with_last_marked = def & name_ .~ "test" & maybe_last_marked_ .~ (Just $ dayHour 1 2)
test_habit_group = Habit "group" (Tagged (Success High) (Failure Low)) Indefinite [test_group_id] Nothing

test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9"
test_habit_id_once = read "7dbafaf9-560a-4ac4-b6bb-b64c647e387d"
test_habit_id_with_last_marked = read "7ada06ff-ccf5-4c68-83df-e54999cc42b3"
test_habit_id_group = read "74d6b013-df62-4989-8ce8-b0e0af3e29d3"

createGroup group_id group = putGroup group_id group >>= (@?= ResourceCreated)
replaceGroup group_id group = putGroup group_id group >>= (@?= ResourceReplaced)

createHabit habit_id habit = putHabit habit_id habit >>= (@?= ResourceCreated)
replaceHabit habit_id habit = putHabit habit_id habit >>= (@?= ResourceReplaced)

webTestCase ∷ String → ReaderT Int (StateT CookieJar IO) () → TestTree
webTestCase test_name runTest =
  serverTestCase test_name $ \port → do
    runTest
      |> flip runReaderT port
      |> void
      |> flip runStateT mempty
      |> void

requestDocument ∷
  ByteString →
  (Request → Request) →
  ReaderT Int (StateT CookieJar IO) (Response LazyByteString, Tags)
requestDocument path customizeRequest = do
  port ← ask
  old_cookie_jar ← get
  current_time ← liftIO getCurrentTime
  let request_without_cookies =
        defaultRequest
        |> setRequestSecure False
        |> setRequestHost "localhost"
        |> setRequestPort port
        |> setRequestPath path
        |> (\x → x {redirectCount = 0})
        |> customizeRequest
      (cookie_header, new_cookie_jar) =
        computeCookieString request old_cookie_jar current_time True
      request = addRequestHeader "Cookie" cookie_header request_without_cookies
  response ← liftIO $ httpLBS request
  let (updated_cookie_jar, response_without_cookie) =
        updateCookieJar response request current_time new_cookie_jar
  put updated_cookie_jar
  pure (response_without_cookie, response |> responseBody |> decodeUtf8 |> parseTags)

assertRedirectsTo ∷ MonadIO m ⇒ Response α → ByteString → m ()
assertRedirectsTo response expected_location = liftIO $
  getResponseHeader "Location" response @?= [expected_location]

assertPageTitleEquals ∷ MonadIO m ⇒ Tags → Lazy.Text → m ()
assertPageTitleEquals tags expected_page_title =
  (flip scrape tags $ text $ ("head" ∷ Selector) // "title") @?= Just expected_page_title

assertTextIs :: MonadIO m ⇒ Tags → String → Lazy.Text → m ()
assertTextIs tags element_id expected_text =
  (flip scrape tags $ text $ AnyTag @: ["id" @= element_id]) @?= Just expected_text

createTestAccount ∷
  ByteString →
  ByteString →
  ReaderT Int (StateT CookieJar IO) (Response LazyByteString, Tags)
createTestAccount username password =
  requestDocument "/create" $
    setRequestBodyURLEncoded
      [ ("username",username)
      , ("password1",password)
      , ("password2",password)
      ]

loginTestAccount ∷
  ByteString →
  ByteString →
  ReaderT Int (StateT CookieJar IO) (Response LazyByteString, Tags)
loginTestAccount username password =
  requestDocument "/login" $
    setRequestBodyURLEncoded
      [ ("username",username)
      , ("password",password)
      ]

createGroupViaWeb ∷ Show α => α → Group → ReaderT Int (StateT CookieJar IO) ()
createGroupViaWeb group_id group = void $
  ( requestDocument ("/groups/" ⊕ BS8.pack (show group_id))
    $
    setRequestBodyURLEncoded [("name",encodeUtf8 group)]
  )
  >>=
  \(_, tags) → liftIO $ (scrape (innerHTMLs $ "ul" @: ["class" @= "error_message"]) tags) @?= Just []

createHabitViaWeb ∷ Show α => α → Habit → ReaderT Int (StateT CookieJar IO) ()
createHabitViaWeb habit_id habit = void $
  ( requestDocument ("/habits/" ⊕ BS8.pack (show habit_id))
    $
    setRequestBodyURLEncoded (habit |> habitToInputHabit |> inputHabitToRequest)
  )
  >>=
  \(_, tags) → liftIO $ (scrape (innerHTMLs $ "ul" @: ["class" @= "error_message"]) tags) @?= Just []

extractInputText ∷ (HasCallStack, MonadIO m) ⇒ String → Tags → m Lazy.Text
extractInputText name tags =
  maybe
    (assertFailure $ name ⊕ " not found")
    pure
    (flip scrape tags $ attr "value" $ "input" @: ["name" @= name])

parseValueOrFail ∷ (HasCallStack, MonadIO m, Parsable α) ⇒ String → Lazy.Text → m α
parseValueOrFail name value =
  value
    |> parseParam
    |> either
        (\message → assertFailure [i|Error parsing #{name} (value = "#{value}"): #{message}|])
        pure

extractInputTextParam ∷ (HasCallStack, MonadIO m, Parsable α) ⇒ String → Tags → m α
extractInputTextParam name =
  extractInputText name
  >=>
  parseValueOrFail name

extractChoiceParam ∷ (HasCallStack, MonadIO m, Parsable α) ⇒ String → Scraper Lazy.Text Lazy.Text → Tags → m α
extractChoiceParam name scraper =
  scrape scraper
  >>>
  maybe
    (assertFailure [i|Value for "#{name}" was not found|])
    (parseValueOrFail name)

extractInputSelectParam ∷ (HasCallStack, MonadIO m, Parsable α) ⇒ String → Tags → m α
extractInputSelectParam name = extractChoiceParam name $
  attr "value" $ "select" @: ["name" @= name] // "option" @: ["selected" @= "selected"]

extractInputRadioParam ∷ (HasCallStack, MonadIO m, Parsable α) ⇒ String → Tags → m α
extractInputRadioParam name = extractChoiceParam name $
  attr "value" $ "input" @: ["type" @= "radio", "name" @= name, "checked" @= "checked"]

extractInputCheckbox ∷ HasCallStack ⇒ String → Tags → Bool
extractInputCheckbox name =
  scrape (attr "value" $ "input" @: ["type" @= "checkbox", "name" @= name, "checked" @= "checked"])
  >>>
  isJust

extractGroup ∷ (HasCallStack, MonadIO m) ⇒ Tags → m Group
extractGroup tags = extractInputText "name" tags <&> Lazy.toStrict

extractHabit ∷ (HasCallStack, MonadIO m) ⇒ Tags → m Habit
extractHabit tags =
  Habit
    <$> (extractInputText "name" tags <&> Lazy.toStrict)
    <*> (Tagged
          <$> (Success <$> extractInputSelectParam "difficulty" tags)
          <*> (Failure <$> extractInputSelectParam "importance" tags)
        )
    <*> (extractInputRadioParam "frequency" tags
         >>=
         \case
           InputIndefinite → pure Indefinite
           InputOnce → Once <$>
             if extractInputCheckbox "once_has_deadline" tags
               then extractInputTextParam "next_deadline" tags <&> Just
               else pure Nothing
           InputRepeated →
             Repeated
               <$> liftA2 ($)
                     (
                       extractInputRadioParam "days_to_keep_mode" tags
                       <&>
                       \case
                         InputKeepDaysInPast → KeepDaysInPast
                         InputKeepNumberOfDays → KeepNumberOfDays
                     )
                     (extractInputTextParam "days_to_keep" tags)
                <*> extractInputTextParam "next_deadline" tags
                <*> (
                      extractInputRadioParam "repeated" tags
                      >>=
                      \case
                        InputDaily → Daily <$> extractInputTextParam "daily_period" tags
                        InputWeekly →
                          Weekly
                            <$> extractInputTextParam "daily_period" tags
                            <*> (pure $ foldl'
                                  (\days_to_repeat Weekday{..} →
                                    if extractInputCheckbox (unpack weekday_name) tags
                                      then days_to_repeat & weekday_lens_ #~ True
                                      else days_to_repeat
                                  )
                                  def
                                  weekdays
                                )
                    )
        )
    <*> pure []
    <*> (tags
          |> scrape (attr "value" $ "input" @: ["name" @= "maybe_last_marked"])
          |> maybe
              (assertFailure "Expected presence of maybe_last_marked control")
              (\case
                "" → pure Nothing
                other → parseValueOrFail "maybe_last_marked" other <&> Just
              )
        )

testStory ∷ String → Substitutions → Story → TestTree
testStory name substitutions story = testCase name $ (extractPlaceholders story `difference` keysSet substitutions) @?= []

testStories ∷ String → Substitutions → [Story] → TestTree
testStories name substitutions stories =
  testGroup name
    [ testStory (show i) substitutions story
    | i ← [1 ∷ Int ..]
    | story ← stories
    ]

testStoryOutcomes ∷ String → Substitutions → StoryOutcomes → TestTree
testStoryOutcomes name substitutions outcomes =
  testGroup name $
    mapMaybe
      (\(outcome_name, outcome_lens_) →
        let outcome_story = outcomes ^# outcome_lens_
        in if onull outcome_story
          then Nothing
          else Just $ testStory (name ⊕ "." ⊕ outcome_name) substitutions outcome_story
      )
      story_outcome_singleton_labels
    ⊕
    (concat
     $
     mapMaybe
      (\(outcome_name, outcome_lens_) →
        let outcome_stories = outcomes ^# outcome_lens_
        in if onull outcome_stories
          then Nothing
          else Just
            [ testStory [i|#{name}.#{outcome_name}[#{n}]|] substitutions outcome_story
            | outcome_story ← outcome_stories
            | n ← [1..]
            ]
      )
      story_outcome_multiple_labels
    )

testTransitionsAreValid ∷ (Eq α, Show α) ⇒ String → [(α, Transition α)] → TestTree
testTransitionsAreValid name transitions = testGroup name
  [ testCase "All destination nodes are listed in the transitions" $
      let missing_destinations =
            transitions
              |> map (second $ \Transition{..} → filter (flip notMember transitions) next)
              |> filter (snd >>> onull >>> not)
      in unless (onull missing_destinations) $
          assertFailure $ "Missing destinations in transitions: " ⊕ show missing_destinations
  , testCase "All transitions have at least between story" $
      let transitions_with_no_between_stories =
            [ label
            | (label, Transition{..}) ← transitions
            , onull between_stories
            ]
      in unless (onull transitions_with_no_between_stories) $
          assertFailure $ "Missing between stories in transitions: " ⊕ show transitions_with_no_between_stories
  ]

dontTestGroup ∷ String → [TestTree] → TestTree
dontTestGroup name _ = testGroup name []

main ∷ HasCallStack ⇒ IO ()
main = defaultMain $ testGroup "All Tests"
  ------------------------------------------------------------------------------
  [ testGroup "JSON instances"
  ------------------------------------------------------------------------------
    [ QC.testProperty "Frequency" $ \(x ∷ Frequency) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
    , SC.testProperty "Scale" $ \(x ∷ Scale) → (encode >>> eitherDecode) x == Right x
    , QC.testProperty "Habit" $ \(x ∷ Habit) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
    , QC.testProperty "ItemsSequence" $ \(x ∷ ItemsSequence Int) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
    , QC.testProperty "Tagged" $ \(x ∷ Tagged Int) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
    , SC.testProperty "Gender" $ \(x ∷ Gender) → (encode >>> eitherDecode) x == Right x
    , SC.testProperty "Gendered" $ \(x ∷ Gendered) → (encode >>> eitherDecode) x == Right x
    , SC.testProperty "Forest.State" $ \(x ∷ Forest.State) → (encode >>> eitherDecode) x == Right x
    , SC.testProperty "CurrentQuestState" $ \(x ∷ CurrentQuestState) → (encode >>> eitherDecode) x == Right x
    , QC.testProperty "Configuration" $ \(x ∷ Configuration) → (encode >>> eitherDecode) x == Right x
    , QC.testProperty "Account" $ \(x ∷ Account) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Quests..."
  ------------------------------------------------------------------------------
    [ testGroup "HabitOfFate.Quests.Forest"
    ----------------------------------------------------------------------------
      [ testGroup "Stories" $ let subs = Forest.static_substitutions in
      ----------------------------------------------------------------------------
        [ testStory "intro_parent_story" subs Forest.intro_parent_story
        , testStory "intro_healer_story" subs Forest.intro_healer_story
        , testStory "looking_for_herb_story" subs Forest.looking_for_herb_story
        , testStory "returning_home_story" subs Forest.returning_home_story
        , testStories "wander" subs Forest.wander_stories
        , testStoryOutcomes "gingerbread_house" subs Forest.gingerbread_house
        , testStoryOutcomes "found_by_fairy" subs Forest.found_by_fairy
        , testStoryOutcomes "found_by_cat" subs Forest.found_by_cat
        , testStoryOutcomes "fairy_circle" subs Forest.fairy_circle
        , testStoryOutcomes "conclusion_parent" subs Forest.conclusion_parent
        , testStoryOutcomes "conclusion_healer" subs Forest.conclusion_healer
        ]
      , testTransitionsAreValid
          "Forest (parent)"
          (Forest.transitionsFor $ Forest.Internal
            Forest.Parent
            (Gendered "Billy" Male)
            [Forest.GingerbreadHouseEvent, Forest.FoundEvent, Forest.FairyCircleEvent]
          )
      , testTransitionsAreValid
          "Forest (healer)"
          (Forest.transitionsFor $ Forest.Internal
            Forest.Healer
            (Gendered "Elly" Female)
            [Forest.FoundEvent, Forest.FairyCircleEvent, Forest.GingerbreadHouseEvent]
          )
      ]
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Repeated"
  ------------------------------------------------------------------------------
    [ testGroup "nextDaily" $
    ----------------------------------------------------------------------------
      let testNextDailyCase period today deadline expected_result =
            testCase
              (printf
                "%i %s %s"
                period
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
              )
              (nextDaily period today deadline @?= expected_result)
      in
      [ testNextDailyCase 2 (dayHour 4 0) (dayHour 3 0) (dayHour 5 0)
      , testNextDailyCase 1 (dayHour 4 1) (dayHour 3 2) (dayHour 4 2)
      ]
    ----------------------------------------------------------------------------
    , testGroup "previousDailies" $
    ----------------------------------------------------------------------------
      let testPreviousDailiesCase takeDays period next_deadline expected_result =
            testCase
              (printf
                "%i %s"
                period
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" next_deadline)
              )
              (takeDays (previousDailies period next_deadline) @?= expected_result)
      in
      [ testPreviousDailiesCase (take 7) 1 (dayHour 20 2) [ dayHour d 2 | d ← [19, 18..13] ]
      , testPreviousDailiesCase (take 3) 2 (dayHour 20 2) [ dayHour d 2 | d ← [18, 16, 14] ]
      , testPreviousDailiesCase (take 3) 3 (dayHour 17 2) [ dayHour d 2 | d ← [14, 11,  8] ]
      ]
    ----------------------------------------------------------------------------
    , testGroup "nextWeeklyDay" $
    ----------------------------------------------------------------------------
      let testNextWeeklyDayCase days_to_repeat period today next_deadline =
            testCase
              (printf "%s %i %s"
                (repeatDays days_to_repeat)
                period
                (formatTime defaultTimeLocale "%y-%m-%d" today)
              )
              (nextWeeklyDay days_to_repeat period today @?= next_deadline)
      in
      [ QC.testProperty "next_deadline >= today" $ \days_to_repeat (Positive period) today →
          nextWeeklyDay days_to_repeat period today >= today
      , testNextWeeklyDayCase
          (def & monday_ .~ True & tuesday_ .~ True)
          1
          (fromWeekDate 1 1 1)
          (fromWeekDate 1 1 2)
      , testNextWeeklyDayCase
          (def & monday_ .~ True & wednesday_ .~ True)
          1
          (fromWeekDate 1 1 2)
          (fromWeekDate 1 1 3)
      , testNextWeeklyDayCase
          (def & monday_ .~ True)
          2
          (fromWeekDate 1 1 1)
          (fromWeekDate 1 3 1)
      , testNextWeeklyDayCase
          (def & monday_ .~ True)
          3
          (fromWeekDate 1 1 1)
          (fromWeekDate 1 4 1)
      , testNextWeeklyDayCase
          (def & monday_ .~ True)
          2
          (fromWeekDate 1 1 2)
          (fromWeekDate 1 3 1)
      , testNextWeeklyDayCase
          (def & wednesday_ .~ True)
          2
          (fromWeekDate 1 1 3)
          (fromWeekDate 1 3 3)
      , testNextWeeklyDayCase
          (def & tuesday_ .~ True & wednesday_ .~ True)
          2
          (fromWeekDate 1 1 4)
          (fromWeekDate 1 3 2)
      , testNextWeeklyDayCase
          (def & tuesday_ .~ True & friday_ .~ True)
          2
          (fromWeekDate 1 1 2)
          (fromWeekDate 1 1 5)
      , testNextWeeklyDayCase
          (def & monday_ .~ True & wednesday_ .~ True)
          2
          (fromWeekDate 1 5 5)
          (fromWeekDate 1 7 1)
      ]
    ----------------------------------------------------------------------------
    , testGroup "nextWeeklies" $
    ----------------------------------------------------------------------------
      let testNextWeeklyCase days_to_repeat period today deadline result =
            testCase
              (printf "%s %i %s %s"
                (repeatDays days_to_repeat)
                period
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
              )
              (nextWeekly period days_to_repeat today deadline @?= result)
      in
      [ testNextWeeklyCase
          (def & monday_ .~ True & wednesday_ .~ True)
          1
          (LocalTime (fromWeekDate 1 1 2) midnight)
          (LocalTime (fromWeekDate 1 1 1) midnight)
          (LocalTime (fromWeekDate 1 1 3) midnight)
      , testNextWeeklyCase
          (def & monday_ .~ True & wednesday_ .~ True)
          2
          (LocalTime (fromWeekDate 1 5 5) midnight)
          (LocalTime (fromWeekDate 1 1 1) midnight)
          (LocalTime (fromWeekDate 1 7 1) midnight)
      , testNextWeeklyCase
          (def & monday_ .~ True & wednesday_ .~ True)
          1
          (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
          (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))
          (LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0))
      , testNextWeeklyCase
          (def & monday_ .~ True & wednesday_ .~ True)
          1
          (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))
          (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
          (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
      ]
    ----------------------------------------------------------------------------
    , testGroup "previousWeekliesBeforePresent" $
    ----------------------------------------------------------------------------
      let testPreviousWeekliesCase takeDays days_to_repeat period next_deadline previous_deadlines =
            testCase
              (printf "%s %i %s"
                (repeatDays days_to_repeat)
                period
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" next_deadline)
              )
              (
                takeDays (previousWeeklies period days_to_repeat next_deadline)
                  @?= previous_deadlines
              )
      in
      [ testPreviousWeekliesCase
          (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
          (def & monday_ .~ True & tuesday_ .~ True)
          1
          (LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0))
          [ LocalTime (fromWeekDate 1 1 2) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
          ]
      , testPreviousWeekliesCase
          (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
          (def & monday_ .~ True)
          1
          (LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0))
          [ LocalTime (fromWeekDate 1 2 1) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
          ]
      , testPreviousWeekliesCase
          (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
          (def & monday_ .~ True & thursday_ .~ True)
          2
          (LocalTime (fromWeekDate 1 5 4) (TimeOfDay 1 0 0))
          [ LocalTime (fromWeekDate 1 5 1) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 3 4) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 1 4) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
          ]
      , testPreviousWeekliesCase
          (take 5)
          (def & monday_ .~ True & wednesday_ .~ True)
          2
          (LocalTime (fromWeekDate 1 5 5) (TimeOfDay 1 0 0))
          [ LocalTime (fromWeekDate 1 5 3) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 5 1) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 3 3) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0)
          , LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0)
          ]
      ]
    ----------------------------------------------------------------------------
    , testGroup "takeDays" $
    ----------------------------------------------------------------------------
      let testTakePreviousDeadlinesCase days_to_keep today deadline previous_deadlines expected_result =
            testCase
              (printf
                "(%s) %s %s"
                (show days_to_keep)
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
                (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
              )
              (takePreviousDeadlines days_to_keep today deadline previous_deadlines @?= expected_result)
      in
      [ testTakePreviousDeadlinesCase (KeepNumberOfDays 1) (day 3) (day 0) [day 2, day 1] [day 2]
      , testTakePreviousDeadlinesCase (KeepNumberOfDays 3) (day 3) (day 0) [day 2, day 1] [day 2, day 1]
      , testTakePreviousDeadlinesCase (KeepDaysInPast 1) (day 3) (day 0) [day 2, day 1] [day 2]
      , testTakePreviousDeadlinesCase (KeepDaysInPast 3) (day 3) (day 0) [day 2, day 1] [day 2, day 1]
      ]
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Server"
  ------------------------------------------------------------------------------
    [ testGroup "JSON API" $
    ----------------------------------------------------------------------------
        let apiTestCase ∷ String → (SessionIO ()) → TestTree
            apiTestCase test_name action =
              (
                createAccount "bitslayer" "password" Testing "localhost"
                >=>
                (fromMaybe (error "Unable to create account.") >>> runSessionT action)
              )
              |> serverTestCase test_name
        in
        ------------------------------------------------------------------------
        [ testGroup "Missing username/password" $
        ------------------------------------------------------------------------
            let testMissing test_name path =
                  serverTestCase test_name $ \port → do
                    response ←
                      defaultRequest
                        |> setRequestMethod "POST"
                        |> setRequestHost "localhost"
                        |> setRequestPort port
                        |> setRequestPath ("/api/" ⊕ path)
                        |> httpNoBody
                    400 @=? responseStatusCode response
            in
            [ testGroup "Create account"
                [ testMissing "Missing username" "create?password=foobar"
                , testMissing "Missing password" "create?username=foobar"
                ]
            , testGroup "Log in"
                [ testMissing "Missing username" "login?password=foobar"
                , testMissing "Missing password" "login?username=foobar"
                ]
            ]
        ------------------------------------------------------------------------
        , testGroup "Empty username/password" $
        ------------------------------------------------------------------------
            let testEmpty test_name path =
                  serverTestCase test_name $ \port → do
                    response ←
                      defaultRequest
                        |> setRequestMethod "POST"
                        |> setRequestHost "localhost"
                        |> setRequestPort port
                        |> setRequestPath ("/api/" ⊕ path)
                        |> httpNoBody
                    400 @=? responseStatusCode response
            in
            [ testGroup "Create account"
                [ testEmpty "Empty username" "create?username="
                , testEmpty "Empty password" "create?password="
                ]
            ]
        ------------------------------------------------------------------------
        , apiTestCase "Logging out makes habits forbidden to access." $ do
        ------------------------------------------------------------------------
            logout
            try getHabits >>= \case
              Left (UnexpectedStatus _ 403) → pure ()
              Left e → throwM e
              _ → assertFailure "No exception raised."
        ------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ------------------------------------------------------------------------
            getHabits >>= (view items_count_ >>> (@?= 0))
        ------------------------------------------------------------------------
        , apiTestCase "Fetching a habit when none exist returns Nothing" $
        ------------------------------------------------------------------------
            getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
            >>=
            (@?= Nothing)
        ------------------------------------------------------------------------
        , testGroup "putGroup"
        ------------------------------------------------------------------------
            [ apiTestCase "Putting a group and then fetching it returns the group" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                getGroup test_group_id >>= (@?= Just test_group)
            --------------------------------------------------------------------
            , apiTestCase "Putting a group causes fetching all groups to return a singleton map" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                getGroups >>= (@?= [(test_group_id, test_group)])
            --------------------------------------------------------------------
            , apiTestCase "Putting a group, replacing it, and then fetching all groups returns the replaced group" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                replaceGroup test_group_id test_group_2
                getGroups >>= (@?= [(test_group_id, test_group_2)])
            ]
        ------------------------------------------------------------------------
        , testGroup "putHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabit test_habit_id >>= (@?= Just test_habit)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabits >>= (@?= [(test_habit_id, test_habit)])
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                replaceHabit test_habit_id test_habit_2
                getHabits >>= (@?= [(test_habit_id, test_habit_2)])
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit with missing groups creates habit with no groups" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id_group test_habit_group
                getHabit test_habit_id_group >>= (@?= Just (test_habit_group & group_membership_ .~ []))
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit with existing group returns expected habit" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                createHabit test_habit_id_group test_habit_group
                getHabit test_habit_id_group >>= (@?= Just test_habit_group)
            ]
        ------------------------------------------------------------------------
        , testGroup "deleteGroup"
        ------------------------------------------------------------------------
            [ apiTestCase "Deleting a non-existing group returns NoResourceToDelete" $ do
            --------------------------------------------------------------------
                deleteGroup test_group_id >>= (@?= NoResourceToDelete)
            --------------------------------------------------------------------
            , apiTestCase "Putting a group then deleting it returns ResourceDeleted and causes fetching all groups to return an empty map" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                deleteGroup test_group_id >>= (@?= ResourceDeleted)
                getGroups >>= (view items_count_ >>> (@?= 0))
            --------------------------------------------------------------------
            , apiTestCase "Deleting a group removes it from all habits" $ do
            --------------------------------------------------------------------
                createGroup test_group_id test_group
                createHabit test_habit_id_group test_habit_group
                getHabit test_habit_id_group >>= (@?= Just test_habit_group)
                deleteGroup test_group_id >>= (@?= ResourceDeleted)
                getHabit test_habit_id_group >>= (@?= Just (test_habit_group & group_membership_ .~ []))
            ]
        ------------------------------------------------------------------------
        , testGroup "deleteHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Deleting a non-existing habit returns NoResourceToDelete" $ do
            --------------------------------------------------------------------
                deleteHabit test_habit_id >>= (@?= NoResourceToDelete)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit then deleting it returns ResourceDeleted and causes fetching all habits to return an empty map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                deleteHabit test_habit_id >>= (@?= ResourceDeleted)
                getHabits >>= (view items_count_ >>> (@?= 0))
            ]
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ----------------------------------------------------------------------------
            getHabits >>= (view items_count_ >>> (@?= 0))
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching a habit when none exist returns Nothing" $
        ----------------------------------------------------------------------------
            getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506") >>= (@?= Nothing)
        ----------------------------------------------------------------------------
        , testGroup "putHabit"
        ----------------------------------------------------------------------------
            [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabit test_habit_id >>= (@?= Just test_habit)
            ------------------------------------------------------------------------
            , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabits >>= (@?= [(test_habit_id, test_habit)])
            ------------------------------------------------------------------------
            , testGroup "Putting two habits causes them to be returned in order of creation" $
            ------------------------------------------------------------------------
              [ apiTestCase "Test habit 1 followed by test habit 2" $ do
              ------------------------------------------------------------------------
                  createHabit test_habit_id test_habit
                  createHabit test_habit_id_2 test_habit_2
                  getHabits >>= (@?= [(test_habit_id, test_habit), (test_habit_id_2, test_habit_2)])
              ------------------------------------------------------------------------
              , apiTestCase "Test habit 2 followed by test habit 1" $ do
              ------------------------------------------------------------------------
                  createHabit test_habit_id_2 test_habit_2
                  createHabit test_habit_id test_habit
                  getHabits >>= (@?= [(test_habit_id_2, test_habit_2), (test_habit_id, test_habit)])
              ]
            ------------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                markHabits
                  [ (test_habit_id, def & failure_ .~ 1)
                  , (test_habit_id_2, def & success_ .~ 1)
                  , (test_habit_id_2, def)
                  ]
                getMarks >>=
                  (@?=
                    Tagged
                      (Success [test_habit_2 ^. difficulty_])
                      (Failure [test_habit ^. importance_])
                  )
            ------------------------------------------------------------------------
            , testCase "Putting a habit causes the accounts to be written" $ do
            ------------------------------------------------------------------------
                accounts_changed_signal ← newEmptyMVar
                (makeAppRunningInTestMode
                  <$> newTVarIO mempty
                  <*> pure accounts_changed_signal
                 ) >>=
                  flip withApplication (
                    \port → do
                      session_info ← fromJust <$> createAccount "bitslayer" "password" Testing "localhost" port
                      flip runSessionT session_info $ createHabit test_habit_id test_habit
                  )
                tryTakeMVar accounts_changed_signal >>= (@?= Just ())
            ]
        ----------------------------------------------------------------------------
        , testGroup "markHabits"
        ----------------------------------------------------------------------------
            [ apiTestCase "Marking a habit gets the right marks" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                markHabits
                  [ (test_habit_id, def & success_ .~ 1)
                  , (test_habit_id_2, def & failure_ .~ 1)
                  , (test_habit_id, def)
                  ]
                getMarks >>=
                  (@?=
                    Tagged
                      (Success [test_habit ^. difficulty_])
                      (Failure [test_habit_2 ^. importance_])
                  )
            ]
        ----------------------------------------------------------------------------
        , testGroup "configuration"
        ----------------------------------------------------------------------------
            [ apiTestCase "Put then get" $ do
            ------------------------------------------------------------------------
                Configuration old_tzlabel ← getConfiguration
                let new_tzlabel = case old_tzlabel of
                      Pacific__Pago_Pago → Pacific__Palau
                      _ → Pacific__Pago_Pago
                    new_configuration = Configuration new_tzlabel
                putConfiguration $ new_configuration
                getConfiguration >>= (@?= new_configuration)
            ------------------------------------------------------------------------
            , apiTestCase "Put then reset" $ do
            ------------------------------------------------------------------------
                old_configuration@(Configuration old_tzlabel) ← getConfiguration
                let new_tzlabel = case old_tzlabel of
                      Pacific__Pago_Pago → Pacific__Palau
                      _ → Pacific__Pago_Pago
                    new_configuration = Configuration new_tzlabel
                putConfiguration $ new_configuration
                resetConfiguration
                getConfiguration >>= (@?= old_configuration)
            ]
        ----------------------------------------------------------------------------
        , testGroup "deadlines"
        ----------------------------------------------------------------------------
            [ apiTestCase "Two habits with neither having any deadlines" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                getDeadlines >>= (@?= [])
            ------------------------------------------------------------------------
            , apiTestCase "Two habits with one having a Once deadline" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id (test_habit & frequency_ .~ (Once (Just (dayHour 0 0))))
                createHabit test_habit_id_2 test_habit_2
                getDeadlines >>= (@?= [(test_habit_id, [dayHour 0 0])])
                markHabits [(test_habit_id, def), (test_habit_id_2, def & success_ .~ 1)]
                  >>= (@?= (def & success_ .~ [test_habit_2 ^. scales_ . success_]))
                getDeadlines >>= (@?= [])
                getHabit test_habit_id >>= (@?= Nothing)
            ------------------------------------------------------------------------
            , apiTestCase "Two habits with both having Once deadlines" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id (test_habit & frequency_ .~ (Once (Just (dayHour 0 0))))
                let test_habit_2_once = test_habit_2 & frequency_ .~ (Once (Just (dayHour 1 1)))
                createHabit test_habit_id_2 test_habit_2_once
                markHabits [(test_habit_id, def & failure_ .~ 1)]
                  >>= (@?= (def & failure_ .~ [test_habit ^. scales_ . failure_]))
                getDeadlines >>= (@?= [(test_habit_id_2, [dayHour 1 1])])
                getHabit test_habit_id >>= (@?= Nothing)
                getHabit test_habit_id_2 >>= (@?= Just test_habit_2_once)
            ------------------------------------------------------------------------
            , apiTestCase "One habit repeated daily" $ do
            ------------------------------------------------------------------------
                putConfiguration $ Configuration Etc__UTC
                createHabit test_habit_id
                  (test_habit &
                    frequency_ .~ Repeated (KeepNumberOfDays 3) (dayHour 0 0) (Daily 1)
                  )
                LocalTime (ModifiedJulianDay d) _ ← liftIO getCurrentTime <&> utcToLocalTime utc
                getDeadlines >>= (@?= [(test_habit_id, [dayHour (d-i) 0 | i ← [0,1,2]])])
                markHabits [(test_habit_id, def & success_ .~ 1)]
                  >>= (@?= (def & success_ .~ [test_habit ^. scales_ . success_]))
                getDeadlines >>= (@?= [])
                new_test_habit ← getHabit test_habit_id >>= maybe (assertFailure "Habit not found.") pure
                new_test_habit @?= (
                  test_habit
                    & maybe_last_marked_ .~ new_test_habit ^. maybe_last_marked_
                    & frequency_ .~ Repeated (KeepNumberOfDays 3) (dayHour (fromInteger (d+1)) 0) (Daily 1)
                 )
                last_marked_time ←
                  maybe
                    (assertFailure "Habit was not marked.")
                    pure
                    (new_test_habit ^. maybe_last_marked_)
                current_time ← liftIO getCurrentTime <&> utcToLocalTime utc
                assertBool "Current time was before last marked time." (current_time >= last_marked_time)
            ]
        ]
    ----------------------------------------------------------------------------
    , testGroup "Web" $
    ----------------------------------------------------------------------------
        [ testGroup "Redirections" $
        ------------------------------------------------------------------------
            [ webTestCase "GET / redirects to /habits" $ do
                (response, _) ← requestDocument "/" $ setRequestMethod "GET"
                assertRedirectsTo response "/habits"
            , webTestCase "GET /habits redirects to /login" $ do
                (response, _) ← requestDocument "/habits" $ setRequestMethod "GET"
                assertRedirectsTo response "/login"
            ]
        ------------------------------------------------------------------------
        , testGroup "Login page" $
        ------------------------------------------------------------------------
            [ webTestCase "GET /login returns login page" $ do
                (_, tags) ← requestDocument "/login" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - Login"
            , webTestCase "POST /login for non-existent user returns login page withe error" $ do
                (_, tags) ← requestDocument "/login" $
                  setRequestBodyURLEncoded [("username","username"), ("password","password")]
                assertPageTitleEquals tags "Habit of Fate - Login"
                assertTextIs tags "error-message" "No account has that username."
            ]
        ------------------------------------------------------------------------
        , testGroup "Create page" $
        ------------------------------------------------------------------------
            [ webTestCase "GET /create returns account creation page" $ do
                (_, tags) ← requestDocument "/create" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - Account Creation"
            , webTestCase "POST /create with fields filled in redirects to /" $ do
                (response, _) ← createTestAccount "username" "password"
                assertRedirectsTo response "/"
            , webTestCase "Creating an account causes / to load the habits page" $ do
                _ ← createTestAccount "username" "password"
                (_, tags) ← requestDocument "/habits" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - List of Habits"
            , webTestCase "Creating an account then logging in redirects to /" $ do
                _ ← createTestAccount "username" "password"
                (response, _) ← loginTestAccount "username" "password"
                assertRedirectsTo response "/"
            , webTestCase "Creating an account makes /habits display the list of habits" $ do
                _ ← createTestAccount "username" "password"
                (_, tags) ← requestDocument "/habits" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - List of Habits"
            , webTestCase "Creating a conflicting account displays an error message" $ do
                _ ← createTestAccount "username" "password"
                (response, tags) ← createTestAccount "username" "password"
                getResponseStatusCode response @?= 409
                assertTextIs tags "error-message" "This account already exists."
            ]
        ------------------------------------------------------------------------
        , testGroup "Habits" $
        ------------------------------------------------------------------------
            [ webTestCase "Open the group edit page for an existing group." $ do
                _ ← createTestAccount "username" "password"
                createGroupViaWeb test_group_id test_group
                (response, tags) ←
                  requestDocument ([i|/groups/#{UUID.toText test_group_id}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractGroup tags >>= (@?= test_group)
            ]
        ------------------------------------------------------------------------
        , testGroup "Habits" $
        ------------------------------------------------------------------------
            [ webTestCase "Open the habit edit page for an existing habit." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_2 test_habit_2
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id_2}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_2)
            , webTestCase "Open the habit edit page for a once habit with deadline." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_once test_habit_once
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id_once}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_once)
            , webTestCase "Open the habit edit page for a daily repeated habit." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id test_habit_daily
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_daily)
            , webTestCase "Open the habit edit page for a weekly repeated habit." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id test_habit_weekly
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_weekly)
            , webTestCase "Open the habit edit page for a habit with a nontrivial last marked field." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_with_last_marked test_habit_with_last_marked
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id_with_last_marked}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_with_last_marked)
            , webTestCase "Create a habit with a missing group via. web then open." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_group test_habit_group
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id_group}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= (test_habit_group & (group_membership_ .~ [])))
            ]
        ]
        ------------------------------------------------------------------------
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Substitution"
  ------------------------------------------------------------------------------
    [ testCase "literal, one char" $ do
        story ← parseSubstitutions "l"
        extractPlaceholders story @?= []
        substitute mempty story >>= (@?= "l")
    , testCase "literal, whole string" $
        parseSubstitutions "xyz"
        >>=
        substitute mempty
        >>=
        (@?= "xyz")
    , testCase "substitution, name" $ do
        story ← parseSubstitutions "|name"
        extractPlaceholders story @?= ["name"]
        substitute (singletonMap "name" (Gendered "value" Male)) story >>= (@?= "value")
    , testCase "substitution, subject" $
        parseSubstitutions "he/she|name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Female))
        >>=
        (@?= "she")
    , testCase "substitution, possessive" $
        parseSubstitutions "his/her|name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Male))
        >>=
        (@?= "his")
    , testCase "substitution, proper possessive" $
        parseSubstitutions "his/hers|name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Female))
        >>=
        (@?= "hers")
    , testCase "substitution, with a article" $
        parseSubstitutions "an |name"
        >>=
        substitute (singletonMap "name" (Gendered "cat" Female))
        >>=
        (@?= "a cat")
    , testCase "substitution, with an article" $
        parseSubstitutions "a |name"
        >>=
        substitute (singletonMap "name" (Gendered "apple" Female))
        >>=
        (@?= "an apple")
    , testCase "substitution, with capitalized article" $
        parseSubstitutions "An |name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Female))
        >>=
        (@?= "A value")
    , testCase "substitution, with article and a newline" $
        parseSubstitutions "an\n|name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Female))
        >>=
        (@?= "a value")
    , testCase "substitution, uppercase referrant" $
        parseSubstitutions "His/hers|name"
        >>=
        substitute (singletonMap "name" (Gendered "value" Male))
        >>= (@?= "His")
    , testCase "unrecognized key" $
        parseSubstitutions "His/hers|Bob"
        >>=
        (substitute mempty >>> try)
        >>= (@?= Left (NoSuchKeyException "Bob"))
    ]
    ----------------------------------------------------------------------------
  ]
