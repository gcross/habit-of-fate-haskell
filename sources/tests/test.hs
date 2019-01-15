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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements, text)

import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import Control.Monad.Catch
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LazyBS
import Data.ByteString.Strict.Lens (packedChars, unpackedChars)
import Data.Data.Lens (uniplate)
import Data.IORef
import Data.List (cycle, isPrefixOf, sort, zip3)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import qualified Data.Text as Text
import Data.Text (strip)
import Data.Text.Strict.Lens (utf8)
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID, fromText)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Network.HTTP.Client hiding (httpNoBody)
import Network.HTTP.Conduit (Response(..), responseStatus)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (ok200)
import Network.Wai.Handler.Warp
import System.IO hiding (utf8)
import Text.Printf
import Test.QuickCheck hiding (Failure, Success)
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit as HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)
import Data.Time.Zones
import Data.Time.Zones.All
import Text.HTML.DOM (sinkDoc)
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag, parseTags)
import Text.Parsec (many, runParser)
import Text.XML (documentRoot, parseText)
import Web.Scotty (parseParam)

import HabitOfFate.API
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Server
import HabitOfFate.Substitution

dayHour d h = LocalTime (ModifiedJulianDay d) (TimeOfDay h 0 0)
day = flip dayHour 0

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

assertBool ∷ MonadIO m ⇒ String → Bool → m ()
assertBool message = HUnit.assertBool message >>> liftIO

assertFailure ∷ MonadIO m ⇒ String → m α
assertFailure = HUnit.assertFailure >>> liftIO

(@?=) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@?=) x y = liftIO $ (HUnit.@?=) x y

(@=?) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@=?) x y = liftIO $ (HUnit.@=?) x y

testCase ∷ String → IO () → TestTree
testCase name = HUnit.testCase name

type LazyByteString = LazyBS.ByteString
type Tags = [Tag LazyByteString]

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

test_habit, test_habit_2 ∷ Habit
test_habit = Habit "name" (Tagged (Success Low) (Failure Medium)) Indefinite [] Nothing
test_habit_2 = Habit "test" (Tagged (Success Medium) (Failure VeryHigh)) Indefinite [] Nothing

test_habit_id, test_habit_id_2 ∷ UUID
test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9"

createHabit habit_id habit = putHabit habit_id habit >>= (@?= HabitCreated)
replaceHabit habit_id habit = putHabit habit_id habit >>= (@?= HabitReplaced)

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
  pure (response_without_cookie, parseTags $ responseBody response)

assertRedirectsTo ∷ MonadIO m ⇒ Response α → ByteString → m ()
assertRedirectsTo response expected_location = liftIO $
  getResponseHeader "Location" response @?= [expected_location]

assertPageTitleEquals ∷ MonadIO m ⇒ Tags → LazyByteString → m ()
assertPageTitleEquals tags expected_page_title =
  (flip scrape tags $ text $ ("head" ∷ Selector) // "title") @?= Just expected_page_title

assertTextIs :: MonadIO m ⇒ Tags → String → LazyByteString → m ()
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

createHabitViaWeb ∷ Show α => α → Habit → ReaderT Int (StateT CookieJar IO) ()
createHabitViaWeb habit_id habit = void $
  requestDocument ("/habits/" ⊕ BS8.pack (show habit_id))
  $
  setRequestBodyURLEncoded
    [ ("name", habit ^. name_ . re utf8)
    , ("importance", habit ^. importance_ . to (show >>> BS8.pack))
    , ("difficulty", habit ^. difficulty_ . to (show >>> BS8.pack))
    , ("frequency", habit ^. frequency_ . to (show >>> BS8.pack))
    ]

convertLazyBStoString ∷ LazyByteString → String
convertLazyBStoString = decodeUtf8 >>> Lazy.toStrict >>> unpack

assertFailureLazyBS ∷ MonadIO m ⇒ LazyByteString → m α
assertFailureLazyBS = convertLazyBStoString >>> assertFailure

extractTextInput ∷ MonadIO m ⇒ String → Tags → m Text
extractTextInput name tags =
  maybe
    (assertFailure $ name ⊕ " not found")
    (decodeUtf8 >>> Lazy.toStrict >>> pure)
    (flip scrape tags $ attr "value" $ "input" @: ["name" @= "name"])

extractSelect ∷ (MonadIO m, Read α) ⇒ String → Tags → m α
extractSelect name tags =
  maybe
    (assertFailure $ name ⊕ " not found")
    (
      convertLazyBStoString
      >>>
      (\text_value →
        either
          (printf "Error parsing %s (value = \"%s\"): %s" name text_value >>> assertFailure)
          pure
          (readEither text_value)
      )
    )
    (flip scrape tags $
      attr "value" $ "select" @: ["name" @= name] // "option" @: ["selected" @= "selected"]
    )

extractRadio ∷ (MonadIO m, Read α) ⇒ String → Tags → m α
extractRadio name tags =
  maybe
    (assertFailure $ name ⊕ " not found")
    (
      convertLazyBStoString
      >>>
      (\text_value →
        either
          (printf "Error parsing %s (value = \"%s\"): %s" name text_value >>> assertFailure)
          pure
          (readEither text_value)
      )
    )
    (flip scrape tags $
      attr "value" $ "input" @: ["type" @= "radio", "name" @= name, "checked" @= "checked"]
    )

extractHabit ∷ MonadIO m ⇒ Tags → m Habit
extractHabit tags =
  Habit
    <$> extractTextInput "name" tags
    <*> (Tagged
          <$> (Success <$> extractSelect "difficulty" tags)
          <*> (Failure <$> extractSelect "importance" tags)
        )
    <*> extractRadio "frequency" tags
    <*> pure []
    <*> pure Nothing

checkStory ∷ Substitutions → Story → IO ()
checkStory substitutions story = keysSet substitutions @?= extractPlaceholders story

dontTestGroup ∷ String → [TestTree] → TestTree
dontTestGroup name _ = testGroup name []

main = defaultMain $ testGroup "All Tests"
  ------------------------------------------------------------------------------
  [ testGroup "HabitOfFate.Quests..."
  ------------------------------------------------------------------------------
    [ testGroup "HabitOfFate.Quests.Forest"
    ----------------------------------------------------------------------------
      [ testGroup "Stories"
      ----------------------------------------------------------------------------
        [ testCase "intro_parent_story" $ checkStory Forest.static_substitutions Forest.intro_parent_story
        ]
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
      [ testProperty "next_deadline >= today" $ \days_to_repeat (Positive period) today →
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
            ]
        ------------------------------------------------------------------------
        , testGroup "deleteHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Deleting a non-existing habit returns NoHabitToDelete" $ do
            --------------------------------------------------------------------
                deleteHabit test_habit_id >>= (@?= NoHabitToDelete)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit then deleting it returns HabitDeleted and causes fetching all habits to return an empty map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                deleteHabit test_habit_id >>= (@?= HabitDeleted)
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
            [ webTestCase "Open the habit edit page for an existing habit." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_2 test_habit_2
                (response, tags) ←
                  requestDocument ([i|/habits/#{UUID.toText test_habit_id_2}|] |> pack |> encodeUtf8) $
                    setRequestMethod "GET"
                responseStatus response @?= ok200
                extractHabit tags >>= (@?= test_habit_2)
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
