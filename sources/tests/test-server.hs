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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LazyBS
import Data.CallStack
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Text.Lazy as Lazy
import qualified Data.UUID as UUID
import Network.HTTP.Client hiding (httpNoBody)
import Network.HTTP.Conduit (Response(..), responseStatus)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (ok200)
import Network.Wai.Handler.Warp
import Test.Tasty (TestTree, testGroup)
import Data.Time.Zones.All
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag, parseTags)
import Web.Scotty (Parsable(..))

import HabitOfFate.API
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.InputHabit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Mark
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Server

import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions
import HabitOfFate.Testing.Data
import HabitOfFate.Testing.DayHour
import HabitOfFate.Testing.Instances ()
import HabitOfFate.Testing.Server

type LazyByteString = LazyBS.ByteString
type Tags = [Tag Lazy.Text]

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

main ∷ HasCallStack ⇒ IO ()
main = doMain
  ------------------------------------------------------------------------------
  [ testGroup "HabitOfFate.Server"
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
                void $ markHabits
                  [ (test_habit_id, [FailureResult])
                  , (test_habit_id_2, [SuccessResult])
                  , (test_habit_id_2, [])
                  ]
                getMarks >>= (@?=
                  [ Mark FailureResult $ test_habit ^. importance_
                  , Mark SuccessResult $ test_habit_2 ^. difficulty_
                  ])
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
                void $ markHabits
                  [ (test_habit_id, [SuccessResult])
                  , (test_habit_id_2, [FailureResult])
                  , (test_habit_id, [])
                  ]
                getMarks >>= (@?=
                  [ Mark SuccessResult $ test_habit ^. difficulty_
                  , Mark FailureResult $ test_habit_2 ^. importance_
                  ])
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
                markHabits [(test_habit_id, []), (test_habit_id_2, [SuccessResult])]
                  >>= (@?= [ Mark SuccessResult $ test_habit_2 ^. difficulty_ ])
                getMarks
                  >>= (@?= [ Mark SuccessResult $ test_habit_2 ^. difficulty_ ])
                getDeadlines >>= (@?= [(test_habit_id, [dayHour 0 0])])
                markHabits [(test_habit_id, [SuccessResult, FailureResult])]
                  >>= (@?=
                    [ Mark SuccessResult $ test_habit_2 ^. difficulty_
                    , Mark SuccessResult $ test_habit   ^. difficulty_
                    ])
                getDeadlines >>= (@?= [])
                getHabit test_habit_id >>= (@?= Nothing)
            ------------------------------------------------------------------------
            , apiTestCase "Three habits all having Once deadlines" $ do
            ------------------------------------------------------------------------
                let test_habit_1_once = test_habit_1 & frequency_ .~ (Once (Just (dayHour 0 0)))
                    test_habit_2_once = test_habit_2 & frequency_ .~ (Once (Just (dayHour 1 1)))
                    test_habit_3_once = test_habit_3 & frequency_ .~ (Once (Just (dayHour 2 2)))
                createHabit test_habit_id_1 test_habit_1_once
                createHabit test_habit_id_2 test_habit_2_once
                createHabit test_habit_id_3 test_habit_3_once
                getDeadlines >>= (@?=
                  [ (test_habit_id_1, [dayHour 0 0])
                  , (test_habit_id_2, [dayHour 1 1])
                  , (test_habit_id_3, [dayHour 2 2])
                  ])
                markHabits
                  [ (test_habit_id_1, [])
                  , (test_habit_id_2, [FailureResult])
                  , (test_habit_id_3, [SuccessResult, FailureResult])
                  ]
                  >>= (@?=
                  [ Mark FailureResult $ test_habit_2 ^. importance_
                  , Mark SuccessResult $ test_habit_3 ^. difficulty_
                  ])
                getDeadlines >>= (@?= [(test_habit_id_1, [dayHour 0 0])])
                getHabit test_habit_id_1 >>= (@?= Just test_habit_1_once)
                getHabit test_habit_id_2 >>= (@?= Nothing)
                getHabit test_habit_id_3 >>= (@?= Nothing)
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
                markHabits [(test_habit_id, [SuccessResult])]
                  >>= (@?= [ Mark SuccessResult $ test_habit ^. difficulty_ ])
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
    ----------------------------------------------------------------------------
  ]
