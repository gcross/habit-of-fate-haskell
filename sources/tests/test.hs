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
{-# LANGUAGE OverloadedStrings #-}
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
import Data.List (cycle, isPrefixOf)
import Data.Time.Clock
import qualified Data.Text as Text
import Data.Text (strip)
import Data.Text.Strict.Lens (utf8)
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID, fromText)
import Network.HTTP.Client hiding (httpNoBody)
import Network.HTTP.Conduit (Response(..), responseStatus)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (found302, ok200)
import Network.Wai.Handler.Warp
import System.IO hiding (utf8)
import Text.Printf
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.HTML.DOM (sinkDoc)
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag, parseTags)
import Text.Parsec (many, runParser)
import Text.XML (documentRoot, parseText)
import Web.Scotty (parseParam)

import HabitOfFate.API
import HabitOfFate.Data.Credits
import HabitOfFate.Data.Habit
import HabitOfFate.Server
import HabitOfFate.Story.Parser.Quote
import HabitOfFate.Substitution

type LazyByteString = LazyBS.ByteString
type Tags = [Tag LazyByteString]

instance Arbitrary SubstitutionData where
  arbitrary =
    SubstitutionData
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (pack <$> (listOf $ choose ('a', 'z')))

instance Arbitrary Kind where
  arbitrary = oneof
    [ pure Name
    , Referrent <$> arbitrary
    ]

instance Arbitrary Referrent where
  arbitrary = elements [minBound..maxBound]

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
test_habit = Habit "name" (Difficulty Low) (Importance Medium) Indefinite
test_habit_2 = Habit "test" (Difficulty Medium) (Importance VeryHigh) Indefinite

test_habit_id, test_habit_id_2 ∷ UUID
test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9"

createHabit habit_id habit = putHabit habit_id habit >>= ((@?= HabitCreated) >>> liftIO)
replaceHabit habit_id habit = putHabit habit_id habit >>= ((@?= HabitReplaced) >>> liftIO)

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
assertPageTitleEquals tags expected_page_title = liftIO $
  (flip scrape tags $ text $ ("head" ∷ Selector) // "title") @?= Just expected_page_title

assertTextIs :: MonadIO m ⇒ Tags → String → LazyByteString → m ()
assertTextIs tags element_id expected_text = liftIO $
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
  requestDocument ("/edit/" ⊕ BS8.pack (show habit_id))
  $
  setRequestBodyURLEncoded
    [ ("name", habit ^. name_ . re utf8)
    , ("importance", habit ^. importance_ . to (show >>> BS8.pack))
    , ("difficulty", habit ^. difficulty_ . to (show >>> BS8.pack))
    , ("frequency", habit ^. frequency_ . to (show >>> BS8.pack))
    ]

convertLazyBStoString ∷ LazyByteString → String
convertLazyBStoString = decodeUtf8 >>> Lazy.toStrict >>> unpack

assertFailureLazyBS ∷ LazyByteString → IO α
assertFailureLazyBS = convertLazyBStoString >>> assertFailure

extractTextInput ∷ String → Tags → IO Text
extractTextInput name tags =
  maybe
    (assertFailure $ name ⊕ " not found")
    (decodeUtf8 >>> Lazy.toStrict >>> pure)
    (flip scrape tags $ attr "value" $ "input" @: ["name" @= "name"])

extractSelect ∷ Read α ⇒ String → Tags → IO α
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

extractRadio ∷ Read α ⇒ String → Tags → IO α
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

extractHabit ∷ Tags → IO Habit
extractHabit tags =
  Habit
    <$> extractTextInput "name" tags
    <*> (Difficulty <$> extractSelect "difficulty" tags)
    <*> (Importance <$> extractSelect "importance" tags)
    <*> extractRadio "frequency" tags

main = defaultMain $ testGroup "All Tests"
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
              _ → liftIO $ assertFailure "No exception raised."
        ------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ------------------------------------------------------------------------
            getHabits >>= (view habit_count_ >>> (@?= 0) >>> liftIO)
        ------------------------------------------------------------------------
        , apiTestCase "Fetching a habit when none exist returns Nothing" $
        ------------------------------------------------------------------------
            getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
            >>=
            ((@?= Nothing) >>> liftIO)
        ------------------------------------------------------------------------
        , testGroup "putHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabit test_habit_id >>= ((@?= Just test_habit) >>> liftIO)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabits
                  >>=
                  (
                    (@?= Habits (singletonMap test_habit_id test_habit) (singleton test_habit_id))
                    >>>
                    liftIO
                  )
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                replaceHabit test_habit_id test_habit_2
                getHabits
                  >>=
                  (
                    (@?= Habits (singletonMap test_habit_id test_habit_2) (singleton test_habit_id))
                    >>>
                    liftIO
                  )
            ]
        ------------------------------------------------------------------------
        , testGroup "deleteHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Deleting a non-existing habit returns NoHabitToDelete" $ do
            --------------------------------------------------------------------
                deleteHabit test_habit_id >>= ((@?= NoHabitToDelete) >>> liftIO)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit then deleting it returns HabitDeleted and causes fetching all habits to return an empty map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                deleteHabit test_habit_id >>= ((@?= HabitDeleted) >>> liftIO)
                getHabits >>= (view habit_count_ >>> (@?= 0) >>> liftIO)
            ]
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ----------------------------------------------------------------------------
            getHabits >>= (view habit_count_ >>> (@?= 0) >>> liftIO)
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching a habit when none exist returns Nothing" $
        ----------------------------------------------------------------------------
            getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
            >>=
            ((@?= Nothing) >>> liftIO)
        ----------------------------------------------------------------------------
        , testGroup "putHabit"
        ----------------------------------------------------------------------------
            [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabit test_habit_id >>= ((@?= Just test_habit) >>> liftIO)
            ------------------------------------------------------------------------
            , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabits
                  >>=
                  (
                    (@?= Habits (singletonMap test_habit_id test_habit) (singleton test_habit_id))
                    >>>
                    liftIO
                  )
            ------------------------------------------------------------------------
            , testGroup "Putting two habits causes them to be returned in order of creation" $
            ------------------------------------------------------------------------
              [ apiTestCase "Test habit 1 followed by test habit 2" $ do
              ------------------------------------------------------------------------
                  createHabit test_habit_id test_habit
                  createHabit test_habit_id_2 test_habit_2
                  getHabits
                    >>=
                    (
                      (@?=
                        Habits
                          (mapFromList [(test_habit_id, test_habit), (test_habit_id_2, test_habit_2)])
                          (fromList [test_habit_id, test_habit_id_2])
                      )
                      >>>
                      liftIO
                    )
              ------------------------------------------------------------------------
              , apiTestCase "Test habit 2 followed by test habit 1" $ do
              ------------------------------------------------------------------------
                  createHabit test_habit_id_2 test_habit_2
                  createHabit test_habit_id test_habit
                  getHabits
                    >>=
                    (
                      (@?=
                        Habits
                          (mapFromList [(test_habit_id_2, test_habit_2), (test_habit_id, test_habit)])
                          (fromList [test_habit_id_2, test_habit_id])
                      )
                      >>>
                      liftIO
                    )
              ]
            ------------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                markHabits [test_habit_id] [test_habit_id_2]
                getCredits >>= ((@?= Credits (Successes 0.5) (Failures 4)) >>> liftIO)
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
            [ apiTestCase "Marking a habit gets the right credits" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                markHabits [test_habit_id] [test_habit_id_2]
                credits @(Credits actual_successes actual_failures) ← getCredits
                credits |> show |> putStrLn |> liftIO
                let expected_successes = test_habit ^. difficulty_ |> scaleFactor
                    expected_failures = test_habit_2 ^. importance_ |> scaleFactor
                liftIO $ do
                  assertBool
                    ("successes should be " ⊕ show expected_successes ⊕ " not " ⊕ show actual_successes)
                    (abs ((credits ^. successes_) - expected_successes) < 0.1)
                  assertBool
                    ("failures should be " ⊕ show expected_failures ⊕ " not " ⊕ show actual_failures)
                    (abs ((credits ^. failures_ ) - expected_failures ) < 0.1)
            ]
        ]
    ----------------------------------------------------------------------------
    , testGroup "Web" $
    ----------------------------------------------------------------------------
        [ testGroup "Redirections" $
        ------------------------------------------------------------------------
            [ webTestCase "GET / redirects to /login" $ do
                (response, _) ← requestDocument "/" $ setRequestMethod "GET"
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
                (_, tags) ← requestDocument "/" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - List of Habits"
            , webTestCase "Creating an account then logging in redirects to /" $ do
                _ ← createTestAccount "username" "password"
                (response, _) ← loginTestAccount "username" "password"
                assertRedirectsTo response "/"
            , webTestCase "Creating an account makes /load the list of habits" $ do
                _ ← createTestAccount "username" "password"
                (_, tags) ← requestDocument "/" $ setRequestMethod "GET"
                assertPageTitleEquals tags "Habit of Fate - List of Habits"
            , webTestCase "Creating a conflicting account displays an error message" $ do
                _ ← createTestAccount "username" "password"
                (response, tags) ← createTestAccount "username" "password"
                liftIO $ getResponseStatusCode response @?= 409
                assertTextIs tags "error-message" "This account already exists."
            ]
        ------------------------------------------------------------------------
        , testGroup "Habits" $
        ------------------------------------------------------------------------
            [ webTestCase "Open the habit edit page for an existing habit." $ do
                _ ← createTestAccount "username" "password"
                createHabitViaWeb test_habit_id_2 test_habit_2
                (response, tags) ←
                  requestDocument
                    ("/edit/" ⊕ (test_habit_id_2 |> show |> (^. packedChars))) $ setRequestMethod "GET"
                liftIO $ do
                  responseStatus response @?= ok200
                  extractHabit tags >>= (@?= test_habit_2)
            ]
        ]
        ------------------------------------------------------------------------
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Story"
  ------------------------------------------------------------------------------
    [ testGroup "s"
    ----------------------------------------------------------------------------
      [ testCase "just a substitution" $ olength [s|{test}|] @?= 1
      , testCase "single story plain text" $
          olength [s|line1|] @?= 1
      , testCase "2 stories: both non-empty" $
          olength [s|line1
                    =
                    line2
                   |] @?= 2
      ]
    ]
  ------------------------------------------------------------------------------
  , testGroup "HabitOfFate.Substitution"
  ------------------------------------------------------------------------------
    [ testGroup "parseAtom"
    ----------------------------------------------------------------------------
      [ testCase "literal, one char" $
          runParser parseAtom () "<story>" "literal" @?= Right (Literal 'l')
      , testCase "literal, whole string" $
          runParser (many parseAtom) () "<story>" "xyz" @?= Right [Literal 'x', Literal 'y', Literal 'z']
      , testCase "substitution, name" $
          runParser parseAtom () "<story>" "|" @?=
            Right (Substitution $ SubstitutionData False True Name "")
      , testCase "substitution, subject" $
          runParser parseAtom () "<story>" "he/she|x" @?=
            Right (Substitution $ SubstitutionData False False (Referrent Subject) "x")
      , testCase "substitution, subject, multiparse" $
          runParser (many parseAtom) () "<story>" "he/she|x" @?=
            Right [Substitution $ SubstitutionData False False (Referrent Subject) "x"]
      , testCase "substitution, proper possessive" $
          runParser parseAtom () "<story>" "his/hers|Sue" @?=
            Right (Substitution $ SubstitutionData False False (Referrent ProperPossessive) "Sue")
      , testCase "substitution, with article" $
          runParser parseAtom () "<story>" "an |illsbane" @?=
            Right (Substitution $ SubstitutionData True False Name "illsbane")
      , testCase "substitution, with capitalized article" $
          runParser parseAtom () "<story>" "An |illsbane" @?=
            Right (Substitution $ SubstitutionData True True Name "illsbane")
      , testCase "substitution, with article and a newline" $
          runParser parseAtom () "<story>" "an\n|illsbane" @?=
            Right (Substitution $ SubstitutionData True False Name "illsbane")
      , testCase "substitution, uppercase referrant" $
          runParser parseAtom () "<story>" "His/hers|Katie" @?=
            Right (Substitution $ SubstitutionData False True (Referrent ProperPossessive) "Katie")
      , testCase "substitution, uppercase name" $
          runParser parseAtom () "<story>" "|Katie" @?=
            Right (Substitution $ SubstitutionData False True Name "Katie")
      ]
    ----------------------------------------------------------------------------
    , testProperty "round-trip" $ \x →
        x |> convertSubstitutionDataToTag
          |> Lazy.fromStrict
          |> parseText def
          |> fmap documentRoot
          |> (>>= parseSubstitutionTag)
          |> (_Left %~ show)
          |> (@?= Right x)
          |> ioProperty
    ----------------------------------------------------------------------------
    ]
  ]
