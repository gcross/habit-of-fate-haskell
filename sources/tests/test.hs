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

import Control.Monad.Catch
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.Data.Lens (uniplate)
import Data.IORef
import Data.List (cycle)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Text (strip)
import Data.Text.Strict.Lens (utf8)
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID, fromText)
import Network.HTTP.Client hiding (httpNoBody)
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO hiding (utf8)
import Test.Tasty
import Test.Tasty.HUnit
import Text.HTML.DOM (sinkDoc)
import Text.XML.Lens
  ( Document
  , (./)
  , attribute
  , elementAttributes
  , named
  , root
  , text
  )
import qualified Text.XML.Lens as XML
import Web.Scotty (parseParam)

import HabitOfFate.API
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Server
import HabitOfFate.Story

withTestApp ∷ (Int → IO ()) → IO ()
withTestApp = withApplication (makeAppRunningInTestMode mempty (const $ pure ()))

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
test_habit = Habit "name" (Difficulty Low) (Importance Medium)
test_habit_2 = Habit "test" (Difficulty Medium) (Importance VeryHigh)

test_habit_id, test_habit_id_2 ∷ UUID
test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9"

originalFromSubParagraph ∷ SubParagraph → Text
originalFromSubParagraph =
  foldMap (
    \case
      Literal t → t
      Key k → "{" ⊕ k ⊕ "}"
  )
  >>>
  rewords

originalFromSubEvent ∷ SubEvent → Text
originalFromSubEvent =
  unwrapGenEvent
  >>>
  map originalFromSubParagraph
  >>>
  intersperse "\n"
  >>>
  mconcat

createHabit habit_id habit = putHabit habit_id habit >>= ((@?= HabitCreated) >>> liftIO)
replaceHabit habit_id habit = putHabit habit_id habit >>= ((@?= HabitReplaced) >>> liftIO)

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
            getHabits >>= (view habit_count >>> (@?= 0) >>> liftIO)
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
                getHabits >>= (view habit_count >>> (@?= 0) >>> liftIO)
            ]
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ----------------------------------------------------------------------------
            getHabits >>= (view habit_count >>> (@?= 0) >>> liftIO)
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
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            ------------------------------------------------------------------------
                createHabit test_habit_id test_habit
                createHabit test_habit_id_2 test_habit_2
                markHabits [test_habit_id] [test_habit_id_2]
                getCredits >>= ((@?= Credits 0.5 4) >>> liftIO)
            ------------------------------------------------------------------------
            , testCase "Putting a habit causes the accounts to be written" $ do
            ------------------------------------------------------------------------
                write_requested_ref ← newIORef False
                withApplication
                  (makeAppRunningInTestMode mempty (const $ writeIORef write_requested_ref True))
                  $
                  \port → do
                    session_info ← fromJust <$> createAccount "bitslayer" "password" Testing "localhost" port
                    flip runSessionT session_info $ createHabit test_habit_id test_habit
                readIORef write_requested_ref >>= assertBool "Write was not requested."
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
                let expected_successes = test_habit ^. difficulty |> scaleFactor
                    expected_failures = test_habit_2 ^. importance |> scaleFactor
                liftIO $ do
                  assertBool
                    ("successes should be " ⊕ show expected_successes ⊕ " not " ⊕ show actual_successes)
                    (abs (actual_successes - expected_successes) < 0.1)
                  assertBool
                    ("failures should be " ⊕ show expected_failures ⊕ " not " ⊕ show actual_failures)
                    (abs (actual_failures - expected_failures) < 0.1)
            ]
        ]
    ----------------------------------------------------------------------------
    , testGroup "Web" $
        let webTestCase ∷ String → ReaderT Int (StateT CookieJar IO) () → TestTree
            webTestCase test_name runTest =
              serverTestCase test_name $ \port → do
                current_time ← liftIO getCurrentTime
                runTest
                  |> flip runReaderT port
                  |> void
                  |> flip runStateT mempty
                  |> void
            requestDocument ∷
              ByteString →
              (Request → Request) →
              ReaderT Int (StateT CookieJar IO) (Response (), Document)
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
              (response, doc) ← liftIO $ httpSink request (\response → (response,) <$> sinkDoc)
              let (updated_cookie_jar, response_without_cookie) =
                    updateCookieJar response request current_time new_cookie_jar
              put updated_cookie_jar
              return (response_without_cookie, doc)
            assertRedirectsTo response expected_location = liftIO $
              getResponseHeader "Location" response @?= [expected_location]
            assertPageTitleEquals doc expected_page_title = liftIO $
              doc ^? root ./ named "head" ./ named "title" . text
                @?= Just expected_page_title
            assertTextIs doc element_id expected_text = liftIO $
              (
                findOf
                  (cosmosOf uniplate)
                  (elementAttributes >>> lookup "id" >>> (== Just element_id))
                  (doc ^. root)
                |> fmap (^. text)
              )
              @?= Just expected_text
            createTestAccount username password =
              requestDocument "/create" $
                setRequestBodyURLEncoded
                  [ ("username",username)
                  , ("password1",password)
                  , ("password2",password)
                  ]
            loginTestAccount username password =
              requestDocument "/login" $
                setRequestBodyURLEncoded
                  [ ("username",username)
                  , ("password",password)
                  ]
            createHabit habit_id habit = void $
              requestDocument ("/habits/" ⊕ BS8.pack (show habit_id))
              $
              setRequestBodyURLEncoded
                [ ("name", habit ^. name . re utf8)
                , ("importance", habit ^. importance . to (show >>> BS8.pack))
                , ("difficulty", habit ^. difficulty . to (show >>> BS8.pack))
                ]
            readHabitsIn doc = liftIO $ do
              let rows = doc ^.. root ./ named "body" ./ named "table" ./ named "tr"
                  number_of_rows = length rows
                  observed_classes = map (^. attribute "class") rows
                  expected_classes = take number_of_rows (cycle [Just "row odd", Just "row even"])
              observed_classes @?= expected_classes

              forM (zip [(1∷Int)..] rows) $ \(i, row) → do
                case row ^.. uniplate . named "td" of
                  [name, difficulty, importance] → do
                    habit_id_unparsed ←
                      maybe
                        (assertFailure "No link to the habit page.")
                        (drop (olength ("/habits/" ∷ Text)) >>> pure)
                        (name ^. uniplate . named "a" . attribute "href")
                    habit_id ←
                      maybe
                        (assertFailure $ printf "UUID %s did not parse sucessfully." habit_id_unparsed)
                        pure
                        (fromText habit_id_unparsed)
                    let parseColumn name =
                          (^.. text)
                          >>>
                          mconcat
                          >>>
                          words
                          >>>
                          unwords
                          >>>
                          fromStrict
                          >>>
                          (\column_text →
                            column_text
                            |> parseParam
                            |> either
                                (\error_message → Lazy.unpack >>> assertFailure $
                                  "Error parsing \"" ⊕ column_text ⊕ "\": " ⊕ error_message)
                                pure
                          )
                    difficulty_scale ← parseColumn "difficulty" difficulty
                    importance_scale ← parseColumn "importance" importance
                    pure
                      ( habit_id
                      , Habit
                          (maybe "(no name)" (^. text) (name ^? uniplate . named "a"))
                          difficulty_scale
                          importance_scale
                      )
                  x → assertFailure $ printf "Row %i has %i columns" i (length x)
        in
    ----------------------------------------------------------------------------
        [ webTestCase "GET / redirects to /habits" $ do
            (response, _) ← requestDocument "/" $ setRequestMethod "GET"
            assertRedirectsTo response "/habits"
        , webTestCase "GET /login returns login page" $ do
            (_, doc) ← requestDocument "/login" $ setRequestMethod "GET"
            assertPageTitleEquals doc "Habit of Fate - Login"
        , webTestCase "POST /login for non-existent user returns login page withe error" $ do
            (_, doc) ← requestDocument "/login" $
              setRequestBodyURLEncoded [("username","username"), ("password","password")]
            assertPageTitleEquals doc "Habit of Fate - Login"
            assertTextIs doc "error-message" "No account has that username."
        , webTestCase "GET /create returns account creation page" $ do
            (_, doc) ← requestDocument "/create" $ setRequestMethod "GET"
            assertPageTitleEquals doc "Habit of Fate - Account Creation"
        , webTestCase "POST /create with fields filled in redirects to /" $ do
            (response, _) ← createTestAccount "username" "password"
            assertRedirectsTo response "/"
        , webTestCase "Creating an account causes /habits to load the habits page" $ do
            _ ← createTestAccount "username" "password"
            (_, doc) ← requestDocument "/habits" $ setRequestMethod "GET"
            assertPageTitleEquals doc "Habit of Fate - List of Habits"
        , webTestCase "Creating an account then logging in redirects to /habits" $ do
            _ ← createTestAccount "username" "password"
            (response, _) ← loginTestAccount "username" "password"
            assertRedirectsTo response "/habits"
        , webTestCase "Creating an account makes /habits load the list of habits" $ do
            _ ← createTestAccount "username" "password"
            (_, doc) ← requestDocument "/habits" $ setRequestMethod "GET"
            assertPageTitleEquals doc "Habit of Fate - List of Habits"
        , webTestCase "Creating a conflicting account displays an error message" $ do
            _ ← createTestAccount "username" "password"
            (response, doc) ← createTestAccount "username" "password"
            liftIO $ getResponseStatusCode response @?= 409
            assertTextIs doc "error-message" "This account already exists."
        , webTestCase "Create an account and then a habit and check /habits" $ do
            _ ← createTestAccount "username" "password"
            createHabit test_habit_id test_habit
            (_, doc) ← requestDocument "/habits" $ setRequestMethod "GET"
            habits ← readHabitsIn doc
            liftIO $ habits @?= [(test_habit_id, test_habit)]
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
      , testCase "2 stories: 1 empty, 1 non-empty" $
          olength [s|line1
                    =
                   |] @?= 1
      , testCase "2 stories: both non-empty" $
          olength [s|line1
                    =
                    line2
                   |] @?= 2
      , testCase "single literal letter round trip" $
          "x" @?= originalFromSubEvent [s_fixed|x|]
      , testCase "single key letter round trip" $
          "{x}" @?= originalFromSubEvent [s_fixed|{x}|]
      , testCase "two keys separated by a space" $
          "{x} {y}" @?= originalFromSubEvent [s_fixed|{x} {y}|]
      ]
    ----------------------------------------------------------------------------
    , testGroup "makeSubstitutor"
    ----------------------------------------------------------------------------
      [ testGroup "name" $
          [ testCase "gendered"
              <| (Right "Y" @=?)
              <| _Right %~ textFromParagraph
              <| makeSubstitutor
                  (flip lookup [("X", Gendered "Y" (error "should not be using the gender"))])
                  (const Nothing)
                  "X"
          , testCase "neutered"
              <| (Right "Y" @=?)
              <| _Right %~ textFromParagraph
              <| makeSubstitutor
                  (const Nothing)
                  (flip lookup [("X","Y")])
                  "X"
          ]
      ]
    ----------------------------------------------------------------------------
    , testGroup "substitute"
    ----------------------------------------------------------------------------
        [ testCase "single letter" $ do
        ------------------------------------------------------------------------
            let GenEvent [subparagraph] = [s_fixed|{x}|]
            Right "X" @=? (
              (textFromParagraph >>> rewords)
              <$>
              substitute ("X" |> Text_ |> Right |> const) subparagraph
             )
        ------------------------------------------------------------------------
        , testCase "two keys separated by a space" $ do
        ------------------------------------------------------------------------
            let GenEvent [subparagraph] = [s_fixed|{x} {y}|]
            Right "X Y" @=? (
              (textFromParagraph >>> rewords)
              <$>
              substitute (
                  \case {"x" → Right "X"; "y" → Right "Y"; _ → Left "not found"}
                  >>>
                  fmap Text_
              ) subparagraph
             )
        ------------------------------------------------------------------------
        , testCase "paragraph" $ do
        ------------------------------------------------------------------------
            let GenEvent [subparagraph] = [s_fixed|
The last thing in the world that <introduce>{Susie}</introduce> wanted to do was
to wander alone in the Wicked Forest at night, but {her|pos} {son}, little
<introduce>{Tommy}</introduce>, was sick and would not live through the night
unless {Susie} could find <introduce>{an Illsbane}</introduce> plant. It is a
hopeless task, but {she} has no other choice.
|]
            Right "The last thing in the world that Mark wanted to do was to wander alone in the Wicked Forest at night, but his daughter, little Sally, was sick and would not live through the night unless Mark could find a Wolfsbane plant. It is a hopeless task, but he has no other choice." @=? (
              (textFromParagraph >>> rewords)
              <$>
              substitute (
                  \case
                    "Susie" → Right "Mark"
                    "her|pos" → Right "his"
                    "son" → Right "daughter"
                    "Tommy" → Right "Sally"
                    "an Illsbane" → Right "a Wolfsbane"
                    "she" → Right "he"
                    other → Left ("not found: " ⊕ show other)
                  >>>
                  fmap Text_
              ) subparagraph
             )
        ]
    ----------------------------------------------------------------------------
    , testGroup "rendering"
    ----------------------------------------------------------------------------
        [ testCase "three Text_, middle space" $
            (renderStoryToText $ GenStory [GenQuest [GenEvent ["X Y"]]])
            @?=
            (renderStoryToText $ GenStory [GenQuest [GenEvent [mconcat ["X", " ", "Y"]]]])
        ]
    ]
  ]
