{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements, text)

import Control.Monad.Catch
import qualified Data.Map as Map
import Data.IORef
import Network.HTTP.Simple
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import System.IO
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import Text.HTML.DOM (sinkDoc)
import Text.XML (parseLBS)
import Text.XML.Cursor
import Text.XML.Lens (entire, named, root, text)
import Web.JWT

import HabitOfFate.API
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Logging
import HabitOfFate.Server
import HabitOfFate.Server.Testing
import HabitOfFate.Story

serverTestCase ∷ String → FileLocator → (Int → IO ()) → TestTree
serverTestCase test_name locateWebAppFile =
  withTestAppNoFiles
  >>>
  testCase test_name

serverTestCaseNoFiles ∷ String → (Int → IO ()) → TestTree
serverTestCaseNoFiles test_name = serverTestCase test_name no_files

apiTestCase ∷ String → (SessionIO ()) → TestTree
apiTestCase test_name action =
  (
    createAccount "bitslayer" "password" Testing "localhost"
    >=>
    (fromMaybe (error "Unable to create account.") >>> runSessionT action)
  )
  |> serverTestCaseNoFiles test_name

test_habit = Habit "name" Low Medium
test_habit_2 = Habit "test" Medium VeryHigh

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
              |> serverTestCaseNoFiles test_name
        in
        ------------------------------------------------------------------------
        [ testGroup "Missing username/password" $
        ------------------------------------------------------------------------
            let testMissing test_name path =
                  serverTestCase test_name no_files $ \port → do
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
                  serverTestCase test_name no_files $ \port → do
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
            getHabits
            >>=
            ((@?= Map.empty) >>> liftIO)
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
                getHabits >>= ((@?= Map.singleton test_habit_id test_habit) >>> liftIO)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                replaceHabit test_habit_id test_habit_2
                getHabits >>= ((@?= Map.singleton test_habit_id test_habit_2) >>> liftIO)
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
                getHabits >>= ((@?= Map.empty) >>> liftIO)
            ]
        ----------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ----------------------------------------------------------------------------
            getHabits
            >>=
            ((@?= Map.empty) >>> liftIO)
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
                getHabits >>= ((@?= Map.singleton test_habit_id test_habit) >>> liftIO)
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
                  (makeApp True no_files mempty (const $ writeIORef write_requested_ref True))
                  $
                  \port → do
                    session_info ← fromJust <$> createAccount "bitslayer" "password" Testing "localhost" port
                    flip runSessionT session_info $ createHabit test_habit_id test_habit
                readIORef write_requested_ref >>= assertBool "Write was not requested."
            ]
        ----------------------------------------------------------------------------
        , testGroup "putHabit"
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
    , testGroup "Web"
    ----------------------------------------------------------------------------
        [ serverTestCaseNoFiles "GET / with no cookies redirects to login page" $ \port → do
            doc ← defaultRequest
              |> setRequestMethod "GET"
              |> setRequestSecure False
              |> setRequestHost "localhost"
              |> setRequestPort port
              |> setRequestPath "/"
              |> flip httpSink (const sinkDoc)
            doc ^? root . entire . named "head" . entire . named "title" . text
              @?= Just "Habit of Fate - Login"
        , serverTestCaseNoFiles "GET /create returns account creation page" $ \port → do
            doc ← defaultRequest
              |> setRequestMethod "GET"
              |> setRequestSecure False
              |> setRequestHost "localhost"
              |> setRequestPort port
              |> setRequestPath "/create"
              |> flip httpSink (const sinkDoc)
            doc ^? root . entire . named "head" . entire . named "title" . text
              @?= Just "Habit of Fate - Account Creation"
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
