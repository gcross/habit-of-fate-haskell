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

import HabitOfFate.Prelude hiding (elements)

import Control.Exception
import qualified Data.Map as Map
import Data.IORef
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import System.IO
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import Text.XML (parseLBS)
import Text.XML.Cursor
import Web.JWT

import HabitOfFate.Client
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Logging
import HabitOfFate.Server
import HabitOfFate.Story

no_files ∷ FileLocator
no_files = const $ pure Nothing

withTestApp ∷ FileLocator → (Int → IO ()) → IO ()
withTestApp locateWebAppFile =
  withApplication
    (makeApp locateWebAppFile (secret "test secret") mempty (const $ pure ()))

withTestAppNoFiles ∷ (Int → IO ()) → IO ()
withTestAppNoFiles = withTestApp $ no_files

serverTestCase ∷ String → FileLocator → (Int → IO ()) → TestTree
serverTestCase test_name locateWebAppFile =
  testCase test_name
  ∘
  withTestAppNoFiles

serverTestCaseNoFiles ∷ String → (Int → IO ()) → TestTree
serverTestCaseNoFiles test_name = serverTestCase test_name no_files

apiTestCase ∷ String → (ClientIO ()) → TestTree
apiTestCase test_name action =
  serverTestCaseNoFiles test_name
  $
  createAccount "bitslayer" "password" Testing "localhost"
  >=>
  runClientT action ∘ fromMaybe (error "Unable to create account.")

test_habit = Habit "name" Low Medium
test_habit_2 = Habit "test" Medium VeryHigh

test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563"
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9"

originalFromSubParagraph ∷ SubParagraph → Text
originalFromSubParagraph =
  rewords
  ∘
  foldMap (
    \case
      Literal t → t
      Key k → "{" ⊕ k ⊕ "}"
  )

originalFromSubEvent ∷ SubEvent → Text
originalFromSubEvent =
  mconcat
  ∘
  intersperse "\n"
  ∘
  map originalFromSubParagraph
  ∘
  unwrapGenEvent

createHabit habit_id habit = putHabit habit_id habit >>= liftIO ∘ (@?= HabitCreated)
replaceHabit habit_id habit = putHabit habit_id habit >>= liftIO ∘ (@?= HabitReplaced)

main = defaultMain $ testGroup "All Tests"
  ------------------------------------------------------------------------------
  [ testGroup "HabitOfFate.Server"
  ------------------------------------------------------------------------------
    [ testGroup "JSON API" $
    ----------------------------------------------------------------------------
        let apiTestCase ∷ String → (ClientIO ()) → TestTree
            apiTestCase test_name action =
              serverTestCaseNoFiles test_name
              $
              createAccount "bitslayer" "password" Testing "localhost"
              >=>
              runClientT action ∘ fromMaybe (error "Unable to create account.")
        in
        ------------------------------------------------------------------------
        [ testGroup "Missing username/password" $
        ------------------------------------------------------------------------
            let testMissing test_name path =
                  serverTestCase test_name no_files $ \port → do
                    manager ← newManager defaultManagerSettings
                    response ← flip httpNoBody manager $ defaultRequest
                      { method = renderStdMethod POST
                      , host = "localhost"
                      , port = port
                      , path = "/api/" ⊕ path
                      }
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
                    manager ← newManager defaultManagerSettings
                    response ← flip httpNoBody manager $ defaultRequest
                      { method = renderStdMethod POST
                      , host = "localhost"
                      , port = port
                      , path = "/api/" ⊕ path
                      }
                    400 @=? responseStatusCode response
            in
            [ testGroup "Create account"
                [ testEmpty "Empty username" "create?username="
                , testEmpty "Empty password" "create?password="
                ]
            ]
        ------------------------------------------------------------------------
        , apiTestCase "Fetching all habits from a new account returns an empty array" $
        ------------------------------------------------------------------------
            getHabits
            >>=
            liftIO ∘ (@?= Map.empty)
        ------------------------------------------------------------------------
        , apiTestCase "Fetching a habit when none exist returns Nothing" $
        ------------------------------------------------------------------------
            getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
            >>=
            liftIO ∘ (@?= Nothing)
        ------------------------------------------------------------------------
        , testGroup "putHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabit test_habit_id >>= liftIO ∘ (@?= Just test_habit)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                getHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                replaceHabit test_habit_id test_habit_2
                getHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit_2)
            ]
        ------------------------------------------------------------------------
        , testGroup "deleteHabit"
        ------------------------------------------------------------------------
            [ apiTestCase "Deleting a non-existing habit returns NoHabitToDelete" $ do
            --------------------------------------------------------------------
                deleteHabit test_habit_id >>= liftIO ∘ (@?= NoHabitToDelete)
            --------------------------------------------------------------------
            , apiTestCase "Putting a habit then deleting it returns HabitDeleted and causes fetching all habits to return an empty map" $ do
            --------------------------------------------------------------------
                createHabit test_habit_id test_habit
                deleteHabit test_habit_id >>= liftIO ∘ (@?= HabitDeleted)
                getHabits >>= liftIO ∘ (@?= Map.empty)
            ]
        ]
    ----------------------------------------------------------------------------
    , testGroup "Files"
    ----------------------------------------------------------------------------
        [ serverTestCaseNoFiles "Missing root" $ \port → do
        ------------------------------------------------------------------------
            manager ← newManager defaultManagerSettings
            response ← flip httpNoBody manager $ defaultRequest
              { method = renderStdMethod GET
              , host = "localhost"
              , port = port
              , path = "/"
              }
            404 @=? responseStatusCode response
        ------------------------------------------------------------------------
        , testGroup "Existing file" $
        ------------------------------------------------------------------------
            flip map
              [ ("/","index.html")
              , ("/index.html","index.html")
              , ("/habit-of-fate.js","habit-of-fate.js")
              ]
            $
            \(request_path, recognized_filename) → do
              testCase [i|#{request_path} -> #{recognized_filename}|] $
                withSystemTempFile "hoftest" $ \temp_filepath handle → do
                  hPutStr handle "testdata"
                  hFlush handle
                  withTestApp (pure ∘ flip lookup [(recognized_filename, temp_filepath)]) $ \port → do
                    manager ← newManager defaultManagerSettings
                    response ← flip httpLbs manager $ defaultRequest
                      { method = renderStdMethod GET
                      , host = "localhost"
                      , port = port
                      , path = request_path
                      }
                    200 @=? responseStatusCode response
                    "testdata" @=? responseBody response
        ]
    ----------------------------------------------------------------------------
    , apiTestCase "Fetching all habits from a new account returns an empty array" $
    ----------------------------------------------------------------------------
        getHabits
        >>=
        liftIO ∘ (@?= Map.empty)
    ----------------------------------------------------------------------------
    , apiTestCase "Fetching a habit when none exist returns Nothing" $
    ----------------------------------------------------------------------------
        getHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
        >>=
        liftIO ∘ (@?= Nothing)
    ----------------------------------------------------------------------------
    , testGroup "putHabit"
    ----------------------------------------------------------------------------
        [ apiTestCase "Putting a habit and then fetching it returns the habit" $ do
        ------------------------------------------------------------------------
            createHabit test_habit_id test_habit
            getHabit test_habit_id >>= liftIO ∘ (@?= Just test_habit)
        ------------------------------------------------------------------------
        , apiTestCase "Putting a habit causes fetching all habits to return a singleton map" $ do
        ------------------------------------------------------------------------
            createHabit test_habit_id test_habit
            getHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit)
        ------------------------------------------------------------------------
        , apiTestCase "Putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
        ------------------------------------------------------------------------
            createHabit test_habit_id test_habit
            createHabit test_habit_id_2 test_habit_2
            markHabits [test_habit_id] [test_habit_id_2]
            getCredits >>= liftIO ∘ (@?= Credits 0.5 4)
        ------------------------------------------------------------------------
        , testCase "Putting a habit causes the accounts to be written" $ do
        ------------------------------------------------------------------------
            write_requested_ref ← newIORef False
            withApplication
              (makeApp no_files (secret "test secret") mempty (const $ writeIORef write_requested_ref True))
              $
              \port → do
                session_info ← fromJust <$> createAccount "bitslayer" "password" Testing "localhost" port
                flip runClientT session_info $ createHabit test_habit_id test_habit
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
            liftIO ∘ putStrLn ∘ show $ credits
            let expected_successes = scaleFactor $ test_habit ^. difficulty
                expected_failures = scaleFactor $ test_habit_2 ^. importance
            liftIO $ do
              assertBool
                ("successes should be " ⊕ show expected_successes ⊕ " not " ⊕ show actual_successes)
                (abs (actual_successes - expected_successes) < 0.1)
              assertBool
                ("failures should be " ⊕ show expected_failures ⊕ " not " ⊕ show actual_failures)
                (abs (actual_failures - expected_failures) < 0.1)
        ]
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
              ∘
              (Right "Y" @=?)
              ∘
              (_Right %~ textFromParagraph)
              $
              makeSubstitutor
                (flip lookup [("X", Gendered "Y" (error "should not be using the gender"))])
                (const Nothing)
                "X"
          , testCase "neutered"
              ∘
              (Right "Y" @=?)
              ∘
              (_Right %~ textFromParagraph)
              $
              makeSubstitutor
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
              rewords
              ∘
              textFromParagraph
              <$>
              substitute (const ∘ Right ∘ Text_ $ "X") subparagraph
             )
        ------------------------------------------------------------------------
        , testCase "two keys separated by a space" $ do
        ------------------------------------------------------------------------
            let GenEvent [subparagraph] = [s_fixed|{x} {y}|]
            Right "X Y" @=? (
              rewords
              ∘
              textFromParagraph
              <$>
              substitute (
                  fmap Text_
                  ∘
                  \case {"x" → Right "X"; "y" → Right "Y"; _ → Left "not found"}
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
              rewords
              ∘
              textFromParagraph
              <$>
              substitute (
                  fmap Text_
                  ∘
                  (\case
                    "Susie" → Right "Mark"
                    "her|pos" → Right "his"
                    "son" → Right "daughter"
                    "Tommy" → Right "Sally"
                    "an Illsbane" → Right "a Wolfsbane"
                    "she" → Right "he"
                    other → Left ("not found: " ⊕ show other)
                  )
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
