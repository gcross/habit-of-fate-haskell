{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements)

import Control.Concurrent
import Control.Lens.Extras
import qualified Data.Map as Map
import qualified Data.UUID as UUID
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import System.IO
import System.Log
import System.Log.Handler.Simple
import System.Log.Logger
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

import HabitOfFate.Client
import HabitOfFate.Credits
import HabitOfFate.Habit
import HabitOfFate.Server
import HabitOfFate.Story

header ∷ String → String
header header = replicate left_dash_count '-' ⊕ " " ⊕ header ⊕ " " ⊕ replicate right_dash_count '-'
  where
    dash_count = 80 - 2 - olength header
    right_dash_count = dash_count `div` 2
    left_dash_count = dash_count - right_dash_count

serverTestCaseWithoutAccount ∷ String → (Int → IO ()) → TestTree
serverTestCaseWithoutAccount name action = testCase name $ do
  debugM "Test" $ header name
  tempdir ← getTemporaryDirectory
  filepath ← (tempdir </>) ∘ ("test-" ⊕) <$> replicateM 8 (randomRIO ('A','z'))
  withApplication
    (makeApp filepath)
    action

serverTestCase ∷ String → (Client ()) → TestTree
serverTestCase name action = serverTestCaseWithoutAccount name $ \port →
  createAccount "bitslayer" "password" Testing "localhost" port
  >>=
  runReaderT action ∘ fromMaybe (error "Unable to create account.")

initialize = do
  doesFileExist "test.log" >>= flip when (removeFile "test.log")
  file_handler ← fileHandler "test.log" DEBUG
  updateGlobalLogger rootLoggerName $
    setLevel DEBUG
    ∘
    setHandlers [file_handler]

test_habit = Habit "name" (Credits 1 1)
test_habit_2 = Habit "test" (Credits 2 2)

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

main = initialize >> (defaultMain $ testGroup "All Tests"
  [ testGroup "HabitOfFate.Server"
    [ testGroup "Missing username/password" $
        let testMissing test_name path =
              serverTestCaseWithoutAccount test_name $ \port → do
                manager ← newManager defaultManagerSettings
                response ← flip httpNoBody manager $ defaultRequest
                  { method = renderStdMethod POST
                  , host = "localhost"
                  , port = port
                  , path = path
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
    , serverTestCase "Get all habits when none exist" $
        fetchHabits
        >>=
        liftIO ∘ (@?= Map.empty)
    , serverTestCase "Get a particular habit when none exist" $
        fetchHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
        >>=
        liftIO ∘ (@?= Nothing)
    , testGroup "Putting a habit..."
        [ serverTestCase "...then fetching it, returns the habit" $ do
            putHabit test_habit_id test_habit
            fetchHabit test_habit_id >>= liftIO ∘ (@?= Just test_habit)
        , serverTestCase "...then fetching all habits, returns just the habit" $ do
            putHabit test_habit_id test_habit
            fetchHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit)
        ]
    , testGroup "Deleting a habit..."
        [ serverTestCase "...that does not exist, returns NoHabitToDelete" $ do
            deleteHabit test_habit_id >>= liftIO ∘ (@?= NoHabitToDelete)
        , testGroup "...that exists..."
            [ serverTestCase "...returns HabitDeleted" $ do
                putHabit test_habit_id test_habit
                deleteHabit test_habit_id >>= liftIO ∘ (@?= HabitDeleted)
            , serverTestCase "...causes no habits to show up when all are fetched" $ do
                putHabit test_habit_id test_habit
                void $ deleteHabit test_habit_id
                fetchHabits >>= liftIO ∘ (@?= Map.empty)
            ]
        ]
    , serverTestCase "Create a habit, replace it, and fetch all habits" $ do
        putHabit test_habit_id test_habit
        putHabit test_habit_id test_habit_2
        fetchHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit_2)
    , serverTestCase "Mark habits." $ do
        putHabit test_habit_id test_habit
        putHabit test_habit_id_2 test_habit_2
        markHabits [test_habit_id] [test_habit_id_2]
        getCredits >>= liftIO ∘ (@?= Credits 1 2)
    ]
  , testGroup "HabitOfFate.Story"
    [ testGroup "s"
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
    , testGroup "makeSubstitutor"
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
    , testGroup "substitute"
        [ testCase "single letter" $ do
            let GenEvent [subparagraph] = [s_fixed|{x}|]
            Right "X" @=? (
              rewords
              ∘
              textFromParagraph
              <$>
              substitute (const ∘ Right ∘ Text_ $ "X") subparagraph
             )
        , testCase "two keys separated by a space" $ do
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
        , testCase "paragraph" $ do
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
    , testGroup "rendering"
        [ testCase "three Text_, middle space" $
            (renderStoryToText $ GenStory [GenQuest [GenEvent ["X Y"]]])
            @?=
            (renderStoryToText $ GenStory [GenQuest [GenEvent [mconcat ["X", " ", "Y"]]]])
        ]
    ]
  ]
 )
