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
import Web.JWT

import HabitOfFate.Client
import HabitOfFate.Credits
import HabitOfFate.Habit
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

apiTestCase ∷ String → (Client ()) → TestTree
apiTestCase test_name action =
  serverTestCaseNoFiles test_name
  $
  createAccount "bitslayer" "password" Testing "localhost"
  >=>
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

createHabit habit_id habit = putHabit habit_id habit >>= liftIO ∘ (@?= HabitCreated)
replaceHabit habit_id habit = putHabit habit_id habit >>= liftIO ∘ (@?= HabitReplaced)

main = initialize >> (defaultMain $ testGroup "All Tests"
  [ testGroup "HabitOfFate.Server"
    [ testGroup "Missing username/password" $
        let testMissing test_name path =
              serverTestCaseNoFiles test_name $ \port → do
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
    , testGroup "Files"
        [ testGroup "Missing" $
            flip map ["/", "/app/index.html", "dummy"] $ \path →
              serverTestCaseNoFiles path $ \port → do
                  manager ← newManager defaultManagerSettings
                  response ← flip httpNoBody manager $ defaultRequest
                    { method = renderStdMethod POST
                    , host = "localhost"
                    , port = port
                    , path = encodeUtf8 ∘ pack $ path
                    }
                  404 @=? responseStatusCode response
        ]
    , apiTestCase "fetching all habits from a new account returns an empty array" $
        fetchHabits
        >>=
        liftIO ∘ (@?= Map.empty)
    , apiTestCase "fetching a habit when none exist returns Nothing" $
        fetchHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
        >>=
        liftIO ∘ (@?= Nothing)
    , testGroup "putHabit"
        [ apiTestCase "putting a habit and then fetching it returns the habit" $ do
            createHabit test_habit_id test_habit
            fetchHabit test_habit_id >>= liftIO ∘ (@?= Just test_habit)
        , apiTestCase "putting a habit causes fetching all habits to return a singleton map" $ do
            createHabit test_habit_id test_habit
            fetchHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit)
        , apiTestCase "putting a habit, replacing it, and then fetching all habits returns the replaced habit" $ do
            createHabit test_habit_id test_habit
            replaceHabit test_habit_id test_habit_2
            fetchHabits >>= liftIO ∘ (@?= Map.singleton test_habit_id test_habit_2)
        ]
    , testGroup "deleteHabit"
        [ apiTestCase "deleting a non-existing habit returns NoHabitToDelete" $ do
            deleteHabit test_habit_id >>= liftIO ∘ (@?= NoHabitToDelete)
        , apiTestCase "putting a habit then deleting it returns HabitDeleted and causes fetching all habits to return an empty map" $ do
            createHabit test_habit_id test_habit
            deleteHabit test_habit_id >>= liftIO ∘ (@?= HabitDeleted)
            fetchHabits >>= liftIO ∘ (@?= Map.empty)
        ]
    , apiTestCase "markHabits" $ do
        createHabit test_habit_id test_habit
        createHabit test_habit_id_2 test_habit_2
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
