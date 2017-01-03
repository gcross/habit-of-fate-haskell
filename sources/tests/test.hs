{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import System.Log
import System.Log.Handler.Simple
import System.Log.Logger
import System.Random
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import HabitOfFate.Client
import HabitOfFate.Habit
import HabitOfFate.Server
import HabitOfFate.Unicode

header header = replicate left_dash_count '-' ++ " " ++ header ++ " " ++ replicate right_dash_count '-'
  where
    dash_count = 80 - 2 - length header
    right_dash_count = dash_count `div` 2
    left_dash_count = dash_count - right_dash_count

serverTestCase ∷ String → (Client ()) → Test
serverTestCase name action = testCase name $ do
  debugM "Test" $ header name
  tempdir ← getTemporaryDirectory
  filepath ← (tempdir </>) ∘ ("test-" ⊕) <$> replicateM 8 (randomRIO ('A','z'))
  withApplication (makeApp filepath) (runReaderT action ∘ ServerInfo "localhost")

initialize = do
  doesFileExist "test.log" >>= flip when (removeFile "test.log")
  file_handler ← fileHandler "test.log" DEBUG
  updateGlobalLogger rootLoggerName $
    setLevel DEBUG
    ∘
    setHandlers [file_handler]

test_habit = Habit "name" 1 1
test_habit_2 = Habit "test" 2 2

main = initialize >> defaultMain
  [ serverTestCase "Get all habits when none exist" $
      fetchHabits
      >>=
      liftIO ∘ (@?= Map.empty)
  , serverTestCase "Get a particular habit when none exist" $
      fetchHabit (read "730e9d4a-7d72-4a28-a19b-0bcc621c1506")
      >>=
      liftIO ∘ (@?= Nothing)
  , serverTestCase "Create and fetch a habit" $
      createHabit test_habit
      >>=
      fetchHabit
      >>=
      liftIO ∘ (@?= Just test_habit)
  , serverTestCase "Create a habit and fetch all habits" $ do
      uuid ← createHabit test_habit
      fetchHabits >>= liftIO ∘ (@?= Map.singleton uuid test_habit)
  , serverTestCase "Create a habit, delete it, and fetch all habits" $ do
      uuid ← createHabit test_habit
      deleteHabit uuid
      fetchHabits >>= liftIO ∘ (@?= Map.empty)
  , serverTestCase "Create a habit, replace it, and fetch all habits" $ do
      uuid ← createHabit test_habit
      replaceHabit uuid test_habit_2
      fetchHabits >>= liftIO ∘ (@?= Map.singleton uuid test_habit_2)
  , serverTestCase "Mark habits." $ do
      uuid_1 ← createHabit test_habit
      uuid_2 ← createHabit test_habit_2
      markHabits [uuid_1] [uuid_2]
      getCredits >>= liftIO ∘ (@?= (1,2))
  ]
