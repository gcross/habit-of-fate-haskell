{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import Network.Wai.Handler.Warp
import System.Random
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import HabitOfFate.Client
import HabitOfFate.Habit
import HabitOfFate.Server
import HabitOfFate.Unicode

serverTestCase ∷ String → (Port → IO ()) → Test
serverTestCase name action = testCase name $ do
  filepath ← ("test-" ⊕) <$> replicateM 8 (randomRIO ('a','Z'))
  withApplication (makeApp filepath) action

fetchHabits_ = fetchHabits "localhost"

main = defaultMain
  [ serverTestCase "/habits" $
      fetchHabits_
      >=>
      (@?= Map.empty)
  ]
