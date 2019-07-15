{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Testing.Server where

import HabitOfFate.Prelude

import Control.Concurrent.STM.TVar (newTVarIO)
import Network.HTTP.Client (CookieJar)
import Network.Wai.Handler.Warp (withApplication)
import Test.Tasty (TestTree)

import HabitOfFate.Server (makeAppRunningInTestMode)
import HabitOfFate.Testing.Assertions ((@?=), testCase)

import HabitOfFate.API

withApplication' action =
  (makeAppRunningInTestMode
    <$> newTVarIO mempty
    <*> newTVarIO False
  )
  >>=
  flip withApplication action

withTestApp ∷ (Int → IO ()) → IO ()
withTestApp action =
  (makeAppRunningInTestMode
    <$> newTVarIO mempty
    <*> newTVarIO False
  )
  >>=
  flip withApplication action

apiTestCase ∷ String → (SessionIO ()) → TestTree
apiTestCase test_name action =
  (
    createAccount "bitslayer" "password" Testing "localhost"
    >=>
    (fromMaybe (error "Unable to create account.") >>> runSessionT action)
  )
  |> serverTestCase test_name

serverTestCase ∷ String → (Int → IO ()) → TestTree
serverTestCase test_name = withTestApp >>> testCase test_name

webTestCase ∷ String → ReaderT Int (StateT CookieJar IO) () → TestTree
webTestCase test_name runTest =
  serverTestCase test_name $ \port → do
    runTest
      |> flip runReaderT port
      |> void
      |> flip runStateT mempty
      |> void

createGroup group_id group = putGroup group_id group >>= (@?= ResourceCreated)
replaceGroup group_id group = putGroup group_id group >>= (@?= ResourceReplaced)

createHabit habit_id habit = putHabit habit_id habit >>= (@?= ResourceCreated)
replaceHabit habit_id habit = putHabit habit_id habit >>= (@?= ResourceReplaced)
