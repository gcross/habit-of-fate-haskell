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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Testing.Assertions where

import HabitOfFate.Prelude

import Data.CallStack (HasCallStack)
import Test.Tasty (TestTree)
import qualified Test.Tasty.HUnit as HUnit

import HabitOfFate.Testing

assertBool ∷ MonadIO m ⇒ String → Bool → m ()
assertBool message = HUnit.assertBool message >>> liftIO

assertFailure ∷ (HasCallStack, MonadIO m) ⇒ String → m α
assertFailure message = liftIO $
  HUnit.assertFailure (message ⊕ "\n\n" ⊕ stackString)

(@?=) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@?=) x y = liftIO $ (HUnit.@?=) x y

(@=?) ∷ (MonadIO m, Eq α, Show α) ⇒ α → α → m ()
(@=?) x y = liftIO $ (HUnit.@=?) x y

testCase ∷ String → IO () → TestTree
testCase name = HUnit.testCase name
