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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Testing (TestTree, doMain, testGroup, dontTestGroup, stackString) where

import HabitOfFate.Prelude

import Data.CallStack (HasCallStack, SrcLoc(..), callStack)
import Test.Tasty (TestTree, defaultMain, testGroup)

stackString ∷ HasCallStack ⇒ String
stackString = case reverse callStack of
  [] → "No stack."
  (function_name, _):rest →
    intercalate "\n" $
    "Stack:"
    :
    (
      mapAccumL
        (\this_function_name (next_function_name, SrcLoc{..}) →
          (next_function_name, [i|    #{srcLocFile}:#{srcLocEndLine}:#{this_function_name}|])
        )
        function_name
        rest
      &
      (snd >>> reverse)
    )

dontTestGroup ∷ String → [TestTree] → TestTree
dontTestGroup name _ = testGroup name []

doMain ∷ HasCallStack ⇒ [TestTree] → IO ()
doMain = testGroup "All Tests" >>> defaultMain
