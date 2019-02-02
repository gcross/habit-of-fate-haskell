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

module Main where

import HabitOfFate.Prelude

import Data.Text (toLower)
import Data.Text.IO (getContents, putStrLn)
import System.Environment (getArgs)
import System.Random (getStdGen)

import HabitOfFate.Names

main ∷ IO ()
main = do
  [n_str, num_str] ← getArgs
  let n = read n_str
      num = read num_str
  model ← getContents <&> (lines >>> map toLower >>> buildModel n n)
  replicateM_ num $ runModel model >>= putStrLn
