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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Test.Tasty (defaultMain)

import HabitOfFate.API
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged

import HabitOfFate.Testing.Assertions
import HabitOfFate.Testing.Data
import HabitOfFate.Testing.Server

main ∷ IO ()
main = defaultMain $ apiTestCase "Run the game a large number of times" $ do
  createHabit test_habit_id $ Habit "name" (Tagged (Success VeryHigh) (Failure VeryHigh)) Indefinite [] Nothing
  void $ markHabits [(test_habit_id, def & success_ .~ 10000 & failure_ .~ 10000)]
  getMarks >>= (@?= Tagged (Success $ replicate 10000 VeryHigh) (Failure $ replicate 10000 VeryHigh))
  replicateM_ 100000 runGame
  getMarks >>= (@?= Tagged (Success []) (Failure []))
