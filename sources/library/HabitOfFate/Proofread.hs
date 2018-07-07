{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Proofread where

import HabitOfFate.Prelude

import qualified System.IO as IO

printLine ∷ Char → IO ()
printLine c = IO.putStrLn $ replicate 80 c

printCentered ∷ Char → String → IO ()
printCentered c header =
  let dash_count = 80 - 2 - length header
      right_dash_count = dash_count `div` 2
      left_dash_count = dash_count - right_dash_count
  in IO.putStrLn $ concat $
      [ replicate left_dash_count c
      , " "
      , header
      , " "
      , replicate right_dash_count c
      ]

printBanner ∷ Char → String → IO ()
printBanner c header = do
  printLine c
  printCentered c header
  printLine c

printQuestBanner ∷ String → IO ()
printQuestBanner = printBanner '='

printEventBanner ∷ String → IO ()
printEventBanner = printBanner '-'

newline ∷ IO ()
newline = IO.putStrLn ""
