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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Trial where

import HabitOfFate.Prelude

import Control.Monad.Random (MonadRandom(getRandom))

import HabitOfFate.Data.Scale

tryBinomial ∷ MonadRandom m ⇒ Float → Scale → m Bool
tryBinomial probability scale = getRandom <&> (< threshold)
 where
  inverse_probability ∷ Float
  inverse_probability = 1-probability

  threshold ∷ Float
  threshold =
    case scale of
      None → 0
      VeryLow → 1 - inverse_probability**(1/3)
      Low → 1 - inverse_probability**(1/2)
      Medium → probability
      High → 1 - inverse_probability^(2 ∷ Int)
      VeryHigh → 1 - inverse_probability^(3 ∷ Int)
