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

module HabitOfFate.Quest.Classes where

import HabitOfFate.Prelude

import HabitOfFate.Substitution

class AllocateName m where
  allocateName ∷ Gender → m Gendered

class Monad m ⇒ ChooseFrom m where
  chooseFrom ∷ [α] → m α

class Monad m ⇒ ShuffleFrom m where
  shuffleFrom ∷ [α] → m [α]
