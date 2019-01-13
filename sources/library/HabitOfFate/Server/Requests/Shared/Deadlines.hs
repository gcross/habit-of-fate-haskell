{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.Deadlines where

import HabitOfFate.Prelude

import Data.Time.LocalTime (LocalTime)
import Data.UUID (UUID)

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Server.Transaction

getDeadlines ∷ Transaction [(UUID, Habit, [LocalTime])]
getDeadlines = do
  current_time ← getCurrentTimeAsLocalTime
  use (habits_ . items_list_) <&>
    mapMaybe (\(habit_id, habit) →
      case previousHabitDeadlines habit current_time of
        [] → Nothing
        deadlines → Just (habit_id, habit, deadlines)
    )
