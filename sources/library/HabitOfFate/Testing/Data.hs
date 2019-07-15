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

module HabitOfFate.Testing.Data where

import HabitOfFate.Prelude

import Data.UUID

import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged

import HabitOfFate.Testing.DayHour

test_account_id ∷ Text
test_account_id = "test"

test_account_id_1 ∷ Text
test_account_id_1 = "test-1"

test_account_id_2 ∷ Text
test_account_id_2 = "test-2"

test_group_id ∷ UUID
test_group_id = read "f5ccdfde-1776-483c-9140-385e9e75e31d"

test_group, test_group_2 ∷ Group
test_group = "group"
test_group_2 = "grouper"

test_habit = Habit "name" (Tagged (Success Low) (Failure Medium)) Indefinite [] Nothing
test_habit_1 = Habit "name1" (Tagged (Success VeryLow) (Failure Medium)) Indefinite [] Nothing
test_habit_2 = Habit "test" (Tagged (Success Medium) (Failure VeryHigh)) Indefinite [] Nothing
test_habit_once = Habit "once" (Tagged (Success Medium) (Failure Medium)) (Once $ Just $ day 0) [] Nothing
test_habit_daily = def & name_ .~ "daily" & frequency_ .~ (Repeated (KeepNumberOfDays 2) (dayHour 2 3) (Daily 2))
test_habit_weekly = def & name_ .~ "daily" & frequency_ .~ (Repeated (KeepDaysInPast 4) (dayHour 3 2) (Weekly 1 (def & tuesday_ .~ True & thursday_ .~ True)))
test_habit_with_last_marked = def & name_ .~ "test" & maybe_last_marked_ .~ (Just $ dayHour 1 2)
test_habit_group = Habit "group" (Tagged (Success High) (Failure Low)) Indefinite [test_group_id] Nothing

test_habit_id = read "95bef3cf-9031-4f64-8458-884aa6781563" ∷ UUID
test_habit_id_2 = read "9e801a68-4288-4a23-8779-aa68f94991f9" ∷ UUID
test_habit_id_once = read "7dbafaf9-560a-4ac4-b6bb-b64c647e387d" ∷ UUID
test_habit_id_with_last_marked = read "7ada06ff-ccf5-4c68-83df-e54999cc42b3" ∷ UUID
test_habit_id_group = read "74d6b013-df62-4989-8ce8-b0e0af3e29d3" ∷ UUID
