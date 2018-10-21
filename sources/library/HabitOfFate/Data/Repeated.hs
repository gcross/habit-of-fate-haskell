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

module HabitOfFate.Data.Repeated where

import HabitOfFate.Prelude

import Data.List (unfoldr)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), dayFractionToTimeOfDay, timeOfDayToDayFraction)

nextDailyAfterPresent ∷ Rational → Rational → Rational → Rational
nextDailyAfterPresent period today deadline
  | deadline > today = deadline
  | otherwise =
      deadline
      |> (today -)
      |> (/ period)
      |> (floor ∷ Rational → Int)
      |> toRational
      |> (+ 1)
      |> (* period)
      |> (+ deadline)

previousDailies ∷ Rational → Rational → Rational → [Rational]
previousDailies period deadline next_deadline =
  unfoldr
    (
      (\x → x - period)
      >>>
      (\previous_deadline →
        if previous_deadline >= cutoff
          then Just (previous_deadline, previous_deadline)
          else Nothing
      )
    )
    next_deadline
 where
  cutoff = deadline `max` (next_deadline - 7)

localTimeToRational ∷ LocalTime → Rational
localTimeToRational (LocalTime (ModifiedJulianDay day) time_of_day) =
  toRational day + timeOfDayToDayFraction time_of_day

rationalToLocalTime ∷ Rational → LocalTime
rationalToLocalTime =
  properFraction
  >>>
  \(day, time_of_day) → LocalTime (ModifiedJulianDay day) (dayFractionToTimeOfDay time_of_day)

nextAndPreviousDailies ∷ Int → LocalTime → LocalTime → (LocalTime, [LocalTime])
nextAndPreviousDailies period today deadline =
  ( rationalToLocalTime next_deadline_rational
  , map rationalToLocalTime $ previousDailies period_rational deadline_rational next_deadline_rational
  )
 where
  period_rational = toRational period
  deadline_rational = localTimeToRational deadline
  today_rational = localTimeToRational today
  next_deadline_rational = nextDailyAfterPresent period_rational today_rational deadline_rational
