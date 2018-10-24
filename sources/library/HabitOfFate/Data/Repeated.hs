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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Repeated where

import HabitOfFate.Prelude

import Data.List (iterate)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.LocalTime (LocalTime(..), dayFractionToTimeOfDay, timeOfDayToDayFraction)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import HabitOfFate.TH

nextDailyAfterPresent ∷ Rational → Rational → Rational → Rational
nextDailyAfterPresent period today deadline =
  deadline
  |> (today -)
  |> (/ period)
  |> (floor ∷ Rational → Int)
  |> toRational
  |> (+ 1)
  |> (* period)
  |> (+ deadline)

localTimeToRational ∷ LocalTime → Rational
localTimeToRational (LocalTime (ModifiedJulianDay day) time_of_day) =
  toRational day + timeOfDayToDayFraction time_of_day

rationalToLocalTime ∷ Rational → LocalTime
rationalToLocalTime =
  properFraction
  >>>
  \(day, time_of_day) → LocalTime (ModifiedJulianDay day) (dayFractionToTimeOfDay time_of_day)

nextAndPreviousDailies ∷ Int → Int → LocalTime → LocalTime → (LocalTime, [LocalTime])
nextAndPreviousDailies days_to_keep period today deadline
  | today < deadline = (deadline, [])
  | otherwise =
      ( rationalToLocalTime next_deadline_rational
      , next_deadline_rational
        |> subtract period_rational
        |> iterate (subtract period_rational)
        |> takeWhile (>= cutoff_rational)
        |> map rationalToLocalTime
      )
 where
  period_rational = toRational period
  deadline_rational = localTimeToRational deadline
  today_rational = localTimeToRational today
  cutoff_rational = deadline_rational `max` (today_rational - toRational days_to_keep)
  next_deadline_rational = nextDailyAfterPresent period_rational today_rational deadline_rational

data DaysToRepeat = DaysToRepeat
  { _monday_ ∷ Bool
  , _tuesday_ ∷ Bool
  , _wednesday_ ∷ Bool
  , _thursday_ ∷ Bool
  , _friday_ ∷ Bool
  , _saturday_ ∷ Bool
  , _sunday_ ∷ Bool
  } deriving (Eq, Ord, Read, Show)
deriveJSON ''DaysToRepeat
makeLenses ''DaysToRepeat

newtype DaysToRepeatLens = DaysToRepeatLens { unwrapDaysToRepeatLens ∷ Lens' DaysToRepeat Bool }

days_to_repeat_lenses ∷ Vector DaysToRepeatLens
days_to_repeat_lenses = V.fromList $
  [ DaysToRepeatLens monday_
  , DaysToRepeatLens tuesday_
  , DaysToRepeatLens wednesday_
  , DaysToRepeatLens thursday_
  , DaysToRepeatLens friday_
  , DaysToRepeatLens saturday_
  , DaysToRepeatLens sunday_
  ]

instance Default DaysToRepeat where
  def = DaysToRepeat False False False False False False False

days_to_repeat_lenses_rotated_by ∷ Vector (Vector DaysToRepeatLens)
days_to_repeat_lenses_rotated_by = V.fromList
  [ let (days_next_week, rest_of_days_this_week) = V.splitAt index days_to_repeat_lenses
    in rest_of_days_this_week ⊕ days_next_week
  | index ← [0..7-1]
  ]

-- Each entry is a table corresponding to a day of the week such that the
-- element at each index in the entry (which is itself a table) corresponds to
-- the day of the week field lens for that many days in the past.
reversed_days_to_repeat_lenses_rotated_by ∷ Vector (Vector DaysToRepeatLens)
reversed_days_to_repeat_lenses_rotated_by =
  days_to_repeat_lenses_rotated_by
  |> V.splitAt 1 -- in the next two lines we rotate so that the first day is now last
  |> (\(first_day, rest_days) → rest_days ⊕ first_day)
  |> V.map V.reverse -- reverse so that index walking goes in the opposite direction;
                     -- note that now the first day is back to being first again

nextWeeklyAfterPresentOffset ∷ DaysToRepeat → Int → Maybe Int
nextWeeklyAfterPresentOffset days_to_repeat day_of_week =
  V.findIndex
    (unwrapDaysToRepeatLens >>> (days_to_repeat ^.))
    (days_to_repeat_lenses_rotated_by !
      (day_of_week
        |> (\x → x+1) -- start the search on the following day
        |> (`mod` 7)  -- wrap around if we are on Sunday
      )
    )
  <&>
  (+1) -- add 1 to the offset because we started on the following day

nextWeeklyAfterPresentOffsetWithShift ∷ Bool → DaysToRepeat → Int → Maybe Int
nextWeeklyAfterPresentOffsetWithShift should_shift days_to_repeat day_of_week
  | should_shift =
      nextWeeklyAfterPresentOffset days_to_repeat ((day_of_week-1) `mod` 7) <&> (subtract 1)
  | otherwise =
      nextWeeklyAfterPresentOffset days_to_repeat day_of_week

previousWeekliesOffsets ∷ DaysToRepeat → Int → [Int]
previousWeekliesOffsets days_to_repeat day_of_week =
  reversed_days_to_repeat_lenses_rotated_by

  -- Look up the appropriate table for this day of the week.
  |> (! day_of_week)

  -- Because we constructed the table this way, the index in the table
  -- corresponds to how far back the previous weekly is.
  |> V.findIndices (unwrapDaysToRepeatLens >>> (days_to_repeat ^.))

  -- Not necessary, but since it takes so little effort we convert to an unboxed
  -- vector because it is more efficient and the conversion is so little
  -- trouble.
  |> (\indices → UV.generate (V.length indices) (indices !))

  -- Because we are measuring distances into the past we negate them.
  |> UV.map negate

  -- These two lines construct an infinite list of offsets going infinitely far
  -- into the past. It is the responsibility of the caller to truncate this list
  -- after all of the values that they care about.
  |> iterate (UV.map $ subtract 7)
  |> concatMap UV.toList

previousWeekliesOffsetsWithShift ∷ Bool → DaysToRepeat → Int → [Int]
previousWeekliesOffsetsWithShift should_shift days_to_repeat day_of_week
  | should_shift =
      previousWeekliesOffsets days_to_repeat shifted_day_of_week
      |> fmap (+1)
      |> (\case
          [] → []
          list@(first:rest)
            | first > 0 → map (subtract 7) list
            | otherwise → list
         )
  | otherwise =
      previousWeekliesOffsets days_to_repeat day_of_week
  where
    shifted_day_of_week =
      day_of_week
      |> (+1) -- Shift to the next day.
      |> (`mod` 7) -- Wrap around.
