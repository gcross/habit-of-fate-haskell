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
import Data.Time.Calendar (Day(ModifiedJulianDay,toModifiedJulianDay), addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
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

data DaysToKeep = KeepDaysInPast Int | KeepNumberOfDays Int deriving (Eq, Ord, Read, Show)

nextAndPreviousDailies ∷ Int → LocalTime → LocalTime → (LocalTime, [LocalTime])
nextAndPreviousDailies period today deadline
  | today < deadline = (deadline, [])
  | otherwise =
      ( rationalToLocalTime next_deadline_rational
      , next_deadline_rational
        |> subtract period_rational
        |> iterate (subtract period_rational)
        |> map rationalToLocalTime
      )
 where
  period_rational = toRational period
  deadline_rational = localTimeToRational deadline
  today_rational = localTimeToRational today
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

checkDayRepeated ∷ DaysToRepeat → DaysToRepeatLens → Bool
checkDayRepeated = (^.) >>> (unwrapDaysToRepeatLens >>>)

addDaysToLocalTime ∷ Integer → LocalTime → LocalTime
addDaysToLocalTime number_of_days (LocalTime day time) = LocalTime (addDays number_of_days day) time

nextWeeklyAfterPresent ∷ DaysToRepeat → Int → Day → Day
nextWeeklyAfterPresent days_to_repeat period today =
  addDays days_to_add today
 where
  (_, deadline_week, day_of_week_1_based) = toWeekDate today
  day_of_week = day_of_week_1_based - 1
  day_of_week_of_first_deadline =
    fromMaybe
      (error "no days of the week are repeated")
      (V.findIndex (checkDayRepeated days_to_repeat) days_to_repeat_lenses)
  days_to_add =
    days_to_repeat_lenses
    |> V.drop (day_of_week+1)
    |> V.findIndex (checkDayRepeated days_to_repeat)
    |> maybe
        (period*7 - day_of_week + day_of_week_of_first_deadline)
        (+1) -- the index counts the number of days to the next repeated day minus 1
    |> toInteger

previousWeekliesBeforePresent ∷ DaysToRepeat → Int → LocalTime → [LocalTime]
previousWeekliesBeforePresent days_to_repeat period next_deadline =
  map
    (\day_int → next_deadline { localDay = ModifiedJulianDay day_int })
    (next_deadline_week_day_ints ⊕ previous_weeks_day_ints)
 where
  period_as_integer = toInteger period
  next_deadline_day = localDay next_deadline
  next_deadline_int = next_deadline_day |> toModifiedJulianDay |> fromInteger
  (_, _, day_of_week_1_based) = toWeekDate next_deadline_day
  day_of_week = toInteger (day_of_week_1_based-1)
  days_to_repeat_as_offsets =
    days_to_repeat
    |> checkDayRepeated
    |> flip V.findIndices days_to_repeat_lenses
    |> V.reverse
    |> V.map toInteger
  next_deadline_week_day_ints =
    days_to_repeat_as_offsets
    |> V.dropWhile (>= day_of_week)
    |> V.map (+ (next_deadline_int - day_of_week))
    |> V.toList
  previous_weeks_day_ints =
    next_deadline_int
    -- Move to the start of the week as we already have the current week
    -- covered.
    |> subtract day_of_week

    -- Jump period weeks into the past to move to the start of the previous week
    -- with repeated days.
    |> subtract (period_as_integer*7)

    -- Generate an infinite list of the starts of the week of all weeks in the
    -- past with repeated days.
    |> iterate (subtract (period_as_integer*7))

    -- For each of these weeks, add the offsets of all the repeated days to the
    -- start of the week, and then concantenate all of the resulting lists.
    |> concatMap ((+) >>> flip map (V.toList days_to_repeat_as_offsets))


nextAndPreviousWeeklies ∷ DaysToRepeat → Int → LocalTime → LocalTime → (LocalTime, [LocalTime])
nextAndPreviousWeeklies days_to_repeat period today deadline
  | today < deadline = (deadline, [])
  | otherwise =
      ( next_deadline
      , previousWeekliesBeforePresent days_to_repeat period next_deadline
      )
 where
  (_, _, today_day_of_week_1_based) = toWeekDate $ localDay today
  today_day_of_week = today_day_of_week_1_based - 1
  next_deadline
    -- If we are on a day to be repeated and the repeat time is later in the day
    -- then the next deadline is *today* at that time.
    | days_to_repeat ^. (unwrapDaysToRepeatLens $ days_to_repeat_lenses ! today_day_of_week)
        && localTimeOfDay today < localTimeOfDay deadline
          = today { localTimeOfDay = localTimeOfDay deadline }

    -- Otherwise, we can assume that the next deadline is a day after this one, which
    -- is the case handled by nextWeeklyAfterPresent.
    | otherwise = deadline { localDay = nextWeeklyAfterPresent days_to_repeat period (localDay today) }

data Repeated = Daily Int | Weekly Int DaysToRepeat deriving (Eq, Ord, Read, Show)

nextAndPreviousDeadlines ∷ DaysToKeep → Repeated → LocalTime → LocalTime → (LocalTime, [LocalTime])
nextAndPreviousDeadlines days_to_keep repeated today deadline =
  (next_deadline, previous_deadlines)
 where
  (next_deadline, previous_deadlines_stream) =
    (case repeated of
      Daily period → nextAndPreviousDailies period
      Weekly period days_to_repeat → nextAndPreviousWeeklies days_to_repeat period
    ) today deadline
  previous_deadlines =
    previous_deadlines_stream
      |> case days_to_keep of
          KeepDaysInPast days_in_past →
            takeWhile (>= addDaysToLocalTime (-toInteger days_in_past) today)
          KeepNumberOfDays number_of_days →
            take number_of_days
      |> takeWhile (>= deadline)
