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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Repeated where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), withArray, withObject, withText)
import Data.Aeson.Types (Parser)
import Data.List (iterate)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Calendar (Day(ModifiedJulianDay,toModifiedJulianDay), addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (LocalTime(..), dayFractionToTimeOfDay, timeOfDayToDayFraction)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import HabitOfFate.JSON
import HabitOfFate.TH

localTimeToRational ∷ LocalTime → Rational
localTimeToRational (LocalTime (ModifiedJulianDay day) time_of_day) =
  toRational day + timeOfDayToDayFraction time_of_day

rationalToLocalTime ∷ Rational → LocalTime
rationalToLocalTime =
  properFraction
  >>>
  \(day, time_of_day) → LocalTime (ModifiedJulianDay day) (dayFractionToTimeOfDay time_of_day)

nextDaily ∷ Int → LocalTime → LocalTime → LocalTime
nextDaily period today deadline =
  deadline_rational
  |> (today_rational -)
  |> (/ period_rational)
  |> (floor ∷ Rational → Int)
  |> toRational
  |> (+ 1)
  |> (* period_rational)
  |> (+ deadline_rational)
  |> rationalToLocalTime
 where
  period_rational = toRational period
  deadline_rational = localTimeToRational deadline
  today_rational = localTimeToRational today

addDaysToLocalTime ∷ Integral α ⇒ α → LocalTime → LocalTime
addDaysToLocalTime number_of_days (LocalTime day time) = LocalTime (addDays (toInteger number_of_days) day) time

previousDailies ∷ Int → LocalTime → [LocalTime]
previousDailies period = subtractPeriod >>> iterate (subtractPeriod)
 where
  subtractPeriod = addDaysToLocalTime (-period)

data DaysToRepeat = DaysToRepeat
  { _monday_ ∷ !Bool
  , _tuesday_ ∷ !Bool
  , _wednesday_ ∷ !Bool
  , _thursday_ ∷ !Bool
  , _friday_ ∷ !Bool
  , _saturday_ ∷ !Bool
  , _sunday_ ∷ !Bool
  } deriving (Eq, Ord, Read, Show)
makeLenses ''DaysToRepeat

instance ToJSON DaysToRepeat where
  toJSON x = Array $ V.fromList
    [ String (Lazy.toStrict weekday_name)
    | Weekday{..} ← weekdays
    , x ^# weekday_lens_
    ]

instance FromJSON DaysToRepeat where
  parseJSON = withArray "days to repeat must be an array" $
    traverse
      (withText "day name must be text" $
        Lazy.fromStrict
        >>>
        (\day_name →
          maybe
            (fail [i|#{day_name} is not a recognized weekday name|])
            ((& weekday_lens_) >>> pure)
            (find ((& weekday_name) >>> (== day_name)) weekdays)
        )
      )
    >>>
    (<&> foldl' (\x lens_ → x & lens_ #~ True) def)

instance Semigroup DaysToRepeat where
  (DaysToRepeat xm xt xw xth xf xs xsu) <> (DaysToRepeat ym yt yw yth yf ys ysu) =
    DaysToRepeat (xm || ym) (xt || yt) (xw || yw) (xth || yth) (xf || yf) (xs || ys) (xsu || ysu)

instance NFData DaysToRepeat where rnf !_ = ()

-- Note: This list is a little different from weekdays in that it starts on
--       Monday because the time library does.
days_to_repeat_lenses ∷ Vector (ALens' DaysToRepeat Bool)
days_to_repeat_lenses = V.fromList $
  [ monday_
  , tuesday_
  , wednesday_
  , thursday_
  , friday_
  , saturday_
  , sunday_
  ]

data Weekday = Weekday
  { weekday_first_letter ∷ !Text
  , weekday_abbrev ∷ !Text
  , weekday_name ∷ !Lazy.Text
  , weekday_lens_ ∷ !(ALens' DaysToRepeat Bool)
  }

weekdays ∷ [Weekday]
weekdays =
  [ Weekday "S" "Sun" "sunday" sunday_
  , Weekday "M" "Mon" "monday"  monday_
  , Weekday "T" "Tue" "tuesday"  tuesday_
  , Weekday "W" "Wed" "wednesday" wednesday_
  , Weekday "T" "Thu" "thursday" thursday_
  , Weekday "F" "Fri" "friday" friday_
  , Weekday "S" "Sat" "saturday" saturday_
  ]

weekday_abbrevs ∷ HashSet Text
weekday_abbrevs = setFromList [weekday_abbrev | Weekday{..} ← weekdays]

instance Default DaysToRepeat where
  def = DaysToRepeat False False False False False False False

nextWeeklyDay ∷ DaysToRepeat → Int → Day → Day
nextWeeklyDay days_to_repeat period today =
  addDays days_to_add today
 where
  (_, _, day_of_week_1_based) = toWeekDate today
  day_of_week = day_of_week_1_based - 1
  day_of_week_of_first_deadline =
    fromMaybe
      (error "no days of the week are repeated")
      (V.findIndex (days_to_repeat ^#) days_to_repeat_lenses)
  days_to_add =
    days_to_repeat_lenses
    |> V.drop (day_of_week+1)
    |> V.findIndex (days_to_repeat ^#)
    |> maybe
        (period*7 - day_of_week + day_of_week_of_first_deadline)
        (+1) -- the index counts the number of days to the next repeated day minus 1
    |> toInteger

nextWeekly ∷ Int → DaysToRepeat → LocalTime → LocalTime → LocalTime
nextWeekly period days_to_repeat today deadline
  -- If we are on a day to be repeated and the repeat time is later in the day
  -- then the next deadline is *today* at that time.
  | days_to_repeat ^# (days_to_repeat_lenses ! today_day_of_week)
      && localTimeOfDay today < localTimeOfDay deadline
        = today { localTimeOfDay = localTimeOfDay deadline }

  -- Otherwise, we can assume that the next deadline is a day after this one, which
  -- is the case handled by nextWeeklyAfterPresent.
  | otherwise = deadline { localDay = nextWeeklyDay days_to_repeat period (localDay today) }
 where
  (_, _, today_day_of_week_1_based) = toWeekDate $ localDay today
  today_day_of_week = today_day_of_week_1_based - 1

previousWeeklies ∷ Int → DaysToRepeat → LocalTime → [LocalTime]
previousWeeklies period days_to_repeat next_deadline =
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
    |> (^#)
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

data DaysToKeep = KeepDaysInPast !Int | KeepNumberOfDays !Int deriving (Eq, Ord, Read, Show)

instance ToJSON DaysToKeep where
  toJSON (KeepDaysInPast n) = runJSONBuilder $ do
    writeTextField "mode" "keep days in past"
    writeField "number" n
  toJSON (KeepNumberOfDays n) = runJSONBuilder $ do
    writeTextField "mode" "keep number of days"
    writeField "number" n

instance FromJSON DaysToKeep where
  parseJSON = withObject "expecting object-shaped value for days to keep" $ \o →
    (o .: "mode" ∷ Parser Text)
    >>=
    \case
      "keep days in past" → KeepDaysInPast <$> (o .: "number")
      "keep number of days" → KeepNumberOfDays <$> (o .: "number")
      x → fail [i|"#{x}" is an invalid value for the mode of the days to keep|]

instance NFData DaysToKeep where rnf !_ = ()

data Repeated = Daily !Int | Weekly !Int !DaysToRepeat deriving (Eq, Ord, Read, Show)
deriveJSON ''Repeated

instance NFData Repeated where rnf !_ = ()

nextDeadline ∷ Repeated → LocalTime → LocalTime → LocalTime
nextDeadline (Daily period) = nextDaily period
nextDeadline (Weekly period days_to_repeat) = nextWeekly period days_to_repeat

previousDeadlines ∷ Repeated → LocalTime → [LocalTime]
previousDeadlines (Daily period) = previousDailies period
previousDeadlines (Weekly period days_to_repeat) = previousWeeklies period days_to_repeat

takePreviousDeadlines ∷ DaysToKeep → LocalTime → LocalTime → [LocalTime] → [LocalTime]
takePreviousDeadlines days_to_keep today deadline =
  (case days_to_keep of
    KeepDaysInPast days_in_past → takeWhile (>= addDaysToLocalTime (toInteger (-days_in_past)) today)
    KeepNumberOfDays number_of_days → take number_of_days
  )
  >>>
  takeWhile (>= deadline)
