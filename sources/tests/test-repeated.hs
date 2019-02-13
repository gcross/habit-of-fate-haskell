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

module Main where

import HabitOfFate.Prelude

import Data.Time.Calendar.WeekDate (fromWeekDate)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), midnight)
import qualified Data.Vector as V
import Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Positive(..))

import HabitOfFate.Data.Repeated
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions
import HabitOfFate.Testing.DayHour
import HabitOfFate.Testing.Instances

repeatDays ∷ DaysToRepeat → String
repeatDays days_to_repeat =
  intercalate "+"
    [ day_of_week_name
    | (days_to_repeat_lens_, day_of_week_name) ←
        zip (V.toList days_to_repeat_lenses) day_of_week_names
    , days_to_repeat ^# days_to_repeat_lens_
    ]
 where
  day_of_week_names ∷ [String]
  day_of_week_names = ["M", "T", "W", "Th", "F", "Sat", "Sun"]

main = doMain
  ----------------------------------------------------------------------------
  [ testGroup "nextDaily" $
  ----------------------------------------------------------------------------
    let testNextDailyCase period today deadline expected_result =
          testCase
            (printf
              "%i %s %s"
              period
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
            )
            (nextDaily period today deadline @?= expected_result)
    in
    [ testNextDailyCase 2 (dayHour 4 0) (dayHour 3 0) (dayHour 5 0)
    , testNextDailyCase 1 (dayHour 4 1) (dayHour 3 2) (dayHour 4 2)
    ]
  ----------------------------------------------------------------------------
  , testGroup "previousDailies" $
  ----------------------------------------------------------------------------
    let testPreviousDailiesCase takeDays period next_deadline expected_result =
          testCase
            (printf
              "%i %s"
              period
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" next_deadline)
            )
            (takeDays (previousDailies period next_deadline) @?= expected_result)
    in
    [ testPreviousDailiesCase (take 7) 1 (dayHour 20 2) [ dayHour d 2 | d ← [19, 18..13] ]
    , testPreviousDailiesCase (take 3) 2 (dayHour 20 2) [ dayHour d 2 | d ← [18, 16, 14] ]
    , testPreviousDailiesCase (take 3) 3 (dayHour 17 2) [ dayHour d 2 | d ← [14, 11,  8] ]
    ]
  ----------------------------------------------------------------------------
  , testGroup "nextWeeklyDay" $
  ----------------------------------------------------------------------------
    let testNextWeeklyDayCase days_to_repeat period today next_deadline =
          testCase
            (printf "%s %i %s"
              (repeatDays days_to_repeat)
              period
              (formatTime defaultTimeLocale "%y-%m-%d" today)
            )
            (nextWeeklyDay days_to_repeat period today @?= next_deadline)
    in
    [ testProperty "next_deadline >= today" $ \days_to_repeat (Positive period) today →
        nextWeeklyDay days_to_repeat period today >= today
    , testNextWeeklyDayCase
        (def & monday_ .~ True & tuesday_ .~ True)
        1
        (fromWeekDate 1 1 1)
        (fromWeekDate 1 1 2)
    , testNextWeeklyDayCase
        (def & monday_ .~ True & wednesday_ .~ True)
        1
        (fromWeekDate 1 1 2)
        (fromWeekDate 1 1 3)
    , testNextWeeklyDayCase
        (def & monday_ .~ True)
        2
        (fromWeekDate 1 1 1)
        (fromWeekDate 1 3 1)
    , testNextWeeklyDayCase
        (def & monday_ .~ True)
        3
        (fromWeekDate 1 1 1)
        (fromWeekDate 1 4 1)
    , testNextWeeklyDayCase
        (def & monday_ .~ True)
        2
        (fromWeekDate 1 1 2)
        (fromWeekDate 1 3 1)
    , testNextWeeklyDayCase
        (def & wednesday_ .~ True)
        2
        (fromWeekDate 1 1 3)
        (fromWeekDate 1 3 3)
    , testNextWeeklyDayCase
        (def & tuesday_ .~ True & wednesday_ .~ True)
        2
        (fromWeekDate 1 1 4)
        (fromWeekDate 1 3 2)
    , testNextWeeklyDayCase
        (def & tuesday_ .~ True & friday_ .~ True)
        2
        (fromWeekDate 1 1 2)
        (fromWeekDate 1 1 5)
    , testNextWeeklyDayCase
        (def & monday_ .~ True & wednesday_ .~ True)
        2
        (fromWeekDate 1 5 5)
        (fromWeekDate 1 7 1)
    ]
  ----------------------------------------------------------------------------
  , testGroup "nextWeeklies" $
  ----------------------------------------------------------------------------
    let testNextWeeklyCase days_to_repeat period today deadline result =
          testCase
            (printf "%s %i %s %s"
              (repeatDays days_to_repeat)
              period
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
            )
            (nextWeekly period days_to_repeat today deadline @?= result)
    in
    [ testNextWeeklyCase
        (def & monday_ .~ True & wednesday_ .~ True)
        1
        (LocalTime (fromWeekDate 1 1 2) midnight)
        (LocalTime (fromWeekDate 1 1 1) midnight)
        (LocalTime (fromWeekDate 1 1 3) midnight)
    , testNextWeeklyCase
        (def & monday_ .~ True & wednesday_ .~ True)
        2
        (LocalTime (fromWeekDate 1 5 5) midnight)
        (LocalTime (fromWeekDate 1 1 1) midnight)
        (LocalTime (fromWeekDate 1 7 1) midnight)
    , testNextWeeklyCase
        (def & monday_ .~ True & wednesday_ .~ True)
        1
        (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
        (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))
        (LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0))
    , testNextWeeklyCase
        (def & monday_ .~ True & wednesday_ .~ True)
        1
        (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))
        (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
        (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 1 0))
    ]
  ----------------------------------------------------------------------------
  , testGroup "previousWeekliesBeforePresent" $
  ----------------------------------------------------------------------------
    let testPreviousWeekliesCase takeDays days_to_repeat period next_deadline previous_deadlines =
          testCase
            (printf "%s %i %s"
              (repeatDays days_to_repeat)
              period
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" next_deadline)
            )
            (
              takeDays (previousWeeklies period days_to_repeat next_deadline)
                @?= previous_deadlines
            )
    in
    [ testPreviousWeekliesCase
        (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
        (def & monday_ .~ True & tuesday_ .~ True)
        1
        (LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0))
        [ LocalTime (fromWeekDate 1 1 2) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
        ]
    , testPreviousWeekliesCase
        (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
        (def & monday_ .~ True)
        1
        (LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0))
        [ LocalTime (fromWeekDate 1 2 1) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
        ]
    , testPreviousWeekliesCase
        (takeWhile (>= (LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0))))
        (def & monday_ .~ True & thursday_ .~ True)
        2
        (LocalTime (fromWeekDate 1 5 4) (TimeOfDay 1 0 0))
        [ LocalTime (fromWeekDate 1 5 1) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 3 4) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 1 4) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 1 1) (TimeOfDay 1 0 0)
        ]
    , testPreviousWeekliesCase
        (take 5)
        (def & monday_ .~ True & wednesday_ .~ True)
        2
        (LocalTime (fromWeekDate 1 5 5) (TimeOfDay 1 0 0))
        [ LocalTime (fromWeekDate 1 5 3) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 5 1) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 3 3) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 3 1) (TimeOfDay 1 0 0)
        , LocalTime (fromWeekDate 1 1 3) (TimeOfDay 1 0 0)
        ]
    ]
  ----------------------------------------------------------------------------
  , testGroup "takeDays" $
  ----------------------------------------------------------------------------
    let testTakePreviousDeadlinesCase days_to_keep today deadline previous_deadlines expected_result =
          testCase
            (printf
              "(%s) %s %s"
              (show days_to_keep)
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" today)
              (formatTime defaultTimeLocale "%y-%m-%d.%Hh" deadline)
            )
            (takePreviousDeadlines days_to_keep today deadline previous_deadlines @?= expected_result)
    in
    [ testTakePreviousDeadlinesCase (KeepNumberOfDays 1) (day 3) (day 0) [day 2, day 1] [day 2]
    , testTakePreviousDeadlinesCase (KeepNumberOfDays 3) (day 3) (day 0) [day 2, day 1] [day 2, day 1]
    , testTakePreviousDeadlinesCase (KeepDaysInPast 1) (day 3) (day 0) [day 2, day 1] [day 2]
    , testTakePreviousDeadlinesCase (KeepDaysInPast 3) (day 3) (day 0) [day 2, day 1] [day 2, day 1]
    ]
  ]
