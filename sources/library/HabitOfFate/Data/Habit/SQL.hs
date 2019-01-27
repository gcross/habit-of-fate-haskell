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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Habit.SQL where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception(..), throwM)
import Data.Int (Int64)
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (LocalTime(..), timeToTimeOfDay, timeOfDayToTime)
import Database.SQLite.Simple

encodeLocalTime ∷ LocalTime → Int
encodeLocalTime (LocalTime (ModifiedJulianDay days) time_of_day) =
  fromIntegral (days*86400) + (time_of_day |> timeOfDayToTime |> floor)

decodeLocalTime ∷ Int → LocalTime
decodeLocalTime x =
  LocalTime
    (x |> (`div` 86400) |> fromIntegral |> ModifiedJulianDay)
    (x |> (`mod` 86400) |> fromIntegral |> timeToTimeOfDay)

createHabitFrequencyOnceTable ∷ Connection → IO ()
createHabitFrequencyOnceTable c = execute_ c "CREATE TABLE habit_frequency_once (deadline INT);"

insertHabitFrequencyOnce ∷ Connection → Maybe LocalTime → IO Int64
insertHabitFrequencyOnce c maybe_deadline = do
  execute c "INSERT INTO habit_frequency_once (deadline) VALUES (?);" $ Only (encodeLocalTime <$> maybe_deadline)
  lastInsertRowId c

data NumberOfRowsShouldHaveBeenOne = NumberOfRowsShouldHaveBeenOne String Int deriving (Show)
instance Exception NumberOfRowsShouldHaveBeenOne where
  displayException (NumberOfRowsShouldHaveBeenOne table count) = [i|wrong number of rows for #{table}, not #{count}|]

selectHabitFrequencyOnce ∷ Connection → Int64 → IO (Maybe LocalTime)
selectHabitFrequencyOnce c rowid = do
  results ∷ [Only (Maybe Int)] ← query c "SELECT (deadline) FROM habit_frequency_once WHERE rowid = ?" (Only rowid)
  case results of
    [(Only maybe_deadline_encoded)] → pure $ decodeLocalTime <$> maybe_deadline_encoded
    rows → throwM $ NumberOfRowsShouldHaveBeenOne "once" (length rows)
