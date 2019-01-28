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
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as Lazy
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField

import HabitOfFate.Data.Repeated

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

selectUniqueRow ∷ ∀ id r. (ToField id, FromRow r) ⇒ Connection → Query → id → IO r
selectUniqueRow c q id =
  (query c q (Only id) ∷ IO [r])
  >>=
  \case
    [row] → pure row
    rows → throwM $ NumberOfRowsShouldHaveBeenOne "once" (length rows)

selectHabitFrequencyOnce ∷ Connection → Int64 → IO (Maybe LocalTime)
selectHabitFrequencyOnce c rowid =
  selectUniqueRow c "SELECT (deadline) FROM habit_frequency_once WHERE rowid = ?" rowid
  <&>
  (fromOnly >>> fmap decodeLocalTime)

createHabitFrequencyRepeatedTable ∷ Connection → IO ()
createHabitFrequencyRepeatedTable c = execute_ c "CREATE TABLE habit_frequency_repeated (days_to_keep_mode INT, days_to_keep_number INT, deadline INT, repeated_mode INT, period INT, days_to_repeat INT);"

insertHabitFrequencyRepeated ∷ Connection → DaysToKeep → LocalTime → Repeated → IO Int64
insertHabitFrequencyRepeated c days_to_keep deadline repeated = do
  let (days_to_keep_mode∷Int, days_to_keep_number) = case days_to_keep of
        KeepNumberOfDays n → (1, n)
        KeepDaysInPast n → (2, n)
      (repeated_mode∷Int, period, maybe_days_to_repeat) = case repeated of
        Daily period → (1, period, Nothing)
        Weekly period days_to_repeat → (2, period, Just (encodeDaysToRepeat days_to_repeat))
  execute c "INSERT INTO HABIT_FREQUENCY_REPEATED (days_to_keep_mode, days_to_keep_number, deadline, repeated_mode, period, days_to_repeat) VALUES (?,?,?,?,?,?)" $
    ( days_to_keep_mode
    , days_to_keep_number
    , encodeLocalTime deadline
    , repeated_mode
    , period
    , maybe_days_to_repeat
    )
  lastInsertRowId c

data InvalidMode = InvalidMode String Int deriving (Show)
instance Exception InvalidMode where
  displayException (InvalidMode name mode) = [i|invalid mode #{mode} for #{name}|]

data InternalInconsistency = InternalInconsistency String deriving (Show)
instance Exception InternalInconsistency where
  displayException (InternalInconsistency message) = [i|internal inconsistency: #{message}|]

selectHabitFrequencyRepeated ∷ Connection → Int64 → IO (DaysToKeep, LocalTime, Repeated)
selectHabitFrequencyRepeated c rowid =
  selectUniqueRow c "SELECT days_to_keep_mode, days_to_keep_number, deadline, repeated_mode, period, days_to_repeat FROM habit_frequency_repeated WHERE rowid = ?" rowid
  >>=
  \(days_to_keep_mode, days_to_keep_number, deadline, repeated_mode, period, maybe_days_to_repeat) →
    liftA3 (,,)
      (case days_to_keep_mode of
        1 → pure $ KeepNumberOfDays days_to_keep_number
        2 → pure $ KeepDaysInPast days_to_keep_number
        other → throwM $ InvalidMode "days_to_keep" other
      )
      (pure $ decodeLocalTime deadline)
      (case repeated_mode of
        1 → pure $ Daily period
        2 → Weekly period <$>
              maybe
                (throwM $ InternalInconsistency "repeated mode was weekly but days to repeat was null")
                (decodeDaysToRepeat >>> pure)
                maybe_days_to_repeat
        other → throwM $ InvalidMode "repeated_mode" other
       )

instance ToField UUID where
  toField = UUID.toByteString >>> (^. strict) >>> SQLBlob

data InvalidFormatForUUID = InvalidFormatForUUID String deriving (Show)
instance Exception InvalidFormatForUUID where
  displayException (InvalidFormatForUUID message) =
    [i|Invalid format for UUID: #{message}|]

err ∷ String → Ok α
err message = Errors [toException $ InvalidFormatForUUID message]

instance FromField UUID where
  fromField field = case fieldData field of
    SQLInteger _ → err "8-bit integer"
    SQLFloat _ → err "floating-point"
    SQLText t → maybe (err "empty text") Ok (UUID.fromText t)
    SQLBlob b → maybe (err "empty blob") Ok (Lazy.fromStrict >>> UUID.fromByteString $ b)
    SQLNull → Ok UUID.nil

createHabitGroupsTable ∷ Connection → IO ()
createHabitGroupsTable c = execute_ c "CREATE TABLE habit_groups (habit_id BLOB, group_id BLOB);"

insertHabitGroup ∷ Connection → UUID → UUID → IO ()
insertHabitGroup c habit_id group_id =
  execute c "INSERT INTO habit_groups (habit_id, group_id) VALUES (?,?)" (habit_id, group_id)

selectHabitGroups ∷ Connection → UUID → IO [UUID]
selectHabitGroups c habit_id =
  query c "SELECT group_id FROM habit_groups WHERE habit_id = ?;" (Only habit_id)
  <&>
  map fromOnly
