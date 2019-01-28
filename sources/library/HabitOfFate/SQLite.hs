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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.SQLite where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception(..), throwM)
import Data.Bits
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

import HabitOfFate.Data.Habit
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged

encodeLocalTime ∷ LocalTime → Int64
encodeLocalTime (LocalTime (ModifiedJulianDay days) time_of_day) =
  fromIntegral (days*86400) + (time_of_day |> timeOfDayToTime |> floor)

instance ToField LocalTime where
  toField = encodeLocalTime >>> SQLInteger

decodeLocalTime ∷ Int64 → LocalTime
decodeLocalTime x =
  LocalTime
    (x |> (`div` 86400) |> fromIntegral |> ModifiedJulianDay)
    (x |> (`mod` 86400) |> fromIntegral |> timeToTimeOfDay)

instance FromField LocalTime where
  fromField field = case fieldData field of
    SQLInteger x → decodeLocalTime x |> Ok
    _ → returnError Incompatible field "local time must be number of seconds"

encodeDaysToRepeat ∷ DaysToRepeat → Int64
encodeDaysToRepeat DaysToRepeat{..} = foldl' (.|.) 0
  [ if _sunday_ then bit 0 else 0
  , if _monday_ then bit 1 else 0
  , if _tuesday_ then bit 2 else 0
  , if _wednesday_ then bit 3 else 0
  , if _thursday_ then bit 4 else 0
  , if _friday_ then bit 5 else 0
  , if _saturday_ then bit 6 else 0
  ]

instance ToField DaysToRepeat where
  toField = encodeDaysToRepeat >>> SQLInteger

decodeDaysToRepeat ∷ Int64 → DaysToRepeat
decodeDaysToRepeat x = DaysToRepeat{..}
 where
  _sunday_ = testBit x 0
  _monday_ = testBit x 1
  _tuesday_ = testBit x 2
  _wednesday_ = testBit x 3
  _thursday_ = testBit x 4
  _friday_ = testBit x 5
  _saturday_ = testBit x 6

instance ToField Scale where
  toField None = SQLNull
  toField VeryLow = SQLInteger (-2)
  toField Low = SQLInteger (-1)
  toField Medium = SQLInteger 0
  toField High = SQLInteger 1
  toField VeryHigh = SQLInteger 2

instance FromField Scale where
  fromField field = case fieldData field of
    SQLNull → Ok None
    SQLInteger (-2) → Ok VeryLow
    SQLInteger (-1) → Ok Low
    SQLInteger ( 0) → Ok Medium
    SQLInteger ( 1) → Ok High
    SQLInteger ( 2) → Ok VeryHigh
    SQLInteger ( _) → returnError ConversionFailed field "invalid numeric value for the scale"
    _ → returnError ConversionFailed field "expected null or numeric value for the scale"

instance FromField DaysToRepeat where
  fromField field = case fieldData field of
    SQLInteger x → decodeDaysToRepeat x |> Ok
    _ → returnError Incompatible field "days to repeat is encoded in a number"

createHabitFrequencyOnceTable ∷ Connection → IO ()
createHabitFrequencyOnceTable c = execute_ c "CREATE TABLE habit_frequency_once (deadline INT);"

insertHabitFrequencyOnce ∷ Connection → Maybe LocalTime → IO Int64
insertHabitFrequencyOnce c maybe_deadline = do
  execute c "INSERT INTO habit_frequency_once (deadline) VALUES (?);" $ Only maybe_deadline
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
  selectUniqueRow c "SELECT (deadline) FROM habit_frequency_once WHERE rowid = ?" rowid <&> fromOnly

createHabitFrequencyRepeatedTable ∷ Connection → IO ()
createHabitFrequencyRepeatedTable c = execute_ c "CREATE TABLE habit_frequency_repeated (days_to_keep_mode INT, days_to_keep_number INT, deadline INT, repeated_mode INT, period INT, days_to_repeat INT);"

insertHabitFrequencyRepeated ∷ Connection → DaysToKeep → LocalTime → Repeated → IO Int64
insertHabitFrequencyRepeated c days_to_keep deadline repeated = do
  let (days_to_keep_mode∷Int, days_to_keep_number) = case days_to_keep of
        KeepNumberOfDays n → (1, n)
        KeepDaysInPast n → (2, n)
      (repeated_mode∷Int, period, maybe_days_to_repeat) = case repeated of
        Daily period → (1, period, Nothing)
        Weekly period days_to_repeat → (2, period, Just days_to_repeat)
  execute c "INSERT INTO HABIT_FREQUENCY_REPEATED (days_to_keep_mode, days_to_keep_number, deadline, repeated_mode, period, days_to_repeat) VALUES (?,?,?,?,?,?)" $
    ( days_to_keep_mode
    , days_to_keep_number
    , deadline
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
      (pure $ deadline)
      (case repeated_mode of
        1 → pure $ Daily period
        2 → Weekly period <$>
              maybe
                (throwM $ InternalInconsistency "repeated mode was weekly but days to repeat was null")
                pure
                maybe_days_to_repeat
        other → throwM $ InvalidMode "repeated_mode" other
       )

instance ToField UUID where
  toField = UUID.toByteString >>> (^. strict) >>> SQLBlob

instance FromField UUID where
  fromField field = case fieldData field of
    SQLNull → Ok UUID.nil
    SQLText t → maybe
                  (returnError ConversionFailed field "bad text for UUID")
                  Ok
                  (UUID.fromText t)
    SQLBlob b → maybe
                  (returnError ConversionFailed field "bad bytes for UUID")
                  Ok
                  (Lazy.fromStrict >>> UUID.fromByteString $ b)
    _ → returnError Incompatible field "UUID must be null, text, or blob type"

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

createHabitsTable ∷ Connection → IO ()
createHabitsTable c = execute_ c "CREATE TABLE habits (habit_id BLOB, name TEXT, difficulty INT, frequency_mode INT, frequency_id INT, importance INT, last_marked INT);"

data FrequencyMode = IndefiniteMode | OnceMode | RepeatedMode
instance ToField FrequencyMode where
  toField IndefiniteMode = SQLNull
  toField OnceMode = SQLInteger 1
  toField RepeatedMode = SQLInteger 2
instance FromField FrequencyMode where
  fromField field = case fieldData field of
    SQLNull → Ok IndefiniteMode
    SQLInteger 1 → Ok OnceMode
    SQLInteger 2 → Ok RepeatedMode
    SQLInteger _ → returnError ConversionFailed field "invalid numeric value for the frequency mode"
    _ → returnError ConversionFailed field "expected null or numeric value for the frequency mode"

insertHabit ∷ Connection → UUID → Habit → IO ()
insertHabit c habit_id Habit{..} = do
  (frequency_mode, frequency_id) ← case _frequency_ of
    Indefinite → pure (IndefiniteMode, Nothing)
    Once maybe_deadline →
      insertHabitFrequencyOnce c maybe_deadline
      <&>
      (\frequency_id → (OnceMode, Just frequency_id))
    Repeated days_to_keep deadline days_to_repeat →
      insertHabitFrequencyRepeated c days_to_keep deadline days_to_repeat
      <&>
      (\frequency_id → (RepeatedMode, Just frequency_id))
  forM_ _group_membership_ $ insertHabitGroup c habit_id
  execute c "INSERT INTO habits (habit_id, name, difficulty, importance, frequency_mode, frequency_id, last_marked) VALUES (?,?,?,?,?,?,?)"
    ( habit_id
    , _name_
    , _scales_ ^. success_
    , _scales_ ^. failure_
    , frequency_mode
    , frequency_id
    , _maybe_last_marked_
    )
 
selectHabit ∷ Connection → UUID → IO Habit
selectHabit c habit_id = do
  (name, difficulty, importance, (frequency_mode), frequency_id, maybe_last_marked) ←
    selectUniqueRow c "SELECT name, difficulty, importance, frequency_mode, frequency_id, last_marked FROM habits WHERE habit_id = ?" habit_id
  Habit
    <$> pure name
    <*> pure (Tagged (Success difficulty) (Failure importance))
    <*> case (frequency_mode, frequency_id) of
          (IndefiniteMode, _) → pure Indefinite
          (OnceMode, Just id) → Once <$> selectHabitFrequencyOnce c id
          (RepeatedMode, Just id) → do
            (days_to_keep, deadline, repeated) ← selectHabitFrequencyRepeated c id
            pure $ Repeated days_to_keep deadline repeated
          -- We know this isn't indefinite mode so if there is no id something is wrong.
          (_, Nothing) → throwM $ InternalInconsistency [i|valid frequency mode, but no id given|]
    <*> (selectHabitGroups c habit_id <&> setFromList)
    <*> pure maybe_last_marked

createTables ∷ Connection → IO ()
createTables c = do
  createHabitFrequencyOnceTable c
  createHabitFrequencyRepeatedTable c
  createHabitGroupsTable c
  createHabitsTable c