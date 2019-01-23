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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.InputHabit where

import HabitOfFate.Prelude

import qualified Data.ByteString.Char8 as BS8
import Data.Time (LocalTime)
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Web.Scotty (Parsable(..))

import HabitOfFate.Data.Habit
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale

data InputFrequency = InputIndefinite | InputOnce | InputRepeated deriving (Eq,Ord,Read,Show)
instance Default InputFrequency where def = InputIndefinite

data InputRepeated = InputDaily | InputWeekly deriving (Eq,Ord,Read,Show)
instance Default InputRepeated where def = InputDaily

data InputDaysToKeepMode = InputKeepDaysInPast | InputKeepNumberOfDays deriving (Eq,Ord,Read,Show)
instance Default InputDaysToKeepMode where def = InputKeepDaysInPast

instance Parsable LocalTime where
  parseParam time =
    time
      |> unpack
      |> parseLocalTimeFromExchange
      |> maybe
          (Left $ pack [i|Unable to parse local time: "#{time}"|])
          Right

data InputHabit = InputHabit
 { _input_name_ ∷ Maybe Text
 , _input_difficulty_ ∷ Maybe Scale
 , _input_importance_ ∷ Maybe Scale
 , _input_frequency_ ∷ Maybe InputFrequency
 , _input_once_has_deadline_ ∷ Bool
 , _input_repeated_ ∷ Maybe InputRepeated
 , _input_daily_period_ ∷ Maybe Int
 , _input_weekly_period_ ∷ Maybe Int
 , _input_days_to_repeat_ ∷ DaysToRepeat
 , _input_days_to_keep_ ∷ Maybe Int
 , _input_days_to_keep_mode_ ∷ Maybe InputDaysToKeepMode
 , _input_next_deadline_ ∷ Maybe String
 , _input_group_membership_ ∷ HashSet UUID
 , _input_maybe_last_marked_ ∷ Maybe LocalTime
 } deriving (Eq,Ord,Read,Show)
makeLenses ''InputHabit

instance Default InputHabit where
  def = InputHabit{..}
   where
    _input_name_ = Just ""
    _input_difficulty_ = Just def
    _input_importance_ = Just def
    _input_frequency_ = Just def
    _input_once_has_deadline_ = False
    _input_repeated_ = Just InputDaily
    _input_daily_period_ = Just 1
    _input_weekly_period_ = Just 1
    _input_days_to_repeat_ = def
    _input_days_to_keep_ = Just 3
    _input_days_to_keep_mode_ = Just InputKeepDaysInPast
    _input_next_deadline_ = def
    _input_group_membership_ = mempty
    _input_maybe_last_marked_ = Nothing

instance Semigroup InputHabit where
  x <> y = InputHabit{..}
   where
    lastMaybeUsing lens_ = getLast $ (Last $ x ^. lens_) ⊕ (Last $ x ^. lens_)

    _input_name_ = lastMaybeUsing input_name_
    _input_difficulty_ = lastMaybeUsing input_difficulty_
    _input_importance_ = lastMaybeUsing input_importance_
    _input_frequency_ = lastMaybeUsing input_frequency_
    _input_once_has_deadline_ = ((||) `on` (^. input_once_has_deadline_)) x y
    _input_repeated_ = lastMaybeUsing input_repeated_
    _input_daily_period_ = lastMaybeUsing input_daily_period_
    _input_weekly_period_ = lastMaybeUsing input_weekly_period_
    _input_days_to_repeat_ = ((⊕) `on` (^. input_days_to_repeat_)) x y
    _input_days_to_keep_ = lastMaybeUsing input_days_to_keep_
    _input_days_to_keep_mode_ = lastMaybeUsing input_days_to_keep_mode_
    _input_next_deadline_ = lastMaybeUsing input_next_deadline_
    _input_group_membership_ = ((⊕) `on` (^. input_group_membership_)) x y
    _input_maybe_last_marked_ = lastMaybeUsing input_maybe_last_marked_

instance Monoid InputHabit where
  mempty = InputHabit{..}
   where
    _input_name_ = Nothing
    _input_difficulty_ = Nothing
    _input_importance_ = Nothing
    _input_frequency_ = Nothing
    _input_once_has_deadline_ = False
    _input_repeated_ = Nothing
    _input_daily_period_ = Nothing
    _input_weekly_period_ = Nothing
    _input_days_to_repeat_ = def
    _input_days_to_keep_ = Nothing
    _input_days_to_keep_mode_ = Nothing
    _input_next_deadline_ = Nothing
    _input_group_membership_ = mempty
    _input_maybe_last_marked_ = Nothing

habitToInputHabit ∷ Habit → InputHabit
habitToInputHabit habit@Habit{..} = flip execState mempty $ do
  input_name_ .= Just _name_
  input_difficulty_ .= Just (habit ^. difficulty_)
  input_importance_ .= Just (habit ^. importance_)
  case _frequency_ of
    Indefinite → input_frequency_ .= Just InputIndefinite
    Once maybe_next_deadline → do
      input_frequency_ .= Just InputOnce
      case maybe_next_deadline of
        Nothing → pure ()
        Just next_deadline → do
          input_once_has_deadline_ .= True
          input_next_deadline_ .= Just (formatLocalTimeForExchange next_deadline)
    Repeated days_to_keep next_deadline repeated → do
      input_frequency_ .= Just InputRepeated
      case days_to_keep of
        KeepDaysInPast n → do
          input_days_to_keep_ .= Just n
          input_days_to_keep_mode_ .= Just InputKeepDaysInPast
        KeepNumberOfDays n → do
          input_days_to_keep_ .= Just n
          input_days_to_keep_mode_ .= Just InputKeepNumberOfDays
      input_next_deadline_ .= Just (formatLocalTimeForExchange next_deadline)
      case repeated of
        Daily period → do
          input_repeated_ .= Just InputDaily
          input_daily_period_ .= Just period
        Weekly period days_to_repeat → do
          input_repeated_ .= Just InputWeekly
          input_weekly_period_ .= Just period
          input_days_to_repeat_ .= days_to_repeat
  input_group_membership_ .= _group_membership_
  input_maybe_last_marked_ .= _maybe_last_marked_

data InputHabitRequestField = ∀ α. F !String !(Getter InputHabit (Maybe α)) !(α → String)

inputHabitToRequest ∷ InputHabit → [(BS8.ByteString, BS8.ByteString)]
inputHabitToRequest input_habit =
  mapMaybe
    (\(F name getter_ f) → (input_habit ^. getter_) <&> (f >>> BS8.pack >>> (BS8.pack name,)))
    input_habit_request_fields
  ⊕
  map (UUID.toText >>> encodeUtf8 >>> ("group",)) (toList $ input_habit ^. input_group_membership_)
 where
  bF ∷ String → ALens' InputHabit Bool → InputHabitRequestField
  bF name lens_= F name (cloneLens lens_ . to (bool Nothing (Just "on"))) identity

  input_habit_request_fields ∷ [InputHabitRequestField]
  input_habit_request_fields =
    [ F "name" input_name_ unpack
    , F "importance" input_importance_ show
    , F "difficulty" input_difficulty_ show
    , F "frequency" input_frequency_ $ \case
          InputIndefinite → "Indefinite"
          InputOnce → "Once"
          InputRepeated → "Repeated"
    , bF "once_has_deadline" input_once_has_deadline_
    , F "repeated" input_repeated_ $ \case
          InputDaily → "Daily"
          InputWeekly → "Weekly"
    , F "daily_period" input_daily_period_ show
    , F "weekly_period" input_weekly_period_ show
    ]
    ⊕
    [ bF (unpack weekday_name) (input_days_to_repeat_ . weekday_lens_)
    | (_, weekday_name, weekday_lens_) ← weekdays
    ]
    ⊕
    [ F "days_to_keep" input_days_to_keep_ show
    , F "days_to_keep_mode" input_days_to_keep_mode_ $ \case
          InputKeepDaysInPast → "KeepDaysInPast"
          InputKeepNumberOfDays → "KeepNumberOfDays"
    , F "next_deadline" input_next_deadline_ identity
    , F "maybe_last_marked" input_maybe_last_marked_ formatLocalTimeForExchange
    ]
