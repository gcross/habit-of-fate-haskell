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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Habit where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData)
import Control.Monad.Catch

import Data.Aeson
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)
import Data.UUID

import GHC.Generics (Generic)

import Text.Blaze (ToMarkup(..))
import Web.Scotty (Parsable(..))

import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged
import HabitOfFate.TH

data Frequency =
    Indefinite
  | Once (Maybe LocalTime)
  | Repeated DaysToKeep LocalTime Repeated
  deriving (Eq, Generic, NFData, Read, Show, Ord)
deriveJSON ''Frequency

instance Default Frequency where
  def = Indefinite

data Habit = Habit
  { _name_ ∷ Text
  , _scales_ ∷ Tagged Scale
  , _frequency_ ∷ Frequency
  , _group_membership_ ∷ Set UUID
  , _maybe_last_marked_ ∷ Maybe LocalTime
  } deriving (Eq,Generic,NFData,Ord,Read,Show)
makeLenses ''Habit
deriveJSON ''Habit

difficulty_, importance_ ∷ Lens' Habit Scale
difficulty_ = scales_ . success_
importance_ = scales_ . failure_

instance Default Habit where
  def = Habit "" def def mempty Nothing

getHabitDeadline ∷ Habit → Maybe LocalTime
getHabitDeadline =
  (^. frequency_)
  >>>
  (\case
    Once maybe_deadline → maybe_deadline
    Repeated _ deadline _ → Just deadline
    Indefinite → Nothing
  )

nextHabitDeadline ∷ Habit → LocalTime → Maybe LocalTime
nextHabitDeadline habit today =
  case habit ^. frequency_ of
    Repeated _ deadline repeated → Just $ nextDeadline repeated today deadline
    _ → Nothing

updateHabitDeadline ∷ Habit → LocalTime → Habit
updateHabitDeadline habit today =
  case habit ^. frequency_ of
    Repeated days_to_keep deadline repeated | today >= deadline →
      habit & frequency_ .~ Repeated days_to_keep (nextDeadline repeated today deadline) repeated
    _ → habit

previousHabitDeadlines ∷ Habit → LocalTime → [LocalTime]
previousHabitDeadlines habit today =
  case habit ^. frequency_ of
    Once (Just deadline) | today >= deadline → [deadline]
    Repeated days_to_keep deadline repeated →
      deadline
        |> nextDeadline repeated today
        |> previousDeadlines repeated
        |> takePreviousDeadlines days_to_keep today deadline
    _ → []

formatLocalTime ∷ LocalTime → String
formatLocalTime = formatTime defaultTimeLocale "%FT%R"
