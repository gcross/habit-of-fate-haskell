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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Habit where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData)

import Data.Aeson (FromJSON(..),Object,ToJSON(..),Value(..),(.:),(.:?),withArray,withObject)
import Data.Aeson.Types (Parser)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime)
import Data.UUID

import GHC.Generics (Generic)

import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged
import HabitOfFate.JSON
import HabitOfFate.TH

local_time_exchange_format_string ∷ String
local_time_exchange_format_string = "%FT%R"

formatLocalTimeForExchange ∷ LocalTime → String
formatLocalTimeForExchange = formatTime defaultTimeLocale local_time_exchange_format_string

parseLocalTimeFromExchange ∷ MonadFail m ⇒ String → m LocalTime
parseLocalTimeFromExchange = parseTimeM False defaultTimeLocale local_time_exchange_format_string

data Frequency =
    Indefinite
  | Once (Maybe LocalTime)
  | Repeated DaysToKeep LocalTime Repeated
  deriving (Eq, Generic, NFData, Read, Show, Ord)

instance ToJSON Frequency where
  toJSON Indefinite = runJSONBuilder $ do
    writeTextField "mode" "indefinite"
  toJSON (Once maybe_next_deadline) = runJSONBuilder $ do
    writeTextField "mode" "once"
    writeMaybeField "next_deadline" maybe_next_deadline
  toJSON (Repeated days_to_keep next_deadline repeated) = runJSONBuilder $ do
    writeTextField "mode" "repeated"
    case repeated of
      Daily period → do
        writeTextField "repeated_mode" "daily"
        writeField "period" period
      Weekly period days_to_repeat → do
        writeTextField "repeated_mode" "weekly"
        writeField "period" period
        writeField "days_to_repeat" $ unwords $
          [ weekday_abbrev
          | Weekday{..} ← weekdays
          , days_to_repeat ^# weekday_lens_
          ]
    writeField "next_deadline" next_deadline
    case days_to_keep of
      KeepNumberOfDays n → do
        writeTextField "days_to_keep_mode" "count"
        writeField "days_to_keep" n
      KeepDaysInPast n → do
        writeTextField "days_to_keep_mode" "past"
        writeField "days_to_keep" n

instance FromJSON Frequency where
  parseJSON = withObject "expecting object-shaped value" $ \o →
    (o .: "mode" ∷ Parser Text)
    >>=
    \case
      "indefinite" → pure Indefinite
      "once" → Once <$> (o .:? "next_deadline")
      "repeated" →
        Repeated
          <$> (liftA2 ($)
                (
                  (o .: "days_to_keep_mode" ∷ Parser Text)
                  >>=
                  \case
                    "count" → pure KeepNumberOfDays
                    "past" → pure KeepDaysInPast
                    other → fail [i|unrecognized mode for keeping days in past: #{other}|]
                )
                (o .: "days_to_keep")
              )
          <*> (o .: "next_deadline")
          <*> ((o .: "repeated_mode" ∷ Parser Text)
               >>=
               \case
                "daily" →
                    Daily
                      <$> (o .: "period")
                "weekly" → do
                    Weekly
                      <$> (o .: "period")
                      <*> (do days_to_repeat_abbrevs ← (o .: "days_to_repeat") <&> (words >>> setFromList)
                              let unexpected_values = days_to_repeat_abbrevs `difference` weekday_abbrevs
                              unless (onull unexpected_values) $
                                fail [i|Unexpected days_to_repeat values: #{unexpected_values}|]
                              pure $ foldl'
                                (\accum Weekday{..} →
                                  accum & weekday_lens_ #~ member weekday_abbrev days_to_repeat_abbrevs)
                                def
                                weekdays
                          )
                other → fail [i|unexpected repeated mode: #{show other}|]
              )
      other → fail [i|unrecognized frequency mode: #{other}|]

instance Default Frequency where
  def = Indefinite

data Habit = Habit
  { _name_ ∷ Text
  , _scales_ ∷ Tagged Scale
  , _frequency_ ∷ Frequency
  , _group_membership_ ∷ HashSet UUID
  , _maybe_last_marked_ ∷ Maybe LocalTime
  } deriving (Eq,Generic,NFData,Ord,Read,Show)
makeLenses ''Habit

instance ToJSON Habit where
  toJSON Habit{..} = runJSONBuilder $ do
    writeField "name" _name_
    writeField "difficulty" (_scales_ ^. success_)
    writeField "importance" (_scales_ ^. failure_)
    writeField "frequency" _frequency_
    writeField "groups" $ toList _group_membership_
    writeMaybeField "last_marked" _maybe_last_marked_

instance FromJSON Habit where
  parseJSON = withObject "expected entity with shape object" $ \o →
    Habit
      <$> (o .: "name")
      <*> (Tagged <$> (Success <$> (o .: "difficulty")) <*> (Failure <$> (o .: "importance")))
      <*> (o .: "frequency")
      <*> (o .: "groups" >>= withArray "groups must be an array" (toList >>> mapM parseJSON >>> fmap setFromList))
      <*> (o .:? "last_marked")

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
