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
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Outcomes where

import HabitOfFate.Prelude

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , withObject
  )
import Language.Haskell.TH.Lift (Lift)

import HabitOfFate.JSON

data Outcomes content =
    SuccessFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      }
  | SuccessAvertedFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_averted_choice ∷ content
      , outcomes_averted_title ∷ content
      , outcomes_averted_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      }
  | SuccessDangerAvertedFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_danger_choice ∷ content
      , outcomes_danger_title ∷ content
      , outcomes_danger_story ∷ content
      , outcomes_danger_question ∷ content
      , outcomes_averted_choice ∷ content
      , outcomes_averted_title ∷ content
      , outcomes_averted_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      } deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

instance ToJSON α ⇒ ToJSON (Outcomes α) where
  toJSON outcomes = runJSONBuilder $ do
    writeField "common title"    (outcomes & outcomes_common_title)
    writeField "common story"    (outcomes & outcomes_common_story)
    writeField "common question" (outcomes & outcomes_common_question)
    writeField "success choice"  (outcomes & outcomes_success_choice)
    writeField "success title"   (outcomes & outcomes_success_title)
    writeField "success story"   (outcomes & outcomes_success_story)
    writeField "failure choice"  (outcomes & outcomes_failure_choice)
    writeField "failure title"   (outcomes & outcomes_failure_title)
    writeField "failure story"   (outcomes & outcomes_failure_story)
    writeField "shames"          (outcomes & outcomes_shames)
    case outcomes of
      SuccessFailure{..} → pure ()
      _ → do
        writeField "averted choice" (outcomes & outcomes_averted_choice)
        writeField "averted title"  (outcomes & outcomes_averted_title)
        writeField "averted story"  (outcomes & outcomes_averted_story)
    case outcomes of
      SuccessDangerAvertedFailure{..} → do
        writeField "danger choice"    outcomes_danger_choice
        writeField "danger title"     outcomes_danger_title
        writeField "danger story"     outcomes_danger_story
        writeField "danger question"  outcomes_danger_question
      _ → pure ()
    writeField "kind" $ case outcomes of
      SuccessFailure{..} → "success/failure" ∷ Text
      SuccessAvertedFailure{..} → "success/averted/failure"
      SuccessDangerAvertedFailure{..} → "success/danger/averted/failure"

instance FromJSON α ⇒ FromJSON (Outcomes α) where
  parseJSON = withObject "outcomes must be object-shaped" $ \o → do
    outcomes_common_title    ← o .: "common title"
    outcomes_common_story    ← o .: "common story"
    outcomes_common_question ← o .: "common question"
    outcomes_success_choice  ← o .: "success choice"
    outcomes_success_title   ← o .: "success title"
    outcomes_success_story   ← o .: "success story"
    outcomes_failure_choice  ← o .: "failure choice"
    outcomes_failure_title   ← o .: "failure title"
    outcomes_failure_story   ← o .: "failure story"
    outcomes_shames          ← o .: "shames"
    kind ∷ Text ← o .: "kind"
    unless (kind ∈ ["success/failure", "success/averted/failure", "success/danger/averted/failure"]) $
      fail [i|Invalid outcomes kind: #{kind}|]
    case kind of
      "success/failure" → pure SuccessFailure{..}
      _ → do
        outcomes_averted_choice ← o .: "averted choice"
        outcomes_averted_title  ← o .: "averted title"
        outcomes_averted_story  ← o .: "averted story"
        case kind of
          "success/averted/failure" → pure SuccessAvertedFailure{..}
          _ → do
            outcomes_danger_choice    ← o .: "danger choice"
            outcomes_danger_title     ← o .: "danger title"
            outcomes_danger_story     ← o .: "danger story"
            outcomes_danger_question  ← o .: "danger question"
            pure SuccessDangerAvertedFailure{..}
