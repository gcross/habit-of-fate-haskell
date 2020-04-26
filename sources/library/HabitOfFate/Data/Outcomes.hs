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
      { common_title ∷ content
      , common_story ∷ content
      , common_question ∷ content
      , success_choice ∷ content
      , success_title ∷ content
      , success_story ∷ content
      , failure_choice ∷ content
      , failure_title ∷ content
      , failure_story ∷ content
      , shames ∷ [content]
      }
  | SuccessAvertedFailure
      { common_title ∷ content
      , common_story ∷ content
      , common_question ∷ content
      , success_choice ∷ content
      , success_title ∷ content
      , success_story ∷ content
      , averted_choice ∷ content
      , averted_title ∷ content
      , averted_story ∷ content
      , failure_choice ∷ content
      , failure_title ∷ content
      , failure_story ∷ content
      , shames ∷ [content]
      }
  | SuccessDangerAvertedFailure
      { common_title ∷ content
      , common_story ∷ content
      , common_question ∷ content
      , success_choice ∷ content
      , success_title ∷ content
      , success_story ∷ content
      , danger_choice ∷ content
      , danger_title ∷ content
      , danger_story ∷ content
      , danger_question ∷ content
      , averted_choice ∷ content
      , averted_title ∷ content
      , averted_story ∷ content
      , failure_choice ∷ content
      , failure_title ∷ content
      , failure_story ∷ content
      , shames ∷ [content]
      } deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

instance ToJSON α ⇒ ToJSON (Outcomes α) where
  toJSON outcomes = runJSONBuilder $ do
    writeField "common title"    (outcomes & common_title)
    writeField "common story"    (outcomes & common_story)
    writeField "common question" (outcomes & common_question)
    writeField "success choice"  (outcomes & success_choice)
    writeField "success title"   (outcomes & success_title)
    writeField "success story"   (outcomes & success_story)
    writeField "failure choice"  (outcomes & failure_choice)
    writeField "failure title"   (outcomes & failure_title)
    writeField "failure story"   (outcomes & failure_story)
    writeField "shames"          (outcomes & shames)
    case outcomes of
      SuccessFailure{..} → pure ()
      _ → do
        writeField "averted choice" (outcomes & averted_choice)
        writeField "averted title"  (outcomes & averted_title)
        writeField "averted story"  (outcomes & averted_story)
    case outcomes of
      SuccessDangerAvertedFailure{..} → do
        writeField "danger choice"    danger_choice
        writeField "danger title"     danger_title
        writeField "danger story"     danger_story
        writeField "danger question"  danger_question
      _ → pure ()
    writeField "kind" $ case outcomes of
      SuccessFailure{..} → "success/failure" ∷ Text
      SuccessAvertedFailure{..} → "success/averted/failure"
      SuccessDangerAvertedFailure{..} → "success/danger/averted/failure"

instance FromJSON α ⇒ FromJSON (Outcomes α) where
  parseJSON = withObject "outcomes must be object-shaped" $ \o → do
    common_title    ← o .: "common title"
    common_story    ← o .: "common story"
    common_question ← o .: "common question"
    success_choice  ← o .: "success choice"
    success_title   ← o .: "success title"
    success_story   ← o .: "success story"
    failure_choice  ← o .: "failure choice"
    failure_title   ← o .: "failure title"
    failure_story   ← o .: "failure story"
    shames          ← o .: "shames"
    kind ∷ Text ← o .: "kind"
    unless (kind ∈ ["success/failure", "success/averted/failure", "success/danger/averted/failure"]) $
      fail [i|Invalid outcomes kind: #{kind}|]
    case kind of
      "success/failure" → pure SuccessFailure{..}
      _ → do
        averted_choice ← o .: "averted choice"
        averted_title  ← o .: "averted title"
        averted_story  ← o .: "averted story"
        case kind of
          "success/averted/failure" → pure SuccessAvertedFailure{..}
          _ → do
            danger_choice    ← o .: "danger choice"
            danger_title     ← o .: "danger title"
            danger_story     ← o .: "danger story"
            danger_question  ← o .: "danger question"
            pure SuccessDangerAvertedFailure{..}
