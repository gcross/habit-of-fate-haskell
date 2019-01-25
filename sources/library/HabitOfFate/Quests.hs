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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests where

import HabitOfFate.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object,String), (.:), withObject, withText)

import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Quest

data CurrentQuestState =
    Forest Forest.State
  deriving (Eq,Ord,Read,Show)
makePrisms ''CurrentQuestState

instance ToJSON CurrentQuestState where
  toJSON (Forest state) = toJSONFor "forest" state
   where
    toJSONFor ∷ ToJSON α ⇒ Text → α → Value
    toJSONFor module_name = toJSON >>> \case
      Object without_module_name → Object (insertMap "module" (String module_name) without_module_name)
      _ → error [i|quest state for #{module_name} must encode to an object|]

instance FromJSON CurrentQuestState where
  parseJSON = withObject "expected entity with object shape" $ \o →
    o .: "module"
    >>=
    withText
      "module name must be a string"
      (\case
        "forest" → Forest <$> parseJSON (Object o)
        other → fail [i|no such module #{other}|]
      )

data Quest s = Quest
  { questPrism ∷ Prism' CurrentQuestState s
  , questInitialize ∷ InitializeQuestRunner s
  , questGetStatus ∷ GetStatusQuestRunner s
  , questTrial ∷ TrialQuestRunner s
  }

data WrappedQuest = ∀ s. WrappedQuest (Quest s)

quests ∷ [WrappedQuest]
quests =
  [WrappedQuest $
     Quest
       _Forest
       Forest.initialize
       Forest.getStatus
       Forest.trial
  ]

type RunCurrentQuestResult = RunQuestResult CurrentQuestState

runCurrentQuest ∷ (∀ s. Quest s → s → α) → CurrentQuestState → α
runCurrentQuest f current_quest_state =
  foldr
    (\(WrappedQuest quest) rest →
      case current_quest_state ^? (questPrism quest) of
        Nothing → rest
        Just quest_state → f quest quest_state
    )
    (error "Unrecognized quest type")
    quests
