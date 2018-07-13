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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Random

import HabitOfFate.Credits
import HabitOfFate.Story

data InitialQuestResult α = InitialQuestResult
  { initialQuestState ∷ α
  , initialQuestCredits ∷ Credits
  }
makeLenses ''InitialQuestResult

data RunQuestResult s = RunQuestResult
  { maybeRunQuestState ∷ Maybe s
  , runQuestEvent ∷ Event
  } deriving (Functor)
makeLenses ''RunQuestResult

data QuestStatus = QuestInProgress Double | QuestHasEnded

data QuestResult = QuestResult
  { questStatus ∷ QuestStatus
  , questEvent ∷ Event
  }

type InitialQuestRunner s = Rand StdGen (InitialQuestResult s)
type IntroQuestRunner s = s → (Rand StdGen) Event
type GetStatusQuestRunner s = s → Event
type ProgressToMilestoneQuestRunner s = s → (Rand StdGen) Event
type AttainedMilestoneQuestRunner s = StateT s (Rand StdGen) QuestResult

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join
