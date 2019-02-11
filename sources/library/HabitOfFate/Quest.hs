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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest
  ( AllocateName(..)

  , InitializeQuestResult(..)
  , RunQuestResult(..)
  , QuestStatus(..)
  , TryQuestResult(..)

  , InitializeQuestRunner
  , GetStatusQuestRunner
  , TrialQuestRunner

  , allocateAny
  , uniformAction
  , weightedAction
  ) where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Quest.Classes
import qualified HabitOfFate.Quest.Runners.GetStatus as GetStatus
import HabitOfFate.Substitution

allocateAny ∷ (AllocateName m, MonadRandom m) ⇒ m Gendered
allocateAny =
  getRandom
  >>=
  bool (allocateName Male) (allocateName Female)

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join

data InitializeQuestResult α = InitializeQuestResult
  { initialQuestState ∷ α
  , initialQuestEvent ∷ Markdown
  }
makeLenses ''InitializeQuestResult

data RunQuestResult s = RunQuestResult
  { maybeRunQuestState ∷ Maybe s
  , runQuestEvent ∷ Markdown
  } deriving (Functor)
makeLenses ''RunQuestResult

data QuestStatus = QuestInProgress | QuestHasEnded SuccessOrFailureResult Markdown

data TryQuestResult = TryQuestResult
  { tryQuestStatus ∷ QuestStatus
  , tryQuestEvent ∷ Markdown
  }

type InitializeQuestRunner s = ∀ m. (AllocateName m, MonadRandom m, MonadThrow m) ⇒ m (InitializeQuestResult s)
type GetStatusQuestRunner s = GetStatus.Program s Markdown
type TrialQuestRunner s = ∀ m. (MonadState s m, MonadRandom m, MonadThrow m) ⇒ SuccessOrFailureResult → Scale → m TryQuestResult
