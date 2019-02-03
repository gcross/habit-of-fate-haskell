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

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random
import qualified Data.Text.Lazy as Lazy
import Data.Vector (Vector)

import HabitOfFate.Data.Age
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Names
import HabitOfFate.Substitution

data InitializeQuestResult α = InitializeQuestResult
  { initialQuestState ∷ α
  , initialQuestEvent ∷ Lazy.Text
  }
makeLenses ''InitializeQuestResult

data RunQuestResult s = RunQuestResult
  { maybeRunQuestState ∷ Maybe s
  , runQuestEvent ∷ Lazy.Text
  } deriving (Functor)
makeLenses ''RunQuestResult

data QuestStatus = QuestInProgress | QuestHasEnded SuccessOrFailureResult Lazy.Text

data TryQuestResult = TryQuestResult
  { tryQuestStatus ∷ QuestStatus
  , tryQuestEvent ∷ Lazy.Text
  }

class MonadAllocateName m where
  allocateNameFrom ∷ Vector Text → Age → m Text

allocateAnyGendered ∷ (MonadAllocateName m, MonadRandom m) ⇒ Age → m Gendered
allocateAnyGendered age =
  getRandom
  >>=
  bool
    (flip Gendered Male <$> allocateNameFrom male_names age)
    (flip Gendered Female <$> allocateNameFrom female_names age)

type InitializeQuestRunner s = ∀ m. (MonadAllocateName m, MonadRandom m, MonadThrow m) ⇒ m (InitializeQuestResult s)
type GetStatusQuestRunner s = ∀ m. s → (MonadRandom m, MonadThrow m) ⇒ m Lazy.Text
type TrialQuestRunner s = ∀ m. (MonadState s m, MonadRandom m, MonadThrow m) ⇒ SuccessOrFailureResult → Scale → m TryQuestResult

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join
