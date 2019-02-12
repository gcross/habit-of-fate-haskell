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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Runners.Trial.Transaction where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.Operational (interpretWithMonad)
import Control.Monad.Random (uniform, weighted)

import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest.Runners.Trial
import HabitOfFate.Server.Transaction
import HabitOfFate.Trial

runInTransaction ∷ ∀ s α. Scale → Program s α → StateT s Transaction α
runInTransaction scale = unwrapProgram >>> interpretWithMonad interpret
 where
  interpret ∷ ∀ β. Instruction s β → StateT s Transaction β
  interpret (Instruction_ChooseFrom list) = uniform list
  interpret  Instruction_Get = get
  interpret (Instruction_Put s) = put s
  interpret (Instruction_Throw exc) = throwM exc
  interpret (Instruction_TryAverted programs) =
    weighted [(SuccessResult, 1), (FailureResult, 2)]
    >>=
    (
      taggedLensForResult
      >>>
      (programs ^.)
      >>>
      Program
      >>>
      runInTransaction scale
    )
  interpret (Instruction_TryProgress no_progress progress) =
    tryBinomial (1/3) scale >>= (bool `on` (Program >>> runInTransaction scale)) no_progress progress
