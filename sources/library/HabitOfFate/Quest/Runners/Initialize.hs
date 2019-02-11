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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Runners.Initialize where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(..))
import qualified Control.Monad.Operational as Operational

import HabitOfFate.Quest.Classes
import HabitOfFate.Substitution

data Instruction α where
  Instruction_AllocateName ∷ Gender → Instruction Gendered
  Instruction_ChooseFrom ∷ [α] → Instruction α
  Instruction_ShuffleFrom ∷ [α] → Instruction [α]
  Instruction_Throw ∷ Exception e ⇒ e → Instruction α

newtype Program α = Program
  { unwrapProgram ∷ Operational.Program Instruction α }
  deriving (Applicative, Functor, Monad)

wrapConstant ∷ Instruction β → Program β
wrapConstant instr = instr |> Operational.singleton >>> Program

wrapFunction ∷ (α → Instruction β) → α → Program β
wrapFunction instr = instr >>> Operational.singleton >>> Program

instance AllocateName Program where
  allocateName = wrapFunction Instruction_AllocateName

instance ChooseFrom Program where
  chooseFrom = wrapFunction Instruction_ChooseFrom

instance ShuffleFrom Program where
  shuffleFrom = wrapFunction Instruction_ShuffleFrom

instance MonadThrow Program where
  throwM = wrapFunction Instruction_Throw
