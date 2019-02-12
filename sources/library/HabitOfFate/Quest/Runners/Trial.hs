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

module HabitOfFate.Quest.Runners.Trial where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(..))
import qualified Control.Monad.Operational as Operational

import HabitOfFate.Data.Tagged
import HabitOfFate.Quest.Classes

data Instruction s α where
  Instruction_ChooseFrom ∷ [α] → Instruction s α
  Instruction_Get ∷ Instruction s s
  Instruction_Put ∷ s → Instruction s ()
  Instruction_Throw ∷ Exception e ⇒ e → Instruction s α
  Instruction_TryAverted ∷ Tagged (InnerProgram s α) → Instruction s α
  Instruction_TryProgress ∷ InnerProgram s α → InnerProgram s α → Instruction s α

type InnerProgram s = Operational.Program (Instruction s)

newtype Program s α = Program
  { unwrapProgram ∷ InnerProgram s α }
  deriving (Applicative, Functor, Monad)

wrapConstant ∷ Instruction s β → Program s β
wrapConstant instr = instr |> Operational.singleton >>> Program

wrapFunction ∷ (α → Instruction s β) → α → Program s β
wrapFunction instr = instr >>> Operational.singleton >>> Program

instance ChooseFrom (Program s) where
  chooseFrom = wrapFunction Instruction_ChooseFrom

instance MonadState s (Program s) where
  get = wrapConstant Instruction_Get
  put = wrapFunction Instruction_Put

instance MonadThrow (Program s) where
  throwM = wrapFunction Instruction_Throw

tryAverted ∷ Tagged (Program s α) → Program s α
tryAverted = fmap unwrapProgram >>> Instruction_TryAverted >>> Operational.singleton >>> Program

tryProgress ∷ Program s α → Program s α → Program s α
tryProgress (Program no_progress) (Program progress) =
  Instruction_TryProgress no_progress progress |> Operational.singleton >>> Program
