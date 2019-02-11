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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Runners.GetStatus where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(..))
import qualified Control.Monad.Operational as Operational

import HabitOfFate.Quest.Classes

data Instruction s α where
  Instruction_Ask ∷ Instruction s s
  Instruction_ChooseFrom ∷ [α] → Instruction s α
  -- Instruction_Local ∷ (s → s) → InnerProgram s α → Instruction s α
  Instruction_Throw ∷ Exception e ⇒ e → Instruction s α

type InnerProgram s = Operational.Program (Instruction s)

newtype Program s α = Program { unwrapProgram ∷ InnerProgram s α }
  deriving (Applicative, Functor, Monad)

wrapConstant ∷ Instruction s β → Program s β
wrapConstant instr = instr |> Operational.singleton >>> Program

wrapFunction ∷ (α → Instruction s β) → α → Program s β
wrapFunction instr = instr >>> Operational.singleton >>> Program

instance ChooseFrom (Program s) where
  chooseFrom = wrapFunction Instruction_ChooseFrom

instance MonadReader s (Program s) where
  ask = wrapConstant Instruction_Ask
  -- local f (Program inner) = Operational.singleton >>> Program $ Instruction_Local f inner
  local _ _ = error "local not implemented for GetStatus"

instance MonadThrow (Program s) where
  throwM = wrapFunction Instruction_Throw
