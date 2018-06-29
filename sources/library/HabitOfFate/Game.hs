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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Game where

import HabitOfFate.Prelude

import Control.Monad.Random hiding (split, uniform)
import qualified Control.Monad.Random as Random

import HabitOfFate.Credits
import HabitOfFate.TH

data GameState = GameState
  { _credits_ ∷ Credits
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''GameState
makeLenses ''GameState

newtype Game α =
    Game { unwrapGame ∷ StateT GameState (Rand StdGen) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    ,MonadState GameState
    )

newGame ∷ GameState
newGame = GameState (Credits (Successes 0) (Failures 0))

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join
