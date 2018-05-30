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
import HabitOfFate.Story
import HabitOfFate.Story.Substitution
import HabitOfFate.TH

data GameState = GameState
  { _credits ∷ Credits
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''GameState
makeLenses ''GameState

newtype Game α =
    Game { unwrapGame ∷ StateT GameState (WriterT (Seq Paragraph) (Rand StdGen)) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    ,MonadState GameState
    ,MonadWriter (Seq Paragraph)
    )

data RunGameResult α = RunGameResult
  { _returned_value ∷ α
  , _new_game ∷ GameState
  , _game_paragraphs ∷ Seq Paragraph
  } deriving (Eq,Ord,Read,Show)
makeLenses ''RunGameResult

class MonadRandom m ⇒ MonadGame m where
  addParagraph ∷ Paragraph → m ()

instance MonadGame Game where
  addParagraph = gameAddParagraph

instance MonadGame m ⇒ MonadGame (StateT s m) where
  addParagraph = lift . addParagraph

newGame ∷ GameState
newGame = GameState (Credits (Successes 0) (Failures 0))

runGame ∷ GameState → Game α → Rand StdGen (RunGameResult α)
runGame state =
  unwrapGame
  >>>
  flip runStateT state
  >>>
  runWriterT
  >>>
  fmap (RunGameResult |> uncurry |> uncurry)

gameAddParagraph ∷ Paragraph → Game ()
gameAddParagraph = singleton >>> tell

substituteAndAddParagraphs ∷ MonadGame m ⇒ Substitutor → [SubParagraph] → m ()
substituteAndAddParagraphs subs =
  traverse (substitute subs)
  >>>
  either error identity
  >>>
  traverse_ addParagraph

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join
