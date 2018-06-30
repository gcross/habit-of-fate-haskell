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

import HabitOfFate.Game
import HabitOfFate.Story

data QuestState α = QuestState
  { _game_ ∷ GameState
  , _quest_ ∷ α
  }
makeLenses ''QuestState

newtype QuestAction s α = QuestAction
  { unwrapQuestAction ∷ StateT s Game α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    )

instance MonadState (QuestState s) (QuestAction s) where
  get =
    QuestAction
    $
    (QuestState
      <$> lift get
      <*> get
    )

  put (QuestState game quest) = QuestAction $ do
    game |> put |> lift
    put quest

data InitialQuestResult α = InitialQuestResult
  { _initial_quest_state_ ∷ α
  , _initial_quest_event_ ∷ Event
  }
makeLenses ''InitialQuestResult

data RunQuestResult s = RunQuestResult
  { _maybe_run_quest_state_ ∷ Maybe s
  , _run_quest_event_ ∷ Event
  } deriving (Functor)
makeLenses ''RunQuestResult

data QuestStatus = QuestInProgress | QuestHasEnded

data QuestResult = QuestResult
  { _quest_status_ ∷ QuestStatus
  , _quest_event_ ∷ Event
  }
makeLenses ''QuestResult

runQuest ∷ s → QuestAction s QuestResult → Game (RunQuestResult s)
runQuest state action = do
  (result, new_state) ←
    action
      |> unwrapQuestAction
      |> flip runStateT state
  pure $
    RunQuestResult
      (case result ^. quest_status_ of
        QuestInProgress → Just new_state
        QuestHasEnded → Nothing
      )
      (result ^. quest_event_)

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = uniform >>> join

weightedAction ∷ MonadRandom m ⇒ [(m α, Rational)] → m α
weightedAction = weighted >>> join
