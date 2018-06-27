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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Cont
import Control.Monad.Random

import HabitOfFate.Game

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

instance MonadGame (QuestAction s) where
  addParagraph = addParagraph >>> lift >>> QuestAction

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

data QuestStatus = QuestInProgress | QuestHasEnded

runQuest ∷ s → QuestAction s QuestStatus → Game (Maybe s)
runQuest state action = do
  (new_status, new_state) ←
    action
      |> unwrapQuestAction
      |> flip runStateT state
  pure $ case new_status of
    QuestInProgress → Just new_state
    QuestHasEnded → Nothing
