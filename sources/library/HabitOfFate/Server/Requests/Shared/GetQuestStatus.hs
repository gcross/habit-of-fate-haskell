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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.GetQuestStatus where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.Account
import HabitOfFate.Quests
import HabitOfFate.Server.Transaction

getQuestStatus ∷ Transaction Lazy.Text
getQuestStatus =
  use maybe_current_quest_state_
  >>=
  maybe
    (pure "You are between quests.")
    (runCurrentQuest run)
  where
   run ∷ ∀ s. Quest s → s → Transaction Lazy.Text
   run quest quest_state = questGetStatus quest quest_state
