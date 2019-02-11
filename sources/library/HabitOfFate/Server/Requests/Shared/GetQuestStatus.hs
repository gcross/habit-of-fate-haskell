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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.GetQuestStatus where

import HabitOfFate.Prelude

import HabitOfFate.Data.Account
import HabitOfFate.Data.Markdown
import HabitOfFate.Quest.Runners.GetStatus.Transaction
import HabitOfFate.Quests
import HabitOfFate.Server.Transaction

getQuestStatus ∷ Transaction Markdown
getQuestStatus =
  use maybe_current_quest_state_
  >>=
  maybe
    (pure $ Markdown "You are between quests.")
    (runCurrentQuest $ flip runGetQuestStatus)

runGetQuestStatus ∷ s → Quest s → Transaction Markdown
runGetQuestStatus quest_state = questGetStatus >>> runInTransaction quest_state
