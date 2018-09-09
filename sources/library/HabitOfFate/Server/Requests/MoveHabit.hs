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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.MoveHabit (handleMoveHabit) where

import HabitOfFate.Prelude

import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction.Common
import HabitOfFate.Server.Transaction.Writer

handleMoveHabit ∷ Environment → ScottyM ()
handleMoveHabit environment = do
  Scotty.post "/move/:habit_id" action
  Scotty.post "/move/:habit_id/:new_index" action
 where
  action = webWriter environment $ do
    habit_id ← getParam "habit_id"
    new_index ← getParam "new_index" <&> (\n → n-1)
    log [i|Web POST request to move habit with id #{habit_id} to index #{new_index}.|]
    old_habits ← use habits_
    case moveHabitWithIdToIndex habit_id new_index old_habits of
      Left exc → log [i|Exception moving habit: #{exc}|]
      Right new_habits → habits_ .= new_habits
    redirectTo "/"
