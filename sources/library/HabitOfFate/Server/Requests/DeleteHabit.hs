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

module HabitOfFate.Server.Requests.DeleteHabit (handleDeleteHabit) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (noContent204, notFound404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction.Common
import HabitOfFate.Server.Transaction.Writer

handleDeleteHabit ∷ Environment → ScottyM ()
handleDeleteHabit environment =
  Scotty.delete "/api/habits/:habit_id" <<< apiWriter environment $ do
    habit_id ← getParam "habit_id"
    log $ [i|Requested to delete habit with id #{habit_id}.|]
    habit_was_there ← isJust <$> (habits_ . at habit_id <<.= Nothing)
    returnNothing $
      if habit_was_there
        then noContent204
        else notFound404
