{-
    Habit of Fate, a game to incentivize group formation.
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

module HabitOfFate.Server.Requests.Api.DeleteGroup (handler) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (noContent204, notFound404)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

handler ∷ Environment → ScottyM ()
handler environment =
  Scotty.delete "/api/groups/:group_id" <<< apiTransaction environment $ do
    group_id ← getParam "group_id"
    log $ [i|Requested to delete group with id #{group_id}.|]
    group_was_there ← isJust <$> (groups_ . at group_id <<.= Nothing)
    habits_ . traversed . group_membership_ %= deleteSet group_id
    noContentResult >>> pure $
      if group_was_there
        then noContent204
        else notFound404
