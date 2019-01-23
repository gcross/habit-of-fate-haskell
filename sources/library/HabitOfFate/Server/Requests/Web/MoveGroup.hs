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

module HabitOfFate.Server.Requests.Web.MoveGroup (handler) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (temporaryRedirect307)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

handler ∷ Environment → ScottyM ()
handler environment = do
  Scotty.post "/groups/:group_id/move" action
  Scotty.post "/groups/:group_id/move/:new_index" action
 where
  action = webTransaction environment $ do
    group_id ← getParam "group_id"
    new_index ← getParam "new_index" <&> (\n → n-1)
    log [i|Web POST request to move group with id #{group_id} to index #{new_index}.|]
    old_groups ← use groups_
    case moveWithIdToIndex group_id new_index old_groups of
      Left exc → log [i|Exception moving habit: #{exc}|]
      Right new_groups → groups_ .= new_groups
    pure $ redirectsToResult temporaryRedirect307 "/"
