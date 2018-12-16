{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Api.Deadlines (handler) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (ok200)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Server.Common
import HabitOfFate.Server.Requests.Shared.Deadlines
import HabitOfFate.Server.Transaction

handleGetDeadlines ∷ Environment → ScottyM ()
handleGetDeadlines environment =
  Scotty.get "/api/deadlines" $ apiTransaction environment $
    (getDeadlines
     <&>
     (
      fmap ((^. _1) &&& (^. _3)) -- grab the deadlines, drop the habit
      >>>
      jsonResult ok200
     )
    )

handler ∷ Environment → ScottyM ()
handler environment = do
  handleGetDeadlines environment
