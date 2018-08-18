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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.GetQuestStatus (handleGetQuestStatus) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (ok200)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Server.Common
import HabitOfFate.Server.Program.Common
import HabitOfFate.Server.Program.Reader
import HabitOfFate.Story.Renderer.XML

handleGetQuestStatusApi ∷ Environment → ScottyM ()
handleGetQuestStatusApi environment =
  Scotty.get "/api/status" <<< apiReader environment $
    ask
    >>=
    (getAccountStatus >>> renderEventToXMLText >>> returnLazyText ok200)

handleGetQuestStatusWeb ∷ Environment → ScottyM ()
handleGetQuestStatusWeb environment =
  Scotty.get "/status" <<< webReader environment $
    (ask <&> getAccountStatus)
    >>=
    renderEventToHTMLAndReturn "Habit of Fate - Quest Status" [] ok200

handleGetQuestStatus ∷ Environment → ScottyM ()
handleGetQuestStatus environment = do
  handleGetQuestStatusApi environment
  handleGetQuestStatusWeb environment
