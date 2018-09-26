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

module HabitOfFate.Server.Requests.NewHabit (handleNewHabit) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import System.Random (randomIO)
import Network.HTTP.Types.Status (temporaryRedirect307)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common

handleNewHabit ∷ Environment → ScottyM ()
handleNewHabit _ =
  Scotty.get "/habits/new" $
    liftIO (randomIO ∷ IO UUID) <&> (show >>> Lazy.pack >>> ("/habits/" ⊕))
    >>=
    setStatusAndRedirect temporaryRedirect307
