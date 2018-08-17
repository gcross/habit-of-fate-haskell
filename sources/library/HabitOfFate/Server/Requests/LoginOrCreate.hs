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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.LoginOrCreate where

import HabitOfFate.Prelude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, modifyTVar, newTVar)
import Network.HTTP.Types.Status (conflict409, created201)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common

handleCreateAccountApi ∷ Environment → ScottyM ()
handleCreateAccountApi Environment{..} = do
  Scotty.post "/api/create" $ do
    logRequest
    username ← paramGuardingAgainstMissing "username"
    password ← paramGuardingAgainstMissing "password"
    logIO $ [i|Request to create an account for "#{username}".|]
    liftIO >>> join $ do
      new_account ← newAccount password
      atomically $ do
        accounts ← readTVar accounts_tvar
        if member username accounts
          then pure $ do
            logIO $ [i|Account "#{username}" already exists!|]
            Scotty.status conflict409
          else do
            account_tvar ← newTVar new_account
            modifyTVar accounts_tvar $ insertMap username account_tvar
            pure $ do
              logIO $ [i|Account "#{username}" successfully created!|]
              Scotty.status created201
              createAndReturnCookie username

handleLoginApi ∷ Environment → ScottyM ()
handleLoginApi Environment{..} = do
  Scotty.post "/api/login" $ do
    logRequest
    username ← paramGuardingAgainstMissing "username"
    password ← paramGuardingAgainstMissing "password"
    logIO $ [i|Request to log into an account with "#{username}".|]
    account_tvar ←
      (accounts_tvar |> readTVarMonadIO |> fmap (lookup username))
      >>=
      maybe (finishWithStatusMessage 404 "Not Found: No such account") return
    (
      readTVarMonadIO account_tvar
      >>=
      (
        passwordIsValid password
        >>>
        bool (finishWithStatusMessage 403 "Forbidden: Invalid password") (logIO "Login successful.")
      )
      >>
      createAndReturnCookie username
      )
