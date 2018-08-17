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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Logout (handleLogout) where

import HabitOfFate.Prelude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import Web.Cookie (parseCookiesText)
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Server.Common

handleLogout ∷ Environment → ScottyM ()
handleLogout Environment{..} = do
  Scotty.post "/api/logout" $ action
  Scotty.post "/logout"     $ action >> Scotty.redirect "/login"
 where
  action = do
    logRequest
    maybe_cookie_header ← Scotty.header "Cookie"
    case maybe_cookie_header of
      Nothing → pure ()
      Just cookie_header →
        let maybe_cookie =
              cookie_header
                |> view strict
                |> encodeUtf8
                |> parseCookiesText
                |> lookup "token"
                |> fmap Cookie
        in case maybe_cookie of
          Nothing → pure ()
          Just cookie → (liftIO <<< atomically) $ do
            cookies ← readTVar cookies_tvar
            case lookup cookie cookies of
              Nothing → pure ()
              Just (expiration_time, _) →
                modifyTVar expirations_tvar $ deleteSet (expiration_time, cookie)
            writeTVar cookies_tvar $ deleteMap cookie cookies
