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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Actions.Queries where

import HabitOfFate.Prelude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVar)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (badRequest400)
import Web.Cookie (parseCookiesText)
import Web.Scotty (ActionM, Parsable, finish, rescue, status)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Server.Common

param ∷ Parsable α ⇒ Lazy.Text → ActionM α
param name =
  Scotty.param name
  `rescue`
  (const
   $
   do Scotty.text $ "Missing parameter: \"" ⊕ name ⊕ "\""
      status badRequest400
      finish
  )

paramOrBlank ∷ Lazy.Text → ActionM Text
paramOrBlank name = Scotty.param name `rescue` ("" |> pure |> const)

authorizeWith ∷ (∀ α. String → ActionM α) → Environment → ActionM (Username, TVar Account)
authorizeWith actionWhenAuthFails Environment{..} = do
  let handleForbidden ∷ String → Maybe α → ActionM α
      handleForbidden message = maybe (actionWhenAuthFails message) pure
  cookie_header ← Scotty.header "Cookie" >>= handleForbidden "No cookie header."
  cookie ∷ Cookie ←
    cookie_header
      |> view strict
      |> encodeUtf8
      |> parseCookiesText
      |> lookup "token"
      |> handleForbidden "No authorization token in the cookies."
      |> fmap Cookie
  current_time ← liftIO getCurrentTime
  (liftIO <<< atomically <<< runExceptT $ do
    cookies ← lift $ readTVar cookies_tvar
    (expiration_time, username) ←
      cookies
        |> lookup cookie
        |> maybe (throwError "Authorization token unrecognized.") pure
    (expiration_time, cookie)
        |> deleteSet
        |> modifyTVar expirations_tvar
        |> lift
    when (expiration_time < current_time) $ throwError "Authorization token expired."
    accounts ← lift $ readTVar accounts_tvar
    account_tvar ←
      accounts
      |> lookup username
      |> maybe
          (do lift $ modifyTVar cookies_tvar (deleteMap cookie)
              throwError "Token no longer refers to an existing user."
          )
          pure
    lift $ do
      let new_expected_time = addUTCTime (30*86400) current_time
      modifyTVar cookies_tvar $ insertMap cookie (new_expected_time, username)
      modifyTVar expirations_tvar $ insertSet (new_expected_time, cookie)
    pure (username, account_tvar)
   ) >>= either actionWhenAuthFails pure
