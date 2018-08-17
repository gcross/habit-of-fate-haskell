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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.LoginOrCreate (handleLoginOrCreate) where

import HabitOfFate.Prelude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, modifyTVar, newTVar)
import Network.HTTP.Types.Status (conflict409, created201)
import Text.Blaze.Html5 (AttributeValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging
import HabitOfFate.Server.Actions.Queries
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

basicTextInput ∷ AttributeValue → AttributeValue → AttributeValue → Html → Html
basicTextInput type_ name placeholder =
  (! A.type_ type_)
  >>>
  (! A.name name)
  >>>
  (! A.placeholder placeholder)

basicTextForm ∷ [Html → Html] → Html
basicTextForm =
  foldMap
    (\setAttributes →
      H.input |> setAttributes |> (H.div ! A.class_ "fields")
    )

handleCreateAccountWeb ∷ Environment → ScottyM ()
handleCreateAccountWeb Environment{..} = do
  Scotty.get "/create" action
  Scotty.post "/create" action
 where
  action = do
    logRequest
    username@(Username username_) ← Username <$> paramOrBlank "username"
    password1 ← paramOrBlank "password1"
    password2 ← paramOrBlank "password2"

    error_message ∷ Text ←
      if ((not . onull $ password1) && password1 == password2)
        then do
          logIO $ [i|Request to create an account for "#{username_}".|]
          liftIO >>> join $ do
            new_account ← newAccount password1
            atomically $ do
              accounts ← readTVar accounts_tvar
              if member username accounts
                then pure $ do
                  logIO $ [i|Account "#{username_}" already exists!|]
                  Scotty.status conflict409
                  pure "This account already exists."
                else do
                  account_tvar ← newTVar new_account
                  modifyTVar accounts_tvar $ insertMap username account_tvar
                  pure $ do
                    logIO $ [i|Account "#{username_}" successfully created!|]
                    createAndReturnCookie username
                    Scotty.redirect "/"
        else pure $
          if onull username_
            then
              if onull password1
                then ""
                else "Did not specify username."
            else
              case (password1, password2) of
                ("", "") → "Did not type the password."
                ("", _) → "Did not type the password twice."
                (_, "") → "Did not type the password twice."
                _ | password1 == password2 → ""
                _ | otherwise → "The passwords did not agree."

    renderHTMLUsingTemplate "Habit of Fate - Account Creation" ["common", "enter"] >>> Scotty.html $
      H.div ! A.class_ "enter" $ do
        H.div ! A.class_ "tabs" $ do
          H.span ! A.class_ "inactive" $ H.a ! A.href "/login" $ H.toHtml ("Login" ∷ Text)
          H.span ! A.class_ "active" $ H.toHtml ("Create" ∷ Text)
        H.form ! A.method "post" $ do
          basicTextForm >>> H.div $
            [ basicTextInput "text" "username" "Username" >>> (! A.value (H.toValue username_))
            , basicTextInput "password" "password1" "Password"
            , basicTextInput "password" "password2" "Password (again)"
            ]
          when ((not <<< onull) error_message) $
            H.div ! A.id "error-message" $ H.toHtml error_message
          H.div $
            H.input
              ! A.class_ "submit"
              ! A.type_ "submit"
              ! A.formmethod "post"
              ! A.value "Create Account"

handleLoginWeb ∷ Environment → ScottyM ()
handleLoginWeb Environment{..} = do
  Scotty.get "/login" action
  Scotty.post "/login" action
 where
  action = do
    logRequest
    username@(Username username_) ← Username <$> paramOrBlank "username"
    password ← paramOrBlank "password"

    error_message ∷ Text ←
      if onull username_
        then pure ""
        else do
          logIO [i|Request to log in "#{username_}".|]
          accounts ← readTVarMonadIO accounts_tvar
          case lookup username accounts of
            Nothing → do
              logIO [i|No account has username #{username_}.|]
              pure "No account has that username."
            Just account_tvar → do
              account ← readTVarMonadIO account_tvar
              if passwordIsValid password account
                then do
                  logIO [i|Successfully logged in #{username_}.|]
                  createAndReturnCookie username
                  Scotty.redirect "/habits"
                else do
                  logIO [i|Incorrect password for #{username_}.|]
                  pure "No account has that username."

    renderHTMLUsingTemplate "Habit of Fate - Login" ["common", "enter"] >>> Scotty.html $
      H.div ! A.class_ "enter" $ do
        H.div ! A.class_ "tabs" $ do
          H.span ! A.class_ "active" $ H.toHtml ("Login" ∷ Text)
          H.span ! A.class_ "inactive" $ H.a ! A.href "/create" $ H.toHtml ("Create" ∷ Text)
        H.form ! A.method "post" $ do
          basicTextForm >>> H.div $
            [ basicTextInput "text" "username" "Username" >>> (! A.value (H.toValue username_))
            , basicTextInput "password" "password" "Password"
            ]
          when ((not <<< onull) error_message) $
            H.div ! A.id "error-message" $ H.toHtml error_message
          H.div $
            H.input
              ! A.class_ "submit"
              ! A.type_ "submit"
              ! A.formmethod "post"
              ! A.value "Login"

handleLoginOrCreate ∷ Environment → ScottyM ()
handleLoginOrCreate environment = do
  handleCreateAccountApi environment
  handleCreateAccountWeb environment
  handleLoginApi environment
  handleLoginWeb environment
