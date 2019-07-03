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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.EditAndDeleteGroup (handler) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Group
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

data DeletionMode = NoDeletion | DeletionAvailable | ConfirmDeletion

groupPage ∷ Monad m ⇒ UUID → Lazy.Text → DeletionMode → Group → m TransactionResult
groupPage group_id error_message deletion_mode group =
  renderTopOnlyPageResult
   "Group of Fate - Editing a Group"
   (\_ → ["edit_group"])
   []
   Nothing
   ok200
  >>> pure $ \_ →
    H.form ! A.method "post" $ do
      H.div ! A.class_ "fields" $ do
        -- Name
        H.div ! A.class_ "label" $ H.toHtml ("Name:" ∷ Text)
        H.div $
          H.input
            ! A.type_ "text"
            ! A.name "name"
            ! A.value (H.toValue group)
            ! A.required "true"
            ! A.id "name_input"

      H.hr

      H.div ! A.id "error_message" $ H.toHtml error_message

      H.div ! A.class_ "submit" $ do
        H.a ! A.class_ "sub" ! A.href "/habits" $ toHtml ("Cancel" ∷ Text)
        H.input
          ! A.class_ "sub"
          ! A.formaction (H.toValue [i|/groups/#{UUID.toText group_id}|])
          ! A.type_ "submit"

      case deletion_mode of
        NoDeletion → mempty
        DeletionAvailable → do
          H.hr
          H.form ! A.method "get" $ do
            H.input
              ! A.type_ "hidden"
              ! A.name "confirm"
              ! A.value "0"
            H.input
              ! A.type_ "submit"
              ! A.formaction (H.toValue [i|/groups/#{UUID.toText group_id}/delete|])
              ! A.value "Delete"
        ConfirmDeletion → do
          H.hr
          H.form ! A.method "post" $ do
            H.input
              ! A.type_ "hidden"
              ! A.name "confirm"
              ! A.value "1"
            H.input
              ! A.type_ "submit"
              ! A.formaction (H.toValue [i|/groups/#{UUID.toText group_id}/delete|])
              ! A.value "Confirm Delete?"

handleEditGroupGet ∷ Environment → ScottyM ()
handleEditGroupGet environment = do
  Scotty.get "/groups/:group_id" <<< webTransaction environment $ do
    group_id ← getParam "group_id"
    log [i|Web GET request for group with id #{group_id}.|]
    maybe_group ← use (groups_ . at group_id)
    uncurry (groupPage group_id "") $ case maybe_group of
      Nothing → (NoDeletion, "")
      Just group → (DeletionAvailable, group)

extractGroup ∷ Transaction (Group, Lazy.Text)
extractGroup = do
  group_id ← getParam "group_id"
  default_group ← use (groups_ . at group_id) <&> fromMaybe ""
  (name_value, name_error) ←
    getParamMaybe "name"
    <&>
    maybe
      (default_group, "No value for the name was present.")
      (\value →
        if null value
          then ("", "Name for the group may not be empty.")
          else (pack value, "")
      )
  pure
    ( name_value
    , find (onull >>> not) >>> fromMaybe "" $
       [ name_error
       ]
    )

handleEditGroupPost ∷ Environment → ScottyM ()
handleEditGroupPost environment = do
  Scotty.post "/groups/:group_id" <<< webTransaction environment $ do
    group_id ← getParam "group_id"
    log [i|Web POST request for group with id #{group_id}.|]
    (extracted_group, error_message) ← extractGroup
    if onull error_message
      then do
        log [i|Updating group #{group_id} to #{extracted_group}|]
        groups_ . at group_id .= Just extracted_group
        pure $ redirectsToResult temporaryRedirect307 "/habits"
      else do
        log [i|Failed to update group #{group_id}:|]
        log [i|    Error message: #{error_message}|]
        deletion_mode ←
          use (groups_ . items_map_)
          <&>
          (member group_id >>> bool NoDeletion DeletionAvailable)
        groupPage group_id error_message deletion_mode extracted_group

handleDeleteGroupGet ∷ Environment → ScottyM ()
handleDeleteGroupGet environment = do
  Scotty.get "/groups/:group_id/delete" <<< webTransaction environment $ do
    group_id ← getParam "group_id"
    log [i|Web GET request to delete group with id #{group_id}.|]
    maybe_group ← use (groups_ . at group_id)
    case maybe_group of
      Nothing → pure $ redirectsToResult temporaryRedirect307 "/habits"
      Just group → groupPage group_id "" DeletionAvailable group

handleDeleteGroupPost ∷ Environment → ScottyM ()
handleDeleteGroupPost environment = do
  Scotty.post "/groups/:group_id/delete" <<< webTransaction environment $ do
    group_id ← getParam "group_id"
    log [i|Web POST request to delete group with id #{group_id}.|]
    confirm ∷ Int ← getParamMaybe "confirm" <&> fromMaybe 0
    if confirm == 1
      then do
        log [i|Deleting group #{group_id}|]
        groups_ . at group_id .= Nothing
        pure $ redirectsToResult temporaryRedirect307 "/habits"
      else do
        log [i|Confirming delete for group #{group_id}|]
        (extracted_group, error_message) ← extractGroup
        groupPage group_id error_message ConfirmDeletion extracted_group

handler ∷ Environment → ScottyM ()
handler environment = do
  handleEditGroupGet environment
  handleEditGroupPost environment
  handleDeleteGroupGet environment
  handleDeleteGroupPost environment
