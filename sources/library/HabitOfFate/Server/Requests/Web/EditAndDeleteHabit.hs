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

module HabitOfFate.Server.Requests.Web.EditAndDeleteHabit (handler) where

import HabitOfFate.Prelude

import Data.UUID (UUID, toText)
import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction.Common
import HabitOfFate.Server.Transaction.Reader
import HabitOfFate.Server.Transaction.Writer

data DeletionMode = NoDeletion | DeletionAvailable | ConfirmDeletion

habitPage ∷ Monad m ⇒ UUID → Lazy.Text → Lazy.Text → Lazy.Text → DeletionMode → Habit → m TransactionResult
habitPage habit_id name_error difficulty_error importance_error deletion_mode habit =
  renderTopOnlyPageAndReturn "Habit of Fate - Editing a Habit" ["edit"] ok200 $
    H.form ! A.method "post" $ do
      H.div ! A.class_ "fields" $ do
        -- Name
        H.div ! A.class_ "label" ! A.id "name_label" $ H.toHtml ("Name:" ∷ Text)
        H.div $
          H.input
            ! A.type_ "text"
            ! A.name "name"
            ! A.value (H.toValue $ habit ^. name_)
            ! A.required "true"
            ! A.size "60"
            ! A.id "name_input"
        H.div ! A.class_ "error_message" ! A.id "name_error" $ H.toHtml name_error

        -- Template for Difficulty and Importance
        let generateScaleEntry ∷ H.AttributeValue → Text → Lens' Habit Scale → Lazy.Text → H.Html
            generateScaleEntry name label value_lens error = do
              H.div
                ! A.class_ "label"
                ! A.id (name ⊕ "_label")
                $ H.toHtml label
              H.select
                ! A.name name
                ! A.required "true"
                ! A.id (name ⊕ "_input")
                $ flip foldMap scales $ \scale →
                    let addSelectedFlag
                          | habit ^. value_lens == scale = (! A.selected "selected")
                          | otherwise = identity
                        unselected_option = H.option ! A.value (scale |> show |> H.toValue)
                    in addSelectedFlag unselected_option $ H.toHtml (displayScale scale)
              H.div
                ! A.class_ "error_message"
                ! A.id (name ⊕ "_error")
                $ H.toHtml error

        generateScaleEntry "difficulty" "Difficulty:" difficulty_ difficulty_error
        generateScaleEntry "importance" "Importance:" importance_ importance_error

      H.hr

      H.div ! A.class_ "submit" $ do
        H.a ! A.class_ "sub" ! A.href "/" $ toHtml ("Cancel" ∷ Text)
        H.input
          ! A.class_ "sub"
          ! A.formaction (H.toValue $ ("/edit/" ∷ Text) ⊕ toText habit_id)
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
              ! A.formaction (H.toValue $ ("/delete/" ∷ Text) ⊕ toText habit_id)
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
              ! A.formaction (H.toValue $ ("/delete/" ∷ Text) ⊕ toText habit_id)
              ! A.value "Confirm Delete?"

handleEditHabitGet ∷ Environment → ScottyM ()
handleEditHabitGet environment = do
  Scotty.get "/edit/:habit_id" <<< webReader environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request for habit with id #{habit_id}.|]
    maybe_habit ← view (habits_ . at habit_id)
    uncurry (habitPage habit_id "" "" "") $ case maybe_habit of
      Nothing → (NoDeletion, def)
      Just habit → (DeletionAvailable, habit)

data HabitExtractionResult = HabitExtractionResult
  { name_error ∷ Lazy.Text
  , difficulty_error ∷ Lazy.Text
  , importance_error ∷ Lazy.Text
  , extracted_habit ∷ Habit
  }

extractHabit ∷ WriterTransaction HabitExtractionResult
extractHabit = do
  habit_id ← getParam "habit_id"
  default_habit ← use (habits_ . at habit_id) <&> fromMaybe def
  (name_value, name_error) ←
    getParamMaybe "name"
    <&>
    maybe
      (default_habit ^. name_, "No value for the name was present.")
      (\value →
        if null value
          then ("", "Name for the habit may not be empty.")
          else (pack value, "")
      )
  let getScale ∷ Lazy.Text → Lens' Habit Scale → WriterTransaction (Scale, Lazy.Text)
      getScale param_name param_lens_ =
        getParamMaybe param_name
        <&>
        (maybe
          (default_habit ^. param_lens_, "No value for the " ⊕ param_name ⊕ " was present.")
          (
            readMaybe
            >>>
            maybe
              (def, "Invalid value for the " ⊕ param_name ⊕ ".")
              (, "")
          )
        )
  (difficulty_value, difficulty_error) ← getScale "difficulty" difficulty_
  (importance_value, importance_error) ← getScale "importance" importance_
  let extracted_habit = Habit name_value (Difficulty difficulty_value) (Importance importance_value)
  pure $ HabitExtractionResult{..}

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/edit/:habit_id" <<< webWriter environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request for habit with id #{habit_id}.|]
    HabitExtractionResult{..} ← extractHabit
    if onull name_error && onull difficulty_error && onull importance_error
      then do
        log [i|Updating habit #{habit_id} to #{extracted_habit}|]
        habits_ . at habit_id .= Just extracted_habit
        redirectTo temporaryRedirect307 "/"
      else do
        log [i|Failed to update habit #{habit_id}:|]
        log [i|    Name error: #{name_error}|]
        log [i|    Difficulty error: #{difficulty_error}|]
        log [i|    Importance error: #{importance_error}|]
        deletion_mode ←
          use (habits_ . habit_map_)
          <&>
          (member habit_id >>> bool NoDeletion DeletionAvailable)
        habitPage habit_id name_error difficulty_error importance_error deletion_mode def

handleDeleteHabitGet ∷ Environment → ScottyM ()
handleDeleteHabitGet environment = do
  Scotty.get "/delete/:habit_id" <<< webReader environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request to delete habit with id #{habit_id}.|]
    maybe_habit ← view (habits_ . at habit_id)
    case maybe_habit of
      Nothing → redirectTo temporaryRedirect307 "/"
      Just habit → habitPage habit_id "" "" "" DeletionAvailable habit

handleDeleteHabitPost ∷ Environment → ScottyM ()
handleDeleteHabitPost environment = do
  Scotty.post "/delete/:habit_id" <<< webWriter environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request to delete habit with id #{habit_id}.|]
    confirm ∷ Int ← getParamMaybe "confirm" <&> fromMaybe 0
    if confirm == 1
      then do
        log [i|Deleting habit #{habit_id}|]
        habits_ . at habit_id .= Nothing
        redirectTo temporaryRedirect307 "/"
      else do
        log [i|Confirming delete for habit #{habit_id}|]
        HabitExtractionResult{..} ← extractHabit
        habitPage habit_id name_error difficulty_error importance_error ConfirmDeletion extracted_habit

handler ∷ Environment → ScottyM ()
handler environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
  handleDeleteHabitGet environment
  handleDeleteHabitPost environment
