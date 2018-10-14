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
import HabitOfFate.Server.Transaction

data DeletionMode = NoDeletion | DeletionAvailable | ConfirmDeletion

habitPage ∷ Monad m ⇒ UUID → Lazy.Text → DeletionMode → Habit → m TransactionResult
habitPage habit_id error_message deletion_mode habit =
  renderTopOnlyPageResult "Habit of Fate - Editing a Habit" ["edit"] ok200 >>> pure $
    H.form ! A.method "post" $ do
      H.div ! A.class_ "fields" $ do
        -- Name
        H.div ! A.class_ "label" $ H.toHtml ("Name:" ∷ Text)
        H.div $
          H.input
            ! A.type_ "text"
            ! A.name "name"
            ! A.value (H.toValue $ habit ^. name_)
            ! A.required "true"
            ! A.size "60"
            ! A.id "name_input"

        -- Template for Difficulty and Importance
        let generateScaleEntry ∷ H.AttributeValue → Text → Lens' Habit Scale → H.Html
            generateScaleEntry name label value_lens = do
              H.div
                ! A.class_ "label"
                $ H.toHtml label
              H.select
                ! A.name name
                ! A.required "true"
                $ flip foldMap scales $ \scale →
                    (H.option
                      ! A.value (scale |> show |> H.toValue)
                      & if habit ^. value_lens == scale then (! A.selected "selected") else identity
                     )$ H.toHtml (displayScale scale)

        generateScaleEntry "difficulty" "Difficulty:" difficulty_
        generateScaleEntry "importance" "Importance:" importance_

        H.toHtml ("Frequency:" ∷ Text)

        H.div ! A.id ("frequency_input") $ do
          H.input
            ! A.type_ "radio"
            ! A.name "frequency"
            ! A.value "Indefinite"
            & if habit ^. frequency_ == Indefinite then (! A.checked "checked") else identity
          H.toHtml ("Indefinite" ∷ Text)
          H.input
            ! A.type_ "radio"
            ! A.name "frequency"
            ! A.value "Once"
            & if habit ^. frequency_ == Once then (! A.checked "checked") else identity
          H.toHtml ("Once" ∷ Text)

      H.hr

      H.div ! A.id "error_message" $ H.toHtml error_message

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
  Scotty.get "/edit/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request for habit with id #{habit_id}.|]
    maybe_habit ← use (habits_ . at habit_id)
    uncurry (habitPage habit_id "") $ case maybe_habit of
      Nothing → (NoDeletion, def)
      Just habit → (DeletionAvailable, habit)

extractHabit ∷ TransactionProgram (Habit, Lazy.Text)
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
  let getScale ∷ Lazy.Text → Lens' Habit Scale → TransactionProgram (Scale, Lazy.Text)
      getScale param_name param_lens_ =
        getParamMaybe param_name
        <&>
        (maybe
          (
            default_habit ^. param_lens_
          , "No value for the " ⊕ param_name ⊕ " was present."
          )
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
  let irrelevant_error
        | difficulty_value == None && importance_value == None =
            "Either the difficulty or the importance must not be None."
        | otherwise = ""
  (frequency_value, frequency_error) ←
    getParamMaybe "frequency"
    <&>
    maybe
      (default_habit ^. frequency_, "No value for the frequency was present.")
      (\value →
        case value of
          "Indefinite" → (Indefinite, "")
          "Once" → (Once, "")
          _ → (Indefinite, "Frequency must be Indefinite or Once, not " ⊕ value)
      )
  pure
    ( Habit name_value (Difficulty difficulty_value) (Importance importance_value) frequency_value
    , find (onull >>> not) >>> fromMaybe "" $
       [ name_error
       , difficulty_error
       , importance_error
       , irrelevant_error
       , frequency_error
       ]
    )

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/edit/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request for habit with id #{habit_id}.|]
    (extracted_habit, error_message) ← extractHabit
    if onull error_message
      then do
        log [i|Updating habit #{habit_id} to #{extracted_habit}|]
        habits_ . at habit_id .= Just extracted_habit
        pure $ redirectsToResult temporaryRedirect307 "/"
      else do
        log [i|Failed to update habit #{habit_id}:|]
        log [i|    Error message: #{error_message}|]
        deletion_mode ←
          use (habits_ . habit_map_)
          <&>
          (member habit_id >>> bool NoDeletion DeletionAvailable)
        habitPage habit_id error_message deletion_mode extracted_habit

handleDeleteHabitGet ∷ Environment → ScottyM ()
handleDeleteHabitGet environment = do
  Scotty.get "/delete/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request to delete habit with id #{habit_id}.|]
    maybe_habit ← use (habits_ . at habit_id)
    case maybe_habit of
      Nothing → pure $ redirectsToResult temporaryRedirect307 "/"
      Just habit → habitPage habit_id "" DeletionAvailable habit

handleDeleteHabitPost ∷ Environment → ScottyM ()
handleDeleteHabitPost environment = do
  Scotty.post "/delete/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request to delete habit with id #{habit_id}.|]
    confirm ∷ Int ← getParamMaybe "confirm" <&> fromMaybe 0
    if confirm == 1
      then do
        log [i|Deleting habit #{habit_id}|]
        habits_ . at habit_id .= Nothing
        pure $ redirectsToResult temporaryRedirect307 "/"
      else do
        log [i|Confirming delete for habit #{habit_id}|]
        (extracted_habit, error_message) ← extractHabit
        habitPage habit_id error_message ConfirmDeletion extracted_habit

handler ∷ Environment → ScottyM ()
handler environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
  handleDeleteHabitGet environment
  handleDeleteHabitPost environment
