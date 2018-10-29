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

{-# LANGUAGE LambdaCase #-}
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

import qualified Data.Text.Lazy as Lazy
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Scale
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

data DeletionMode = NoDeletion | DeletionAvailable | ConfirmDeletion

habitPage ∷ Monad m ⇒ UUID → Lazy.Text → DeletionMode → Habit → Groups → m TransactionResult
habitPage habit_id error_message deletion_mode habit groups = do
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

        H.div ! A.class_ "label" $ H.toHtml ("Frequency:" ∷ Text)

        H.div ! A.id "frequency_input" $ do
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
            & case habit ^. frequency_ of {Once _ → (! A.checked "checked"); _ → identity}
          H.div ! A.class_ "once" $ do
            H.div ! A.class_ "once_label" $ H.toHtml ("Once" ∷ Text)
            H.div $ do
              H.input
                ! A.type_ "checkbox"
                ! A.name "once_has_deadline"
                & case habit ^. frequency_ of { Once True → (! A.checked "checked"); _ → identity }
              H.toHtml ("With Deadline" ∷ Text)

        H.div ! A.class_ "label" $ H.toHtml ("(Next) Deadline:" ∷ Text)

        H.div $
          H.input
            ! A.type_ "datetime-local"
            ! A.name "deadline"
            ! A.value
                (H.toValue $
                 maybe
                  ""
                  (formatTime defaultTimeLocale "%FT%R")
                  (habit ^. maybe_deadline_)
                )

        H.div ! A.class_ "label" $ H.toHtml ("Groups:" ∷ Text)

        H.div ! A.id "group_input" $ do
          forM_ (groups ^. items_list_) $ \(group_id, group_name) → do
            H.input
              ! A.type_ "checkbox"
              ! A.name (H.toValue ("group" ∷ Text))
              ! A.value (H.toValue $ UUID.toText group_id)
              & if member group_id (habit ^. group_membership_) then (! A.checked "checked") else identity
            H.toHtml group_name

      H.hr

      H.div ! A.id "error_message" $ H.toHtml error_message

      H.div ! A.class_ "submit" $ do
        H.a ! A.class_ "sub" ! A.href "/habits" $ toHtml ("Cancel" ∷ Text)
        H.input
          ! A.class_ "sub"
          ! A.formaction (H.toValue [i|/habits/#{UUID.toText habit_id}|])
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
              ! A.formaction (H.toValue [i|/habits/#{UUID.toText habit_id}/delete|])
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
              ! A.formaction (H.toValue [i|/habits/#{UUID.toText habit_id}/delete|])
              ! A.value "Confirm Delete?"

handleEditHabitGet ∷ Environment → ScottyM ()
handleEditHabitGet environment = do
  Scotty.get "/habits/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request for habit with id #{habit_id}.|]
    maybe_habit ← use (habits_ . at habit_id)
    use groups_ >>=
      (uncurry (habitPage habit_id "") $
        case maybe_habit of
          Nothing → (NoDeletion, def)
          Just habit → (DeletionAvailable, habit)
      )

extractHabit ∷ TransactionProgram (Habit, Lazy.Text)
extractHabit = do
  group_ids ∷ Set UUID ← use groups_ <&> ((^. items_seq_) >>> toList >>> setFromList)
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
  once_has_deadline ← getParamMaybe "once_has_deadline" <&> maybe False (== ("on" ∷ Text))
  (frequency_value, frequency_error) ←
    getParamMaybe "frequency"
    <&>
    maybe
      (default_habit ^. frequency_, "No value for the frequency was present.")
      (\value →
        case value of
          "Indefinite" → (Indefinite, "")
          "Once" → (Once once_has_deadline, "")
          _ → (Indefinite, "Frequency must be Indefinite or Once, not " ⊕ value)
      )
  current_time_as_local_time ← getCurrentTimeAsLocalTime
  (maybe_deadline_value, maybe_deadline_error) ←
    getParamMaybe "deadline"
    <&>
    maybe
      (default_habit ^. maybe_deadline_, "")
      (\case
        "" → (Nothing, "")
        deadline_string →
          deadline_string
            |> parseTimeM False defaultTimeLocale "%FT%R"
            |> maybe
                (Nothing, pack $ "Error parsing deadline: " ⊕ deadline_string)
                (\deadline →
                   ( Just deadline
                   , if deadline < current_time_as_local_time
                       then "Deadline must not be in the past."
                       else ""
                   )
                 )
      )
  let deadline_required_error =
        case (frequency_value, maybe_deadline_value) of
          (Once True, Nothing) → "Must specify the deadline."
          _ → ""
  all_params ← getParams
  log [i|PARAMS = #{show all_params}|]
  (group_membership_error ∷ Lazy.Text, group_membership_value ∷ Set UUID) ←
    getParams
    <&>
    (
      mapMaybe (
        \(key, value) →
          case key of
            "group" →
              value
              |> Lazy.toStrict
              |> UUID.fromText
              |> maybe
                  (Just $ Left ("Group UUID has invalid format: " ⊕ value))
                  (\group_id → Just $
                     if member group_id group_ids
                       then Right group_id
                       else Left ("Group with UUID " ⊕ value ⊕ " does not exist.")
                  )
            _ → Nothing
      )
      >>>
      partitionEithers
      >>>
      (\case { error_message:_ → error_message; _ → ""} *** setFromList)
    )
  pure
    ( Habit
        name_value
        (Difficulty difficulty_value)
        (Importance importance_value)
        frequency_value
        group_membership_value
        (default_habit ^. maybe_last_marked_)
        (case frequency_value of { Once True → maybe_deadline_value; _ → Nothing })
    , find (onull >>> not) >>> fromMaybe "" $
       [ name_error
       , difficulty_error
       , importance_error
       , irrelevant_error
       , frequency_error
       , group_membership_error
       , maybe_deadline_error
       , deadline_required_error
       ]
    )

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/habits/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request for habit with id #{habit_id}.|]
    (extracted_habit, error_message) ← extractHabit
    if onull error_message
      then do
        log [i|Updating habit #{habit_id} to #{extracted_habit}|]
        habits_ . at habit_id .= Just extracted_habit
        pure $ redirectsToResult temporaryRedirect307 "/habits"
      else do
        log [i|Failed to update habit #{habit_id}:|]
        log [i|    Error message: #{error_message}|]
        deletion_mode ←
          use (habits_ . items_map_)
          <&>
          (member habit_id >>> bool NoDeletion DeletionAvailable)
        use groups_ >>= habitPage habit_id error_message deletion_mode extracted_habit

handleDeleteHabitGet ∷ Environment → ScottyM ()
handleDeleteHabitGet environment = do
  Scotty.get "/habits/:habit_id/delete" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request to delete habit with id #{habit_id}.|]
    maybe_habit ← use (habits_ . at habit_id)
    case maybe_habit of
      Nothing → pure $ redirectsToResult temporaryRedirect307 "/habits"
      Just habit → use groups_ >>= habitPage habit_id "" DeletionAvailable habit

handleDeleteHabitPost ∷ Environment → ScottyM ()
handleDeleteHabitPost environment = do
  Scotty.post "/habits/:habit_id/delete" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request to delete habit with id #{habit_id}.|]
    confirm ∷ Int ← getParamMaybe "confirm" <&> fromMaybe 0
    if confirm == 1
      then do
        log [i|Deleting habit #{habit_id}|]
        habits_ . at habit_id .= Nothing
        pure $ redirectsToResult temporaryRedirect307 "/habits"
      else do
        log [i|Confirming delete for habit #{habit_id}|]
        (extracted_habit, error_message) ← extractHabit
        use groups_ >>= habitPage habit_id error_message ConfirmDeletion extracted_habit

handler ∷ Environment → ScottyM ()
handler environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
  handleDeleteHabitGet environment
  handleDeleteHabitPost environment
