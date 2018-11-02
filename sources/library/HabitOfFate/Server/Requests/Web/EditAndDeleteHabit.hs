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

{-# LANGUAGE ImpredicativeTypes #-}
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

import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (LocalTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (Parsable, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

data DeletionMode = NoDeletion | DeletionAvailable | ConfirmDeletion

weekdays ∷ [(Text, Lazy.Text, Lens' DaysToRepeat Bool)]
weekdays =
  [ ("S", "sunday", sunday_)
  , ("M", "monday", monday_)
  , ("T", "tuesday", tuesday_)
  , ("W", "wednesday", wednesday_)
  , ("T", "thursday", thursday_)
  , ("F", "friday", friday_)
  , ("S", "saturday", saturday_)
  ]

habitPage ∷ Monad m ⇒ UUID → Maybe Lazy.Text → DeletionMode → Habit → Groups → m TransactionResult
habitPage habit_id maybe_error_message deletion_mode habit groups = do
  renderTopOnlyPageResult "Habit of Fate - Editing a Habit" ["edit"] ["edit"] ok200 >>> pure $
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

        H.div ! A.class_ "top_aligned_label" $ H.toHtml ("Frequency:" ∷ Text)

        H.div ! A.id "frequency_input" $ do
          H.div ! A.class_ "row" $ do
            H.input
              ! A.class_ "radio"
              ! A.type_ "radio"
              ! A.name "frequency"
              ! A.value "Indefinite"
              ! A.id "indefinite_radio"
              ! A.onclick "clickedIndefinite()"
              & if habit ^. frequency_ == Indefinite then (! A.checked "checked") else identity
            H.div ! A.onclick "document.getElementById(\"indefinite_radio\").click()" $
              H.toHtml ("Indefinite" ∷ Text)

          H.div ! A.class_ "row row_spacer" $ do
            H.input
              ! A.class_ "radio"
              ! A.type_ "radio"
              ! A.name "frequency"
              ! A.value "Once"
              ! A.id "once_radio"
              ! A.onclick "clickedOnce()"
              & case habit ^. frequency_ of {Once _ → (! A.checked "checked"); _ → identity}
            H.div ! A.class_ "row vertically_centered" $ do
              H.div
                ! A.class_ "label"
                ! A.onclick "document.getElementById(\"once_radio\").click()"
                $ H.toHtml ("Once" ∷ Text)
              H.div ! A.class_ "row right10px" $ do
                H.input
                  ! A.class_ "checkbox once_control"
                  ! A.type_ "checkbox"
                  ! A.name "once_has_deadline"
                  & case habit ^. frequency_ of
                      Once (Just _) → (! A.checked "checked")
                      _ → identity
                H.div ! A.class_ "once_control" $ H.toHtml ("With Deadline" ∷ Text)

          H.div ! A.class_ "row row_spacer" $ do
            H.input
              ! A.class_ "radio"
              ! A.type_ "radio"
              ! A.name "frequency"
              ! A.value "Repeated"
              ! A.id "repeated_radio"
              ! A.onclick "clickedRepeated()"
              & (case habit ^. frequency_ of
                  Repeated _ _ _ → (! A.checked "checked")
                  _ → identity
                )
            H.div ! A.onclick "document.getElementById(\"repeated_radio\").click()" $
              H.toHtml ("Repeated" ∷ Text)

          H.div ! A.class_ "indent" $ do
            let createBasicRepeatedControl ∷ Text → Text → Text → (Repeated → Maybe Int) → H.Html
                createBasicRepeatedControl label lowercase_label plural extractPeriod = do
                  let maybe_period =
                        case habit ^. frequency_ of
                          Repeated _ _ repeated → extractPeriod repeated
                          _ → Nothing
                  H.div ! A.class_ "row_spacer" $ do
                    H.div ! A.class_ "row" $ do
                      H.input
                        ! A.class_ "radio"
                        ! A.type_ "radio"
                        ! A.name "repeated"
                        ! A.value (H.toValue label)
                        & if isJust maybe_period then (! A.checked "checked") else identity
                      H.div ! A.class_ "label" $ H.toHtml label
                    H.div ! A.class_ "indent" $ do
                      H.div ! A.class_ "row row_spacer" $ do
                        H.div ! A.class_ "right5px" $ H.toHtml ("Every" ∷ Text)
                        H.input
                          ! A.class_ "period right5px"
                          ! A.type_ "number"
                          ! A.name (H.toValue $ lowercase_label ⊕ "_period")
                          ! A.value (H.toValue $ maybe "1" show maybe_period)
                          ! A.size "2"
                        H.toHtml (plural ⊕ "." ∷ Text)

            createBasicRepeatedControl
              "Daily" "daily" "days" (\case { Daily period → Just period; _ → Nothing })
            createBasicRepeatedControl
              "Weekly" "weekly" "weeks" (\case { Weekly period _ → Just period; _ → Nothing })

            H.div ! A.class_ "indent row row_spacer" $ do
              mconcat
                [ H.div ! A.class_ "column right10px" $ do
                    H.input
                      ! A.type_ "checkbox"
                      ! A.name (H.toValue weekday_name)
                      & (case habit ^. frequency_ of
                          Repeated _ _ (Weekly _ days_to_repeat)
                            | days_to_repeat ^. weekday_lens_ → (! A.checked "checked")
                          _ → identity
                        )
                    H.toHtml weekday_abbrev
                | (weekday_abbrev, weekday_name, weekday_lens_) ← weekdays
                ]

          H.div ! A.class_ "indent row double_row_spacer" $ do
            H.div ! A.class_ "label" $ H.toHtml ("Days to Keep:" ∷ Text)

            H.input
              ! A.class_ "period right5px"
              ! A.type_ "number"
              ! A.name (H.toValue ("days_to_keep" ∷ Text))
              ! A.value (
                  H.toValue $
                  case habit ^. frequency_ of
                    Repeated (KeepNumberOfDays n) _ _ → n
                    Repeated (KeepDaysInPast n) _ _ → n
                    _ → 3
                )
              ! A.size "2"

            H.div $ do
              H.div ! A.class_ "row" $ do
                H.input
                  ! A.class_ "radio"
                  ! A.type_ "radio"
                  ! A.name "days_to_keep_mode"
                  ! A.value "KeepNumberOfDays"
                  & case habit ^. frequency_ of
                      Repeated (KeepNumberOfDays _) _ _ → (! A.checked "checked")
                      _ → identity
                H.div ! A.class_ "label" $ H.toHtml ("in total" ∷ Text)

              H.div ! A.class_ "row" $ do
                H.input
                  ! A.class_ "radio"
                  ! A.type_ "radio"
                  ! A.name "days_to_keep_mode"
                  ! A.value "KeepDaysInPast"
                  & case habit ^. frequency_ of
                      Repeated (KeepDaysInPast _) _ _ → (! A.checked "checked")
                      _ → identity
                H.div ! A.class_ "label" $ H.toHtml ("into the past" ∷ Text)

        H.div ! A.class_ "label" $ H.toHtml ("(Next) Deadline:" ∷ Text)

        H.div $
          H.input
            ! A.type_ "datetime-local"
            ! A.name "deadline"
            ! A.value
                (H.toValue $ maybe "" (formatTime defaultTimeLocale "%FT%R") (getHabitDeadline habit))

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

      case maybe_error_message of
        Just error_message → H.div ! A.id "error_message" $ H.toHtml error_message
        Nothing → pure ()

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
      (uncurry (habitPage habit_id Nothing) $
        case maybe_habit of
          Nothing → (NoDeletion, def)
          Just habit → (DeletionAvailable, habit)
      )

type HabitExtractor = WriterT (First Lazy.Text) (StateT Habit TransactionProgram)
type HabitExtractorWithExceptT = ExceptT Lazy.Text HabitExtractor

extractHabit ∷ TransactionProgram (Habit, Maybe Lazy.Text)
extractHabit = do
  group_ids ∷ Set UUID ← use groups_ <&> ((^. items_seq_) >>> toList >>> setFromList)
  habit_id ← getParam "habit_id"
  default_habit ← use (habits_ . at habit_id) <&> fromMaybe def
  current_time_as_local_time ← getCurrentTimeAsLocalTime

  all_params ← getParams
  log [i|PARAMS = #{show all_params}|]

  (First maybe_first_error, new_habit) ← execWriterT >>> flip runStateT default_habit $ do
    let getParamMaybeLifted ∷ Parsable α ⇒ Lazy.Text → HabitExtractor (Maybe α)
        getParamMaybeLifted = getParamMaybe >>> lift >>> lift

        getParamMaybeLiftedReportingError ∷ Parsable α ⇒ Lazy.Text → Lazy.Text → (α → HabitExtractor ()) → HabitExtractor ()
        getParamMaybeLiftedReportingError param_name error_message f =
          getParamMaybeLifted param_name >>= maybe (reportError error_message) f

        reportError ∷ Lazy.Text → HabitExtractor ()
        reportError = Just >>> First >>> tell

    getParamMaybeLiftedReportingError
      "name"
      "No value for the name was present."
      (\value →
        if null value
          then reportError "Name for the habit may not be empty."
          else name_ .= pack value
      )

    let getScale ∷ Lazy.Text → Lens' Habit Scale → HabitExtractor ()
        getScale param_name param_lens_ =
          getParamMaybeLiftedReportingError
            param_name
            ("No value for the " ⊕ param_name ⊕ " was present.")
            (
              readMaybe
              >>>
              maybe
                (reportError $ "Invalid value for the " ⊕ param_name ⊕ ".")
                (param_lens_ .=)
            )
    getScale "difficulty" difficulty_; new_difficulty ← use difficulty_
    getScale "importance" importance_; new_importance ← use importance_

    when (new_difficulty == None && new_importance == None) $
      reportError "Either the difficulty or the importance must not be None."

    let getDeadline ∷ HabitExtractor (Either Lazy.Text LocalTime)
        getDeadline =
          getParamMaybeLifted "deadline"
          >>=
          (
            maybe
              (Left "Must specify the deadline.")
              (\case
                "" → Left "Must specify the deadline."
                deadline_string →
                  deadline_string
                    |> parseTimeM False defaultTimeLocale "%FT%R"
                    |> maybe
                        (Left $ pack $ "Error parsing deadline: " ⊕ deadline_string)
                        (\deadline →
                          if deadline < current_time_as_local_time
                            then Left "Deadline must not be in the past."
                            else Right deadline
                        )
              )
            >>>
            pure
          )

    getParamMaybeLiftedReportingError
      "frequency"
      "No value for the frequency was present."
      (\case
          ("Indefinite" ∷ Lazy.Text) → frequency_ .= Indefinite
          "Once" → do
            once_has_deadline ← getParamMaybeLifted "once_has_deadline" <&> maybe False (== ("on" ∷ Text))
            if once_has_deadline
              then getDeadline >>= either reportError (\deadline → frequency_ .= Once (Just deadline))
              else frequency_ .= Once Nothing
          "Repeated" → do
            let getParamMaybeExtraLifted ∷ Parsable α ⇒ Lazy.Text → HabitExtractorWithExceptT (Maybe α)
                getParamMaybeExtraLifted = getParamMaybe >>> lift >>> lift >>> lift
            runExceptT (
              getParamMaybeExtraLifted "repeated"
              >>=
              maybe
                (throwError "The frequency was set to Repeated, but neither of the options were chosen.")
                (\repeated_mode →
                  Repeated
                    <$> (getParamMaybeExtraLifted "days_to_keep"
                          >>=
                          maybe
                            (throwError "Frequency set to Repeated but Days to Keep not given.")
                            (
                              readMaybe
                              >>>
                              maybe
                                (throwError "Days to Keep was not a number.")
                                (\n →
                                  if n <= 0
                                    then throwError "Days to Keep must be a positive number."
                                    else
                                      getParamMaybeExtraLifted "days_to_keep_mode"
                                      >>=
                                      maybe
                                        (throwError "Neither mode for Days to Keep was chosen")
                                        (\case
                                          "KeepNumberOfDays" → pure $ KeepNumberOfDays n
                                          "KeepDaysInPast" → pure $ KeepDaysInPast n
                                          other → throwError $ "Invalid mode for Days to Keep: " ⊕ other
                                        )
                                )
                            )
                        )
                    <*> (lift getDeadline >>= either throwError pure)
                    <*> let getPeriod ∷ Lazy.Text → HabitExtractorWithExceptT Int
                            getPeriod name =
                              getParamMaybeExtraLifted name
                              >>=
                              maybe
                                (throwError "No value for the period was present.")
                                (\value →
                                  if null value
                                    then throwError "The period must not be empty."
                                    else
                                      maybe
                                        (throwError "The period must be a number.")
                                        pure
                                        (readMaybe value)
                                )
                        in case (repeated_mode ∷ Lazy.Text) of
                            "Daily" → Daily <$> getPeriod "daily_period"
                            "Weekly" → do
                              period ← getPeriod "weekly_period"
                              days_to_repeat ←
                                mapM (\(_, key, _) → getParamMaybeExtraLifted key) weekdays
                                <&>
                                (
                                  zip weekdays
                                  >>>
                                  foldl'
                                    (\days_to_repeat ((_, _, lens_ ∷ Lens' DaysToRepeat Bool), maybe_value) →
                                      maybe
                                        days_to_repeat
                                        (\value →
                                          if value == ("on" ∷ Text)
                                            then days_to_repeat & lens_ .~ True
                                            else days_to_repeat
                                        )
                                        maybe_value
                                    )
                                    def
                                )
                              when (days_to_repeat == def) $
                                throwError "No days of the week were marked to be repeated."
                              pure $ Weekly period days_to_repeat
                            other → throwError $ "Repeated must be Daily or Weekly, not " ⊕ other ⊕ "."
                )
              ∷ HabitExtractorWithExceptT Frequency) >>= either reportError (frequency_ .=)
          other → reportError $ "Frequency must be Indefinite, Once, or Repeated, not " ⊕ other ⊕ "."
      )

    -- group membership
    (getParams |> lift |> lift)
      >>=
      mapM (
        \(key, value) →
          case key of
            "group" →
              value
              |> Lazy.toStrict
              |> UUID.fromText
              |> maybe
                  (reportError ("Group UUID has invalid format: " ⊕ value) >> pure Nothing)
                  (\group_id →
                     if member group_id group_ids
                       then pure $ Just group_id
                       else reportError ("Group with UUID " ⊕ value ⊕ " does not exist.") >> pure Nothing
                  )
            _ → pure Nothing
      )
      >>=
      (catMaybes >>> setFromList >>> pure)
      >>=
      (group_membership_ .=)
  pure (new_habit, maybe_first_error)

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/habits/:habit_id" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request for habit with id #{habit_id}.|]
    (extracted_habit, maybe_error_message) ← extractHabit
    case maybe_error_message of
      Nothing → do
        log [i|Updating habit #{habit_id} to #{extracted_habit}|]
        habits_ . at habit_id .= Just extracted_habit
        pure $ redirectsToResult temporaryRedirect307 "/habits"
      Just error_message → do
        log [i|Failed to update habit #{habit_id}:|]
        log [i|    Error message: #{error_message}|]
        deletion_mode ←
          use (habits_ . items_map_)
          <&>
          (member habit_id >>> bool NoDeletion DeletionAvailable)
        use groups_ >>= habitPage habit_id (Just error_message) deletion_mode extracted_habit

handleDeleteHabitGet ∷ Environment → ScottyM ()
handleDeleteHabitGet environment = do
  Scotty.get "/habits/:habit_id/delete" <<< webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request to delete habit with id #{habit_id}.|]
    maybe_habit ← use (habits_ . at habit_id)
    case maybe_habit of
      Nothing → pure $ redirectsToResult temporaryRedirect307 "/habits"
      Just habit → use groups_ >>= habitPage habit_id Nothing DeletionAvailable habit

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
        (extracted_habit, maybe_error_message) ← extractHabit
        use groups_ >>= habitPage habit_id maybe_error_message ConfirmDeletion extracted_habit

handler ∷ Environment → ScottyM ()
handler environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
  handleDeleteHabitGet environment
  handleDeleteHabitPost environment
