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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.EditHabit (handleEditHabit) where

import HabitOfFate.Prelude

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

habitPage ∷ Monad m ⇒ Lazy.Text → Lazy.Text → Lazy.Text → Habit → m TransactionResult
habitPage name_error difficulty_error importance_error habit =
  renderHTMLUsingTemplateAndReturn "Habit of Fate - Editing a Habit" ["common", "edit"] ok200 $
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
        let generateScaleEntry name value_lens =
              H.select ! A.name name ! A.required "true" ! A.id (name ⊕ "_input") $
                flip foldMap scales $ \scale →
                  let addSelectedFlag
                        | habit ^. value_lens == scale = (! A.selected "selected")
                        | otherwise = identity
                      unselected_option = H.option ! A.value (scale |> show |> H.toValue)
                  in addSelectedFlag unselected_option $ H.toHtml (displayScale scale)

        -- Difficulty
        H.div ! A.class_ "label" ! A.id "difficulty_label" $ H.toHtml ("Difficulty:" ∷ Text)
        generateScaleEntry "difficulty" difficulty_
        H.div ! A.class_ "error_message" ! A.id "difficulty_error" $ H.toHtml difficulty_error

        -- Importance
        H.div ! A.class_ "label" ! A.id "importance_label" $ H.toHtml ("Importance:" ∷ Text)
        generateScaleEntry "importance" importance_
        H.div ! A.class_ "error_message" ! A.id "importance_error" $ H.toHtml importance_error

      H.hr

      H.div ! A.class_ "submit" $ do
        H.a ! A.class_ "sub" ! A.href "/habits" $ toHtml ("Cancel" ∷ Text)
        H.input ! A.class_ "sub" ! A.type_ "submit"

handleEditHabitGet ∷ Environment → ScottyM ()
handleEditHabitGet environment = do
  Scotty.get "/habits/:habit_id" <<< webReader environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web GET request for habit with id #{habit_id}.|]
    (view (habits_ . at habit_id) <&> fromMaybe def)
      >>= habitPage "" "" ""

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/habits/:habit_id" <<< webWriter environment $ do
    habit_id ← getParam "habit_id"
    log [i|Web POST request for habit with id #{habit_id}.|]
    (maybe_name, name_error) ←
      getParamMaybe "name"
      <&>
      maybe
        (Nothing, "No value for the name was present.")
        (\value →
          if null value
            then (Nothing, "Name for the habit may not be empty.")
            else (Just (pack value), "")
        )
    let getScale param_name =
          getParamMaybe param_name
          <&>
          maybe
            (Nothing, "No value for the " ⊕ param_name ⊕ " was present.")
            (
              readMaybe
              >>>
              maybe
                (Nothing, "Invalid value for the " ⊕ param_name ⊕ ".")
                (\value → (Just value, ""))
            )
    (maybe_difficulty, difficulty_error) ← getScale "difficulty"
    (maybe_importance, importance_error) ← getScale "importance"
    case Habit <$> maybe_name <*> (Difficulty <$> maybe_difficulty) <*> (Importance <$> maybe_importance) of
      Nothing → do
        log [i|Failed to update habit #{habit_id}:|]
        log [i|    Name error: #{name_error}|]
        log [i|    Difficulty error: #{difficulty_error}|]
        log [i|    Importance error: #{importance_error}|]
        habitPage name_error difficulty_error importance_error def
      Just new_habit → do
        log [i|Updating habit #{habit_id} to #{new_habit}|]
        habits_ . at habit_id .= Just new_habit
        redirectTo temporaryRedirect307 "/habits"

handleEditHabit ∷ Environment → ScottyM ()
handleEditHabit environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
