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
import Network.HTTP.Types.Status (ok200)
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
  renderHTMLUsingTemplate "Habit of Fate - Editing a Habit" [] >>> returnLazyTextAsHTML ok200 $
    H.form ! A.method "post" $ do
      H.div $ H.table $ do
        H.tr $ do
          H.td $ H.toHtml ("Name:" ∷ Text)
          H.td $
            H.input
              ! A.id "name"
              ! A.type_ "text"
              ! A.name "name"
              ! A.value (H.toValue $ habit ^. name_)
              ! A.required "true"
          H.td ! A.id "name_error" $ H.toHtml name_error
        let generateScaleEntry name value_lens =
              H.select ! A.name name ! A.required "true" $
                flip foldMap scales $ \scale →
                  let addSelectedFlag
                        | habit ^. value_lens == scale = (! A.selected "selected")
                        | otherwise = identity
                      unselected_option = H.option ! A.value (scale |> show |> H.toValue)
                  in addSelectedFlag unselected_option $ H.toHtml (displayScale scale)
        H.tr $ do
          H.td $ H.toHtml ("Difficulty:" ∷ Text)
          H.td $ generateScaleEntry "difficulty" difficulty_
          H.td ! A.id "difficulty_error" $ H.toHtml difficulty_error
        H.tr $ do
          H.td $ H.toHtml ("Importance:" ∷ Text)
          H.td $ generateScaleEntry "importance" importance_
          H.td ! A.id "importance_error" $ H.toHtml importance_error
      H.div $ do
        H.input !  A.type_ "submit"
        H.a ! A.href "/habits" $ toHtml ("Cancel" ∷ Text)

handleEditHabitGet ∷ Environment → ScottyM ()
handleEditHabitGet environment = do
  Scotty.get "/habits/:habit_id" <<< webReader environment $ do
    habit_id ← getParam "habit_id"
    log $ [i|Web GET request for habit with id #{habit_id}.|]
    (view (habits_ . at habit_id) <&> fromMaybe def)
      >>= habitPage "" "" ""

handleEditHabitPost ∷ Environment → ScottyM ()
handleEditHabitPost environment = do
  Scotty.post "/habits/:habit_id" <<< webWriter environment $ do
    habit_id ← getParam "habit_id"
    log $ [i|Web POST request for habit with id #{habit_id}.|]
    (maybe_name, name_error) ←
      getParamMaybe "name"
      <&>
      maybe
        (Nothing, "No value for the name was present.")
        (\value → (Just (pack value), ""))
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
        redirectTo "/habits"

handleEditHabit ∷ Environment → ScottyM ()
handleEditHabit environment = do
  handleEditHabitGet environment
  handleEditHabitPost environment
