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
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.Deadlines (handler) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Tagged
import HabitOfFate.Server.Common
import HabitOfFate.Server.Requests.Shared.Deadlines
import HabitOfFate.Server.Requests.Shared.MarkHabit hiding (handler)
import HabitOfFate.Server.Requests.Shared.GetQuestStatus
import HabitOfFate.Server.Transaction

deadlinesPage ∷ Transaction TransactionResult
deadlinesPage = do
  deadlines ←
    getDeadlines
    <&>
    concatMap (
      \(habit_id, habit, deadlines) → map (habit_id, habit ^. name_,) deadlines
    )
  quest_status ← getQuestStatus
  renderPageResult "Passed Deadlines" (\_ → ["deadlines"]) [] Nothing ok200 >>> pure $ \device → do
    H.form ! A.method "post" $ do
      generateTopHTML device $ H.div ! A.class_ "story" $ renderMarkdownToHtml quest_status
      H.div ! A.class_ "deadlines" $ mconcat $
        map (\(class_ ∷ Text, column ∷ Text) → H.div ! A.class_ (H.toValue $ "header " ⊕ class_) $ H.toHtml column)
          [ ("", "Success")
          , ("", "Failure")
          , ("", "Skip")
          , ("", "Habit")
          , ("centered", "Deadline")
          ]
          ⊕
          concat
            [ let evenodd = if n `mod` 2 == 0 then "even" else "odd"
                  button ∷ Text → Text → Bool → H.Html
                  button label value checked = H.label $ do
                    H.input
                      ! A.class_ "status_radio"
                      ! A.type_ "radio"
                      ! A.name (H.toValue [i|status-#{habit_id}-#{n}|])
                      ! A.value (H.toValue value)
                      & if checked then (! A.checked "checked") else identity
                    H.div ! A.class_ (H.toValue [i|button #{label}|]) $ mempty
                  name_label = H.div ! A.class_ "name_area" $ H.div ! A.class_ "name" $ H.toHtml $ habit_name
                  deadline_label = H.div ! A.class_ "centered" $ renderLocalTime deadline_time
              in map (H.div >>> (! A.class_ evenodd))
              [ button "good" "+1" False
              , button "bad"  "-1" False
              , button "skip" " 0" True
              , name_label
              , deadline_label
              ]
            | (habit_id, habit_name, deadline_time) ← deadlines
            | n ← [0 ∷ Int ..]
            ]
      H.input ! A.type_ "submit"

handleGetDeadlines ∷ Environment → ScottyM ()
handleGetDeadlines environment =
  Scotty.get "/deadlines" $ webTransaction environment $ deadlinesPage

handlePostDeadlines ∷ Environment → ScottyM ()
handlePostDeadlines environment =
  Scotty.post "/deadlines" $ webTransaction environment $ do
    current_time ← getCurrentTimeAsLocalTime
    getParams >>= (mapM_ $ \(name, value) → when ("status-" `Lazy.isPrefixOf` name) (
      let possible_habit_id =
             name
               |> drop 7 -- length "status-"
               |> take 36 -- length of UUID
               |> Lazy.toStrict
      in case UUID.fromText possible_habit_id of
           Nothing → log [i|Unable to parse habit id: #{possible_habit_id}|]
           Just habit_id → case value of
             " 0" → markHabit False habit_id $ Tagged (Success 0) (Failure 0)
             "+1" → markHabit False habit_id $ Tagged (Success 1) (Failure 0)
             "-1" → markHabit False habit_id $ Tagged (Success 0) (Failure 1)
             other → log [i|Invalid habit deadline status value: #{other}|]
     ))
    habits_ %= fmap (flip updateHabitDeadline current_time)
    use marks_
      <&>
      (\case
        Tagged (Success s) (Failure f) | onull s && onull f → "/"
        _ → "/run"
       >>>
       redirectsToResult temporaryRedirect307
      )

handler ∷ Environment → ScottyM ()
handler environment = do
  handleGetDeadlines environment
  handlePostDeadlines environment
