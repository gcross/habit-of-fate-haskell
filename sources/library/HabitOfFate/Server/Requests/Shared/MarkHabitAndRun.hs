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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.MarkHabitAndRun (handler) where

import HabitOfFate.Prelude

import Data.UUID (UUID)
import Network.HTTP.Types.Status (notFound404, ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Credits
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction
import HabitOfFate.Story
import HabitOfFate.Story.Renderer.HTML
import HabitOfFate.Story.Renderer.XML

runEvent ∷ TransactionProgram Event
runEvent = do
  account ← get
  let (event, new_account) = runState runAccount account
  put new_account
  pure event

runGame ∷ TransactionProgram TransactionResult
runGame = do
  event ← runEvent
  let rendered_event
        | (not <<< null) event = renderEventToHTML event
        | otherwise = H.p $ H.toHtml ("Nothing happened." ∷ Text)
  stored_credits ← use stored_credits_
  renderTopOnlyPageResult "Habit of Fate - Event" ["story"] ok200 >>> pure $ do
    H.div ! A.class_ "story" $ rendered_event
    if stored_credits ^. successes_ /= 0 || stored_credits ^. failures_ /= 0
      then H.form ! A.method "post" $ H.input ! A.type_ "submit" ! A.value "Next"
      else H.a ! A.href "/" $ H.toHtml ("Done" ∷ Text)

handleMarkHabitApi ∷ Environment → ScottyM ()
handleMarkHabitApi environment =
  Scotty.post "/api/mark" <<< apiTransaction environment $ do
    marks ← getBodyJSON

    let markHabits ∷
          Getter HabitsToMark [UUID] →
          Getter Habit Scale →
          Lens' Account Double →
          TransactionProgram Double
        markHabits uuids_getter scale_getter value_lens = do
          old_value ← use value_lens
          increment ∷ Double ←
            marks
              |> (^. uuids_getter)
              |> mapM (lookupHabit >>> fmap ((^. scale_getter) >>> scaleFactor))
              |> fmap sum
          value_lens <.= old_value + increment

    log [i|Marking #{marks ^. succeeded_} successes and #{marks ^. failed_} failures.|]
    (Credits
        <$> (Successes <$> markHabits succeeded_ difficulty_ (stored_credits_ . successes_))
        <*> (Failures  <$> markHabits failed_    importance_ (stored_credits_ . failures_ ))
      ) <&> jsonResult ok200

handleMarkHabitWeb ∷ Environment → ScottyM ()
handleMarkHabitWeb environment = do
  Scotty.post "/mark/success/:habit_id" $ markHabit "succeeded" (difficulty_ . to scaleFactor) successes_
  Scotty.post "/mark/failure/:habit_id" $ markHabit "failed"(importance_ . to scaleFactor) failures_
 where
  markHabit ∷ String → Getter Habit Double → Lens' Credits Double → ActionM ()
  markHabit status habit_scale_getter_ credits_lens_ = webTransaction environment $ do
    habits ← use habits_
    habit_id ← getParam "habit_id"
    log [i|Marking #{habit_id} as #{status}.|]
    case habits ^. at habit_id of
      Nothing →
        renderTopOnlyPageResult "Habit of Fate - Marking a Habit" [] notFound404 >>> pure $ do
          H.h1 "Habit Not Found"
          H.p $ H.toHtml [i|"Habit #{habit_id} was not found.|]
      Just habit → do
        stored_credits_ . credits_lens_ += habit ^. habit_scale_getter_
        runGame

handleRunApi ∷ Environment → ScottyM ()
handleRunApi environment =
  Scotty.post "/api/run" <<< apiTransaction environment $ do
    runEvent >>= (renderEventToXMLText >>> lazyTextResult ok200 >>> pure)

handleRunWeb ∷ Environment → ScottyM ()
handleRunWeb environment =
  Scotty.post "/run" <<< webTransaction environment $ runGame

handler ∷ Environment → ScottyM ()
handler environment = do
  handleMarkHabitApi environment
  handleMarkHabitWeb environment
  handleRunApi environment
  handleRunWeb environment
