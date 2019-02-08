{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.Run (handler) where

import HabitOfFate.Prelude

import Control.Monad.Random (uniform)
import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Quests
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction
import HabitOfFate.Substitution

runGame ∷ Transaction Markdown
runGame = do
  maybe_current_quest_state ← use maybe_current_quest_state_
  case maybe_current_quest_state of
    Nothing → do
      WrappedQuest quest ← uniform quests
      InitializeQuestResult quest_state intro_event ← questInitialize quest
      maybe_current_quest_state_ .= Just (questPrism quest # quest_state)
      pure intro_event
    Just current_quest_state → do
      let run ∷ ∀ s. Quest s → s → Transaction Markdown
          run quest quest_state = do
            marks ← use marks_
            maybe_runQuestTrial ←
              case uncons (marks ^. success_) of
                Just (scale, rest) → do
                  marks_ . success_ .= rest
                  pure $ Just $ questTrial >>> ($ SuccessResult) >>> ($ scale)
                Nothing →
                  case uncons (marks ^. failure_) of
                    Just (scale, rest) → do
                      marks_ . failure_ .= rest
                      pure $ Just $ questTrial >>> ($ FailureResult) >>> ($ scale)
                    Nothing →
                      pure Nothing
            case maybe_runQuestTrial of
              Just runQuestTrial → do
                (TryQuestResult quest_status event, new_quest_state) ←
                  quest
                    |> runQuestTrial
                    |> flip runStateT quest_state
                case quest_status of
                  QuestHasEnded result text → do
                    maybe_current_quest_state_ .= Nothing
                    when ← getCurrentTimeAsLocalTime
                    deeds_ %= (Deed result text when:)
                  QuestInProgress →
                    maybe_current_quest_state_ .= Just (new_quest_state ^. re (questPrism quest))
                pure event
              Nothing →
                questGetStatus quest quest_state
      runCurrentQuest run current_quest_state

handleApi ∷ Environment → ScottyM ()
handleApi environment =
  Scotty.post "/api/run" <<< apiTransaction environment $
    runGame <&> markdownResult ok200

handleWeb ∷ Environment → ScottyM ()
handleWeb environment = do
  Scotty.get "/run" <<< webTransaction environment $ action
  Scotty.post "/run" <<< webTransaction environment $ action
 where
  action =
    runGame
    >>=
    \event → do
      marks_are_present ← marksArePresent
      renderTopOnlyPageResult "Habit of Fate - Event" ["story"] [] Nothing ok200 >>> pure $ do
        H.div ! A.class_ "story" $ renderMarkdownToHtml event
        if marks_are_present
          then
            H.form
              ! A.action "/run"
              ! A.method "post"
              $ H.input
                  ! A.type_ "submit"
                  ! A.value "Next"
            else
            H.a
              ! A.href "/"
              $ H.toHtml ("Done" ∷ Text)

handler ∷ Environment → ScottyM ()
handler environment = do
  handleApi environment
  handleWeb environment
