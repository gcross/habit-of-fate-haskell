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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.MarkHabitAndRun (handler) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

runEvent ∷ TransactionProgram Lazy.Text
runEvent = do
  account ← get
  let (event, new_account) = runState runAccount account
  put new_account
  pure event

runGame ∷ TransactionProgram TransactionResult
runGame = do
  event ← runEvent
  let rendered_event
        | (not <<< onull) event = H.preEscapedLazyText event
        | otherwise = H.p $ H.toHtml ("Nothing happened." ∷ Text)
  marks ← use marks_
  renderTopOnlyPageResult "Habit of Fate - Event" ["story"] [] Nothing ok200 >>> pure $ do
    H.div ! A.class_ "story" $ rendered_event
    if null (marks ^. success_) && null (marks ^. failure_)
      then H.a ! A.href "/" $ H.toHtml ("Done" ∷ Text)
      else H.form ! A.method "post" $ H.input ! A.formaction "/run" ! A.type_ "submit" ! A.value "Next"

taggedLensForResult ∷ SuccessOrFailureResult → Lens' (Tagged α) α
taggedLensForResult SuccessResult = success_
taggedLensForResult FailureResult = failure_

scaleLensForResult ∷ SuccessOrFailureResult → Lens' Habit Scale
scaleLensForResult SuccessResult = difficulty_
scaleLensForResult FailureResult = importance_

markHabit ∷ SuccessOrFailureResult → UUID → TransactionProgram ()
markHabit result habit_id =
  use (habits_ . at habit_id)
  >>=
  maybe
    (raiseStatus 400 (printf "Cannot mark habit %s because it does not exist." $ show habit_id))
    (\habit → do
      marks_ . taggedLensForResult result %= (`snoc` (habit ^. scaleLensForResult result))
      case habit ^. frequency_ of
        Once _ → habits_ . at habit_id .= Nothing
        _ → do
          current_time ← getCurrentTimeAsLocalTime
          habits_ . at habit_id . _Just %=
            (
              (maybe_last_marked_ .~ Just current_time)
              >>>
              (frequency_ %~
                (\case
                  Repeated days_to_keep deadline repeated →
                    Repeated days_to_keep (nextDeadline repeated current_time deadline) repeated
                  other → other
                )
              )
            )
    )

markHabits ∷ [(SuccessOrFailureResult, UUID)] → TransactionProgram ()
markHabits = mapM_ $ uncurry markHabit

handleMarkHabitApi ∷ Environment → ScottyM ()
handleMarkHabitApi environment =
  Scotty.post "/api/marks" <<< apiTransaction environment $ do
    Tagged (Success success_habit_ids) (Failure failure_habit_ids) ← getBodyJSON
    log [i|Marking #{success_habit_ids} as successes and #{failure_habit_ids} as failures.|]
    markHabits $ map (SuccessResult,) success_habit_ids ⊕ map (FailureResult,) failure_habit_ids
    use marks_ <&> jsonResult ok200

handleMarkHabitWeb ∷ Environment → ScottyM ()
handleMarkHabitWeb environment = do
  Scotty.post "/habits/:habit_id/mark/success" $ mark SuccessResult
  Scotty.post "/habits/:habit_id/mark/failure" $ mark FailureResult
 where
  mark ∷ SuccessOrFailureResult → ActionM ()
  mark result = webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Marking #{habit_id} as #{result}.|]
    markHabit result habit_id
    marks ← use marks_
    log [i|MARKS = #{marks}|]
    runGame

handleRunApi ∷ Environment → ScottyM ()
handleRunApi environment =
  Scotty.post "/api/run" <<< apiTransaction environment $
    runEvent <&> lazyTextResult ok200

handleRunWeb ∷ Environment → ScottyM ()
handleRunWeb environment =
  Scotty.post "/run" <<< webTransaction environment $ runGame

handler ∷ Environment → ScottyM ()
handler environment = do
  handleMarkHabitApi environment
  handleMarkHabitWeb environment
  handleRunApi environment
  handleRunWeb environment
