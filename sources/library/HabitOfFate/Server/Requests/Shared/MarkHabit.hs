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

module HabitOfFate.Server.Requests.Shared.MarkHabit (handler, markHabits) where

import HabitOfFate.Prelude

import Data.Time.LocalTime (LocalTime)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Mark
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

resetDeadline ∷ Bool → LocalTime → Habit → Maybe Habit
resetDeadline advance_deadline current_time habit =
  case habit ^. frequency_ of
    Once _ → Nothing
    _ → habit
        |> (maybe_last_marked_ .~ Just current_time)
        |> (frequency_ %~
            (\case
              Repeated days_to_keep deadline repeated →
                let upcoming_deadline = nextDeadline repeated current_time deadline
                    next_deadline
                      | advance_deadline = nextDeadline repeated deadline deadline
                      | otherwise = upcoming_deadline
                in Repeated days_to_keep next_deadline repeated
              other → other
            )
           )
        |> Just

markHabits ∷ Bool → [(UUID, [SuccessOrFailureResult])] → Transaction ()
markHabits advance_deadline habits_and_results = do
  current_time ← getCurrentTimeAsLocalTime
  new_marks ← forM habits_and_results $ \(habit_id, results) → do
    habit ← lookupHabit habit_id
    unless (null results) $
      habits_ . at habit_id .= resetDeadline advance_deadline current_time habit
    let results_to_consider = case habit ^. frequency_ of
          Once _ → take 1 results
          _ → results
    pure
      [ Mark result $ habit ^. scales_ . taggedLensForResult result
      | result ← results_to_consider ]
  marks_ %= (⊕ fromList (concat new_marks))

handleApi ∷ Environment → ScottyM ()
handleApi environment =
  Scotty.post "/api/marks" <<< apiTransaction environment $ do
    marks ← getBodyJSON
    log [i|Marking: #{marks}|]
    markHabits False marks
    use marks_ <&> jsonResult ok200

handleWeb ∷ Environment → ScottyM ()
handleWeb environment = do
  Scotty.post "/habits/:habit_id/mark/success" $ mark SuccessResult
  Scotty.post "/habits/:habit_id/mark/failure" $ mark FailureResult
 where
  mark ∷ SuccessOrFailureResult → ActionM ()
  mark result = webTransaction environment $ do
    habit_id ← getParam "habit_id"
    log [i|Marking #{habit_id} as #{result}.|]
    markHabits True [(habit_id, [result])]
    marks ← use marks_
    pure $ redirectsToResult temporaryRedirect307 "/run"

handler ∷ Environment → ScottyM ()
handler environment = do
  handleApi environment
  handleWeb environment
