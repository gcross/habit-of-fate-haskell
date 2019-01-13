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

module HabitOfFate.Server.Requests.Shared.MarkHabit (handler, markHabit) where

import HabitOfFate.Prelude

import Data.Time.LocalTime (LocalTime)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

resetDeadline ∷ LocalTime → Habit → Maybe Habit
resetDeadline current_time habit =
  case habit ^. frequency_ of
    Once _ → Nothing
    _ → habit
        |> (maybe_last_marked_ .~ Just current_time)
        |> (frequency_ %~
            (\case
              Repeated days_to_keep deadline repeated →
                Repeated days_to_keep (nextDeadline repeated current_time deadline) repeated
              other → other
            )
           )
        |> Just

markHabit ∷ UUID → Tagged Int → TransactionProgram ()
markHabit habit_id result_counts = do
  habit ← lookupHabit habit_id
  current_time ← getCurrentTimeAsLocalTime
  habits_ . at habit_id .= resetDeadline current_time habit
  forEachTaggedLens_ $ \lens_ →
    marks_ . lens_ ⊕= replicate (result_counts ^. lens_) (habit ^. scales_ . lens_)

markHabits ∷ [(UUID, Tagged Int)] → TransactionProgram ()
markHabits = mapM_ $ uncurry markHabit

handleApi ∷ Environment → ScottyM ()
handleApi environment =
  Scotty.post "/api/marks" <<< apiTransaction environment $ do
    marks ← getBodyJSON
    log [i|Marking: #{marks}|]
    markHabits marks
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
    markHabit habit_id (def & taggedLensForResult result .~ 1)
    marks ← use marks_
    pure $ redirectsToResult temporaryRedirect307 "/run"

handler ∷ Environment → ScottyM ()
handler environment = do
  handleApi environment
  handleWeb environment
