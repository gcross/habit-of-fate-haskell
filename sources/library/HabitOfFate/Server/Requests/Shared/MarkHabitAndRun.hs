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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.MarkHabitAndRun (handler) where

import HabitOfFate.Prelude

import Data.UUID (UUID)
import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged
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
    if stored_credits ^. success_ /= 0 || stored_credits ^. failure_ /= 0
      then H.form ! A.method "post" $ H.input ! A.formaction "/run" ! A.type_ "submit" ! A.value "Next"
      else H.a ! A.href "/" $ H.toHtml ("Done" ∷ Text)

data SuccessOrFailureResult = SuccessResult | FailureResult deriving (Enum, Eq, Read, Show, Ord)

creditLensForResult ∷ SuccessOrFailureResult → Lens' (Tagged Double) Double
creditLensForResult SuccessResult = success_
creditLensForResult FailureResult = failure_

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
      stored_credits_ . creditLensForResult result += scaleFactor (habit ^. scaleLensForResult result)
      case habit ^. frequency_ of
        Once → habits_ . at habit_id .= Nothing
        Indefinite → do
          new_last_marked ← getCurrentTimeAsLocalTime
          habits_ . at habit_id .= Just (habit & maybe_last_marked_ .~ Just new_last_marked)
    )

markHabits ∷ [(SuccessOrFailureResult, UUID)] → TransactionProgram ()
markHabits = mapM_ $ uncurry markHabit

handleMarkHabitApi ∷ Environment → ScottyM ()
handleMarkHabitApi environment =
  Scotty.post "/api/mark" <<< apiTransaction environment $ do
    Tagged (Success success_habit_ids) (Failure failure_habit_ids) ← getBodyJSON
    log [i|Marking #{success_habit_ids} as successes and #{failure_habit_ids} as failures.|]
    markHabits $ map (SuccessResult,) success_habit_ids ⊕ map (FailureResult,) failure_habit_ids
    use stored_credits_ <&> jsonResult ok200

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
