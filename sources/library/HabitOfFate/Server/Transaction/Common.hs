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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Transaction.Common where

import HabitOfFate.Prelude

import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Text.Lazy as Lazy
import Data.UUID (UUID)
import Network.HTTP.Types.Status (Status(..))
import Text.Blaze.Html5 (Html)
import Web.Scotty (Param, Parsable(parseParam))

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common
import HabitOfFate.Story (Event)
import HabitOfFate.Story.Renderer.HTML (renderEventToHTML)

data CommonInstruction α where
  GetBodyInstruction ∷ CommonInstruction LazyBS.ByteString
  GetParamsInstruction ∷ CommonInstruction [Param]
  RaiseStatusInstruction ∷ Status → CommonInstruction α
  LogInstruction ∷ String → CommonInstruction ()

class Monad m ⇒ ActionMonad m where
  singletonCommon ∷ CommonInstruction α → m α

getBody ∷ ActionMonad m ⇒ m LazyBS.ByteString
getBody = singletonCommon GetBodyInstruction

getBodyJSON ∷ (FromJSON α, ActionMonad m) ⇒ m α
getBodyJSON = do
  body ← getBody
  case eitherDecode' $ body of
    Left message → do
      log [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body}|]
      raiseStatus 400 "Bad request: Invalid JSON"
    Right json → pure json

getParams ∷ ActionMonad m ⇒ m [Param]
getParams = singletonCommon GetParamsInstruction

getParam ∷ (Parsable α, ActionMonad m) ⇒ Lazy.Text → m α
getParam param_name = do
  params_ ← getParams
  case lookup param_name params_ of
    Nothing → raiseStatus 400 [i|Bad request: Missing parameter %{param_name}|]
    Just value → case parseParam value of
      Left _ →
        raiseStatus
          400
          [i|Bad request: Parameter #{param_name} has invalid format #{value}|]
      Right x → return x

getParamMaybe ∷ (Parsable α, ActionMonad m) ⇒ Lazy.Text → m (Maybe α)
getParamMaybe param_name =
  getParams
  <&>
  (lookup param_name >=> (parseParam >>> either (const Nothing) return))

raiseStatus ∷ ActionMonad m ⇒ Int → String → m α
raiseStatus code =
  pack
  >>>
  encodeUtf8
  >>>
  Status code
  >>>
  RaiseStatusInstruction
  >>>
  singletonCommon

raiseNoSuchHabit ∷ ActionMonad m ⇒ m α
raiseNoSuchHabit = raiseStatus 404 "Not found: No such habit"

log ∷ ActionMonad m ⇒ String → m ()
log = LogInstruction >>> singletonCommon

lookupHabit ∷ (ActionMonad m, MonadState Account m) ⇒ UUID → m Habit
lookupHabit habit_id = do
  use (habits_ . at habit_id)
  >>=
  maybe raiseNoSuchHabit return

data TransactionResult = RedirectsTo Lazy.Text | TransactionResult Status Content

returnNothing ∷ Monad m ⇒ Status → m TransactionResult
returnNothing s = return $ TransactionResult s NoContent

returnLazyText ∷ Monad m ⇒ Status → Lazy.Text → m TransactionResult
returnLazyText s = TextContent >>> TransactionResult s >>> return

returnLazyTextAsHTML ∷ Monad m ⇒ Status → Lazy.Text → m TransactionResult
returnLazyTextAsHTML s = TextContentAsHTML >>> TransactionResult s >>> return

returnJSON ∷ (ToJSON α, Monad m) ⇒ Status → α → m TransactionResult
returnJSON s = JSONContent >>> TransactionResult s >>> return

redirectTo ∷ Monad m ⇒ Lazy.Text → m TransactionResult
redirectTo = RedirectsTo >>> return

renderHTMLUsingTemplateAndReturn ∷ Monad m ⇒ Text → [Text] → Status → Html → m TransactionResult
renderHTMLUsingTemplateAndReturn title stylesheets status =
  renderHTMLUsingTemplate title stylesheets
  >>>
  returnLazyTextAsHTML status

renderEventToHTMLAndReturn ∷ Monad m ⇒ Text → [Text] → Status → Event → m TransactionResult
renderEventToHTMLAndReturn title stylesheets status =
  renderEventToHTML
  >>>
  renderHTMLUsingTemplateAndReturn title stylesheets status
