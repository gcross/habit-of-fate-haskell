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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Transaction where

import HabitOfFate.Prelude

import Control.Concurrent (tryPutMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.DeepSeq (NFData)
import qualified Control.Monad.Operational as Operational
import Control.Monad.Random (RandT, StdGen, evalRandT, newStdGen)
import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime (LocalTime)
import Data.Time.Zones (utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByLabel)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (Status(..), temporaryRedirect307)
import Text.Blaze.Html5 (Html)
import Web.Scotty (ActionM, Param, Parsable(parseParam))
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Logging
import HabitOfFate.Server.Actions.Queries (authorizeWith)
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common

data TransactionInstruction α where
  GetAccountInstruction ∷ TransactionInstruction Account
  PutAccountInstruction ∷ Account → TransactionInstruction ()
  GetBodyInstruction ∷ TransactionInstruction LazyBS.ByteString
  GetParamsInstruction ∷ TransactionInstruction [Param]
  RaiseStatusInstruction ∷ Status → TransactionInstruction α
  LogInstruction ∷ String → TransactionInstruction ()
  GetCurrentTimeInstruction ∷ TransactionInstruction UTCTime

newtype TransactionProgram α = TransactionProgram
  { unwrapTransactionProgram ∷ Operational.Program TransactionInstruction α }
  deriving
  ( Applicative
  , Functor
  , Monad
  )

wrapTransactionInstruction ∷ TransactionInstruction α → TransactionProgram α
wrapTransactionInstruction = Operational.singleton >>> TransactionProgram

instance MonadState Account TransactionProgram where
  get = wrapTransactionInstruction GetAccountInstruction
  put = PutAccountInstruction >>> wrapTransactionInstruction

getBody ∷ TransactionProgram LazyBS.ByteString
getBody = wrapTransactionInstruction GetBodyInstruction

getBodyJSON ∷ FromJSON α ⇒ TransactionProgram α
getBodyJSON = do
  body ← getBody
  case eitherDecode' $ body of
    Left message → do
      log [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body}|]
      raiseStatus 400 "Bad request: Invalid JSON"
    Right json → pure json

getParams ∷ TransactionProgram [Param]
getParams = wrapTransactionInstruction GetParamsInstruction

convertUTCToLocalTime ∷ UTCTime → TransactionProgram LocalTime
convertUTCToLocalTime utc_time =
  utcToLocalTimeTZ
    <$> (tzByLabel <$> use timezone_)
    <*> pure utc_time

getCurrentTime ∷ TransactionProgram UTCTime
getCurrentTime = wrapTransactionInstruction GetCurrentTimeInstruction

getCurrentTimeAsLocalTime ∷ TransactionProgram LocalTime
getCurrentTimeAsLocalTime = getCurrentTime >>= convertUTCToLocalTime

getParam ∷ Parsable α ⇒ Lazy.Text → TransactionProgram α
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

getParamMaybe ∷ Parsable α ⇒ Lazy.Text → TransactionProgram (Maybe α)
getParamMaybe param_name =
  getParams
  <&>
  (lookup param_name >=> (parseParam >>> either (const Nothing) return))

getParamDefault ∷ Parsable α ⇒ Lazy.Text → α → TransactionProgram α
getParamDefault param_name d = getParamMaybe param_name <&> fromMaybe d

raiseStatus ∷ Int → String → TransactionProgram α
raiseStatus code =
  pack
  >>>
  encodeUtf8
  >>>
  Status code
  >>>
  RaiseStatusInstruction
  >>>
  wrapTransactionInstruction

raiseNoSuchHabit ∷ TransactionProgram α
raiseNoSuchHabit = raiseStatus 404 "Not found: No such habit"

log ∷ String → TransactionProgram ()
log = LogInstruction >>> wrapTransactionInstruction

getLastSeenAsLocalTime ∷ TransactionProgram LocalTime
getLastSeenAsLocalTime = use last_seen_ >>= convertUTCToLocalTime

lookupHabit ∷ UUID → TransactionProgram Habit
lookupHabit habit_id =
  use (habits_ . at habit_id)
  >>=
  maybe raiseNoSuchHabit return

data TransactionResult = RedirectsTo Status Lazy.Text | TransactionResult Status Content

noContentResult ∷ Status → TransactionResult
noContentResult = flip TransactionResult NoContent

lazyTextResult ∷ Status → Lazy.Text → TransactionResult
lazyTextResult s = TextContent >>> TransactionResult s

lazyTextAsHTMLResult ∷ Status → Lazy.Text → TransactionResult
lazyTextAsHTMLResult s = TextContentAsHTML >>> TransactionResult s

jsonResult ∷ (NFData α, ToJSON α) ⇒ Status → α → TransactionResult
jsonResult s = JSONContent >>> TransactionResult s

redirectsToResult ∷ Status → Lazy.Text → TransactionResult
redirectsToResult status_ url = RedirectsTo status_ url

renderPageResult ∷ Text → [Text] → [Text] → Maybe Text → Status → Html → TransactionResult
renderPageResult title stylesheets scripts maybe_onload status =
  renderPage title stylesheets scripts maybe_onload
  >>>
  lazyTextAsHTMLResult status

renderTopOnlyPageResult ∷ Text → [Text] → [Text] → Maybe Text → Status → Html → TransactionResult
renderTopOnlyPageResult title stylesheets scripts maybe_onload status =
  renderTopOnlyPage title stylesheets scripts maybe_onload
  >>>
  lazyTextAsHTMLResult status

transactionWith ∷ (∀ α. String → ActionM α) → Environment → TransactionProgram TransactionResult → ActionM ()
transactionWith actionWhenAuthFails (environment@Environment{..}) (TransactionProgram program) = do
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  params_ ← Scotty.params
  body_ ← Scotty.body
  current_time ← liftIO Clock.getCurrentTime
  let interpret ∷
        TransactionInstruction α →
        StateT (Account, Bool) (ExceptT Status (RandT StdGen (Writer (Seq String)))) α
      interpret (GetBodyInstruction) = pure body_
      interpret (GetParamsInstruction) = pure params_
      interpret (GetCurrentTimeInstruction) = pure current_time
      interpret (RaiseStatusInstruction s) = throwError s
      interpret (LogInstruction message) =
        void ([i|[#{unwrapUsername username}]: #{message}|] |> singleton |> tell)
      interpret GetAccountInstruction = get <&> fst
      interpret (PutAccountInstruction new_account) = put (new_account, True)
  initial_generator ← liftIO newStdGen
  (redirect_or_content, status, logs, account_changed) ← atomically >>> liftIO $ do
    old_account ← readTVar account_tvar
    let (error_or_result, logs) =
          Operational.interpretWithMonad interpret program
            |> flip runStateT (old_account, False)
            |> runExceptT
            |> flip evalRandT initial_generator
            |> runWriter
    case error_or_result of
      Left status → pure (Right Nothing, status, logs, False)
      Right (result, (new_account, account_changed)) → do
        writeTVar account_tvar $ new_account { _last_seen_ = current_time }
        pure $ case result of
          RedirectsTo status href → (Left href, status, logs, account_changed)
          TransactionResult status content → (Right (Just content), status, logs, account_changed)
  when account_changed $ liftIO $ void $ tryPutMVar accounts_changed_signal ()
  traverse_ logIO logs
  case redirect_or_content of
    Left href →
      setStatusAndRedirect status href
    Right maybe_content → do
      setStatusAndLog status
      case maybe_content of
        Nothing → pure ()
        Just content → setContent content

apiTransaction ∷ Environment → TransactionProgram TransactionResult → ActionM ()
apiTransaction = transactionWith (finishWithStatusMessage 403)

webTransaction ∷ Environment → TransactionProgram TransactionResult → ActionM ()
webTransaction = transactionWith (const $ setStatusAndRedirect temporaryRedirect307 "/login")
