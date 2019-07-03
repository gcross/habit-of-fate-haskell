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

{-# LANGUAGE AutoDeriveTypeable #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Transaction where

import HabitOfFate.Prelude

import Control.Concurrent (tryPutMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, readTVar, writeTVar)
import Control.DeepSeq (NFData)
import Control.Monad.Catch (Exception(..), MonadThrow(..), SomeException)
import qualified Control.Monad.Operational as Operational
import Control.Monad.Random (MonadRandom(..), MonadSplit(..), RandT, StdGen, runRandT)
import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as LazyBS
import Data.List (head, splitAt, tail)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime (LocalTime)
import Data.Time.Zones (utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByLabel)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (Status(..), internalServerError500, temporaryRedirect307)
import System.Random (Random)
import Text.Blaze.Html5 (Html)
import Web.Scotty (ActionM, Param, Parsable(parseParam))
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Markdown
import HabitOfFate.Logging
import HabitOfFate.Names
import HabitOfFate.Quests
import HabitOfFate.Server.Actions.Queries (authorizeWith)
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common
import HabitOfFate.Substitution

data BadJSONError = BadJSONError String deriving (Eq, Show)
instance Exception BadJSONError where
_BadJSONError ∷ Prism' SomeException BadJSONError
_BadJSONError = prism' toException fromException

data BadParameterError = BadParameterError Lazy.Text String deriving (Eq, Show)
instance Exception BadParameterError where
_BadParameterError ∷ Prism' SomeException BadParameterError
_BadParameterError = prism' toException fromException

data RequestNotFoundError = RequestNotFoundError String deriving (Eq, Show)
instance Exception RequestNotFoundError where
_RequestNotFoundError ∷ Prism' SomeException RequestNotFoundError
_RequestNotFoundError = prism' toException fromException

data StatusError = StatusError Status deriving (Eq, Show)
data StatusExtractor = ∀ e. E (Prism' SomeException e) (e → Status)
instance Exception StatusError where
  fromException exc =
    foldr
      (\(E extractor_prism extract) rest →
        maybe
          rest
          (extract >>> StatusError >>> Just)
          (exc ^? extractor_prism)
      )
      Nothing
      status_extractors
   where
    status_extractors =
      [ E _RequestNotFoundError $ \(RequestNotFoundError message) →
            Status 404 $ encodeUtf8 $ pack $ "NOT FOUND: " ⊕ message
      , E _BadJSONError $ \(BadJSONError message) →
            Status 400 $ encodeUtf8 $ pack $ "BAD JSON: " ⊕ message
      , E _BadParameterError $ \(BadParameterError param_name message) →
            Status 400 $ encodeUtf8 $ pack $ "BAD PARAMETER " ⊕ unpack param_name ⊕ ": " ⊕ unpack message
      ]

data TransactionInstruction α where
  GetAccountInstruction ∷ TransactionInstruction Account
  PutAccountInstruction ∷ Account → TransactionInstruction ()
  GetBodyInstruction ∷ TransactionInstruction LazyBS.ByteString
  GetParamsInstruction ∷ TransactionInstruction [Param]
  LogInstruction ∷ String → TransactionInstruction ()
  GetCurrentTimeInstruction ∷ TransactionInstruction UTCTime
  ThrowInstruction ∷ Exception e ⇒ e → TransactionInstruction α
  GetRandomInstruction ∷ Random α ⇒ TransactionInstruction α
  GetRandomsInstruction ∷ Random α ⇒ TransactionInstruction [α]
  GetRandomRInstruction ∷ Random α ⇒ (α, α) → TransactionInstruction α
  GetRandomRsInstruction ∷ Random α ⇒ (α, α) → TransactionInstruction [α]
  GetSplitInstruction ∷ TransactionInstruction StdGen

newtype Transaction α = Transaction
  { unwrapTransaction ∷ Operational.Program TransactionInstruction α }
  deriving
  ( Applicative
  , Functor
  , Monad
  )

instance MonadThrow Transaction where
  throwM = ThrowInstruction >>> wrapTransactionInstruction

wrapTransactionInstruction ∷ TransactionInstruction α → Transaction α
wrapTransactionInstruction = Operational.singleton >>> Transaction

instance MonadState Account Transaction where
  get = wrapTransactionInstruction GetAccountInstruction
  put = PutAccountInstruction >>> wrapTransactionInstruction

instance MonadRandom Transaction where
  getRandom = wrapTransactionInstruction GetRandomInstruction
  getRandoms = wrapTransactionInstruction GetRandomsInstruction
  getRandomR = GetRandomRInstruction >>> wrapTransactionInstruction
  getRandomRs = GetRandomRsInstruction >>> wrapTransactionInstruction

instance MonadSplit StdGen Transaction where
  getSplit = wrapTransactionInstruction GetSplitInstruction

getBody ∷ Transaction LazyBS.ByteString
getBody = wrapTransactionInstruction GetBodyInstruction

getBodyJSON ∷ FromJSON α ⇒ Transaction α
getBodyJSON = do
  body ← getBody
  case eitherDecode' $ body of
    Left message → do
      log [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body}|]
      throwM $ BadJSONError "Badly formatted JSON"
    Right json → pure json

getParams ∷ Transaction [Param]
getParams = wrapTransactionInstruction GetParamsInstruction

convertUTCToLocalTime ∷ UTCTime → Transaction LocalTime
convertUTCToLocalTime utc_time =
  utcToLocalTimeTZ
    <$> (tzByLabel <$> use (configuration_ . timezone_))
    <*> pure utc_time

getCurrentTime ∷ Transaction UTCTime
getCurrentTime = wrapTransactionInstruction GetCurrentTimeInstruction

getCurrentTimeAsLocalTime ∷ Transaction LocalTime
getCurrentTimeAsLocalTime = getCurrentTime >>= convertUTCToLocalTime

getParam ∷ Parsable α ⇒ Lazy.Text → Transaction α
getParam param_name = do
  params_ ← getParams
  case lookup param_name params_ of
    Nothing → throwM $ BadParameterError param_name [i|Missing parameter %{param_name}|]
    Just value → case parseParam value of
      Left _ → throwM $ BadParameterError param_name [i|Parameter #{param_name} has invalid format #{value}|]
      Right x → return x

getParamMaybe ∷ Parsable α ⇒ Lazy.Text → Transaction (Maybe α)
getParamMaybe param_name =
  getParams
  <&>
  (lookup param_name >=> (parseParam >>> either (const Nothing) return))

getParamDefault ∷ Parsable α ⇒ Lazy.Text → α → Transaction α
getParamDefault param_name d = getParamMaybe param_name <&> fromMaybe d

log ∷ String → Transaction ()
log = LogInstruction >>> wrapTransactionInstruction

getLastSeenAsLocalTime ∷ Transaction LocalTime
getLastSeenAsLocalTime = use last_seen_ >>= convertUTCToLocalTime

lookupResource ∷ String → Lens' Account (ItemsSequence α) → UUID → Transaction α
lookupResource resource_name resource_lens_ resource_id =
  use (resource_lens_ . at resource_id)
  >>=
  maybe (throwM $ RequestNotFoundError [i|No such #{resource_name} #{resource_id}|]) pure

lookupHabit ∷ UUID → Transaction Habit
lookupHabit = lookupResource "habit" habits_

lookupGroup ∷ UUID → Transaction Group
lookupGroup = lookupResource "group" groups_

stripMissingGroupsFromHabit ∷ Habit → Transaction Habit
stripMissingGroupsFromHabit habit = do
  isGroup ← use groups_ <&> flip itemsContainKey
  pure $ (habit & (group_membership_ %~ (setToList >>> filter isGroup >>> setFromList)))

data OutOfCharacters = OutOfCharacters deriving (Eq,Show)
instance Exception OutOfCharacters where
  displayException _ = "Out of names!"

marksArePresent ∷ Transaction Bool
marksArePresent = use marks_ <&> any (null >>> not)

data TransactionResult = RedirectsTo Status Lazy.Text | TransactionResult Status Content

noContentResult ∷ Status → TransactionResult
noContentResult = flip TransactionResult NoContent

lazyTextResult ∷ Status → Lazy.Text → TransactionResult
lazyTextResult s = TextContent >>> TransactionResult s

markdownResult ∷ Status → Markdown → TransactionResult
markdownResult s = unwrapMarkdown >>> Lazy.fromStrict >>> TextContent >>> TransactionResult s

jsonResult ∷ (NFData α, ToJSON α) ⇒ Status → α → TransactionResult
jsonResult s = JSONContent >>> TransactionResult s

redirectsToResult ∷ Status → Lazy.Text → TransactionResult
redirectsToResult status_ url = RedirectsTo status_ url

renderPageResult ∷ Text → [Text] → [Text] → Maybe Text → Status → Html → TransactionResult
renderPageResult title stylesheets scripts maybe_onload status =
  renderPage title stylesheets scripts maybe_onload
  >>>
  HtmlContent
  >>>
  TransactionResult status

renderTopOnlyPageResult ∷ Text → [Text] → [Text] → Maybe Text → Status → Html → TransactionResult
renderTopOnlyPageResult title stylesheets scripts maybe_onload status =
  renderTopOnlyPage title stylesheets scripts maybe_onload
  >>>
  HtmlContent
  >>>
  TransactionResult status

data TransactionResults = TransactionResults
  { redirect_or_content ∷ Either Lazy.Text (Maybe Content)
  , status ∷ Status
  , logs ∷ Seq String
  , account_changed ∷ Bool
  }

instance MonadThrow m ⇒ MonadThrow (RandT r m) where
  throwM = throwM >>> lift

transactionWith ∷ (∀ α. String → ActionM α) → Environment → Transaction TransactionResult → ActionM ()
transactionWith actionWhenAuthFails (environment@Environment{..}) (Transaction program) = do
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  params_ ← Scotty.params
  body_ ← Scotty.body
  current_time ← liftIO Clock.getCurrentTime
  let interpret ∷
        TransactionInstruction α →
        StateT (Account, Bool) (RandT StdGen (ExceptT SomeException (Writer (Seq String)))) α
      interpret (GetBodyInstruction) = pure body_
      interpret (GetParamsInstruction) = pure params_
      interpret (GetCurrentTimeInstruction) = pure current_time
      interpret (LogInstruction message) =
        void ([i|[#{unwrapUsername username}]: #{message}|] |> singleton |> tell)
      interpret GetAccountInstruction = get <&> fst
      interpret (PutAccountInstruction new_account) = put (new_account, True)
      interpret (ThrowInstruction exc) = throwError $ toException exc
      interpret GetRandomInstruction = getRandom
      interpret GetRandomsInstruction = getRandoms
      interpret (GetRandomRInstruction range) = getRandomR range
      interpret (GetRandomRsInstruction range) = getRandomRs range
      interpret GetSplitInstruction = getSplit
  TransactionResults{..} ← atomically >>> liftIO $ do
    old_account ← readTVar account_tvar
    let (error_or_result, logs_without_last) =
          Operational.interpretWithMonad interpret program
            |> flip runStateT (old_account, False)
            |> flip runRandT (_rng_ old_account)
            |> runExceptT
            |> runWriter
    case error_or_result of
      Left exc → do
        let redirect_or_content = Right Nothing
            account_changed = False
            logs = logs_without_last `snoc` [i|EXCEPTION: #{displayException exc}|] `snoc` "(Resetting the quest.)"
            status =
              case fromException exc of
                Just (StatusError status) → status
                _ → internalServerError500
        -- Reset the quest to reduce the risk that the error will just repeat.
        (new_quest_state, new_rng) ← runRandT randomQuestState (old_account & _rng_)
        writeTVar account_tvar $ old_account { _rng_ = new_rng, _quest_state_ = new_quest_state }
        pure $ TransactionResults{..}
      Right ((result, (new_account, account_changed)), new_rng) → do
        let logs = logs_without_last
        when account_changed $
          writeTVar account_tvar $ new_account { _last_seen_ = current_time, _rng_ = new_rng }
        pure $ case result of
          RedirectsTo status href →
            let redirect_or_content = Left href in TransactionResults{..}
          TransactionResult status content →
            let redirect_or_content = Right (Just content) in TransactionResults{..}
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

apiTransaction ∷ Environment → Transaction TransactionResult → ActionM ()
apiTransaction = transactionWith (finishWithStatusMessage 403)

webTransaction ∷ Environment → Transaction TransactionResult → ActionM ()
webTransaction = transactionWith (const $ setStatusAndRedirect temporaryRedirect307 "/login")
