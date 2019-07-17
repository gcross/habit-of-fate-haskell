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

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.DeepSeq (NFData)
import Control.Monad.Catch (Exception(..), MonadThrow(..), SomeException)
import Control.Monad.Random (MonadRandom(..), RandT, StdGen, runRandT)
import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime (LocalTime)
import Data.Time.Zones (utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByLabel)
import Data.UUID (UUID)
import Network.HTTP.Types.Status (Status(..), internalServerError500, temporaryRedirect307)
import System.Random (Random(randomR, random, randomRs, randoms))
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
import HabitOfFate.Quests
import HabitOfFate.Server.Actions.Queries (authorizeWith)
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common

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

data TransactionState =
  TransactionState
    { transaction_account ∷ !Account
    , transaction_account_changed ∷ !Bool
    , transaction_rng ∷ !StdGen
    , transaction_log ∷ !(Seq String)
    }

data TransactionEnvironment = TransactionEnvironment
  { transaction_username ∷ !Username
  , transaction_params ∷ ![Param]
  , transaction_body ∷ !LazyBS.ByteString
  , transaction_current_time ∷ !UTCTime
  }

newtype Transaction α = Transaction
  { runTransaction ∷
      TransactionEnvironment →
      TransactionState →
      (Either (SomeException, Seq String) (α, TransactionState))
  }

instance Functor Transaction where
  fmap f x = Transaction $ \e s → fmap (first f) (runTransaction x e s)

instance Applicative Transaction where
  pure v = Transaction $ \_ s → pure (v, s)

  xf <*> xv = Transaction $ \e s0 → do
    (f, s1) ← runTransaction xf e s0
    (v, s2) ← runTransaction xv e s1
    pure (f v, s2)

  liftA2 f x0 x1 = Transaction $ \e s0 → do
    (v0, s1) ← runTransaction x0 e s0
    (v1, s2) ← runTransaction x1 e s1
    pure (f v0 v1, s2)

  xv0 *> xv1 = Transaction $ \e s0 → do
    (_, s1) ← runTransaction xv0 e s0
    runTransaction xv1 e s1

  xv0 <* xv1 = Transaction $ \e s0 → do
    (v, s1) ← runTransaction xv0 e s0
    (_, s2) ← runTransaction xv1 e s1
    pure (v, s2)

instance Monad Transaction where
  x >>= f = Transaction $ \e s0 → do
    (v, s1) ← runTransaction x e s0
    runTransaction (f v) e s1

  x >> y = Transaction $ \e s0 → do
    (_, s1) ← runTransaction x e s0
    runTransaction y e s1

instance MonadState Account Transaction where
  get = Transaction $ \_ s → pure (s & transaction_account, s)

  put new_account = Transaction $ \_ s →
    pure ((), s { transaction_account = new_account, transaction_account_changed = True })

  state f = Transaction $ \_ s →
    let (v, new_account) = f (s & transaction_account)
    in pure (v, s { transaction_account = new_account, transaction_account_changed = True })

instance MonadRandom Transaction where
  getRandomR range = Transaction $ \_ s →
    let g = s & transaction_rng
        (v, g') = randomR range g
    in pure (v, s { transaction_rng = g' })

  getRandom = Transaction $ \_ s →
    let g = s & transaction_rng
        (v, g') = random g
    in pure (v, s { transaction_rng = g' })

  getRandomRs range = Transaction $ \_ s →
    let g = s & transaction_rng
    in pure (randomRs range g, s)

  getRandoms = Transaction $ \_ s →
    let g = s & transaction_rng
    in pure (randoms g, s)

instance MonadThrow Transaction where
  throwM exc = Transaction $ \_ s → Left (toException exc, s & transaction_log)

getBody ∷ Transaction LazyBS.ByteString
getBody = Transaction $ \e s → pure (e & transaction_body, s)

getBodyJSON ∷ FromJSON α ⇒ Transaction α
getBodyJSON = do
  body ← getBody
  case eitherDecode' $ body of
    Left message → do
      log [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body}|]
      throwM $ BadJSONError "Badly formatted JSON"
    Right json → pure json

getParams ∷ Transaction [Param]
getParams = Transaction $ \e s → pure (e & transaction_params, s)

convertUTCToLocalTime ∷ UTCTime → Transaction LocalTime
convertUTCToLocalTime utc_time =
  utcToLocalTimeTZ
    <$> (tzByLabel <$> use (configuration_ . timezone_))
    <*> pure utc_time

getCurrentTime ∷ Transaction UTCTime
getCurrentTime = Transaction $ \e s → pure (e & transaction_current_time, s)

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
log message = Transaction $ \e s →
  let formatted_message = [i|[#{unwrapUsername $ e & transaction_username}]: #{message}|]
  in pure ((), s { transaction_log = (s & transaction_log) `snoc` formatted_message })

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
marksArePresent = use marks_ <&> (not <<< onull)

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

renderPageResult ∷ Text → (Device → [Text]) → [Text] → Maybe Text → Status → (Device → Html) → TransactionResult
renderPageResult title stylesheetsFor scripts maybe_onload status =
  renderPage title stylesheetsFor scripts maybe_onload
  >>>
  HtmlContent
  >>>
  TransactionResult status

renderTopOnlyPageResult ∷ Text → (Device → [Text]) → [Text] → Maybe Text → Status → (Device → Html) → TransactionResult
renderTopOnlyPageResult title stylesheetsFor scripts maybe_onload status contentFor =
  TransactionResult
    status
    (HtmlContent $ renderTopOnlyPage title stylesheetsFor scripts maybe_onload contentFor)

data TransactionResults = TransactionResults
  { redirect_or_content ∷ Either Lazy.Text (Maybe Content)
  , status ∷ Status
  , logs ∷ Seq String
  , account_changed ∷ Bool
  }

instance MonadThrow m ⇒ MonadThrow (RandT r m) where
  throwM = throwM >>> lift

transactionWith ∷ (∀ α. String → ActionM α) → Environment → Transaction TransactionResult → ActionM ()
transactionWith actionWhenAuthFails (environment@Environment{..}) transaction = do
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  current_time ← liftIO Clock.getCurrentTime
  transaction_environment ←
    TransactionEnvironment
      <$> pure username
      <*> Scotty.params
      <*> Scotty.body
      <*> pure current_time
  TransactionResults{..} ← atomically >>> liftIO $ do
    old_account ← readTVar account_tvar
    let initial_transaction_state = TransactionState
          { transaction_account = old_account
          , transaction_account_changed = False
          , transaction_rng = old_account & _rng_
          , transaction_log = mempty
          }
    case runTransaction transaction transaction_environment initial_transaction_state of
      Left (exc, logs) → do
        -- Reset the quest to reduce the risk that the error will just repeat.
        (new_quest_state, new_rng) ← runRandT randomQuestState (old_account & _rng_)
        writeTVar account_tvar $ old_account { _rng_ = new_rng, _quest_state_ = new_quest_state }
        pure $ TransactionResults
          { redirect_or_content = Right Nothing
          , status =
              case fromException exc of
                Just (StatusError status) → status
                _ → internalServerError500
          , logs = logs `snoc` [i|EXCEPTION: #{displayException exc}|] `snoc` "(Resetting the quest.)"
          , account_changed = True -- because the RNG state was updated
          }
      Right (result, final_transaction_state) → do
        let account_changed = final_transaction_state & transaction_account_changed
            logs = final_transaction_state & transaction_log
        when account_changed $
          writeTVar account_tvar $ (final_transaction_state & transaction_account)
            { _last_seen_ = current_time
            , _rng_ = final_transaction_state & transaction_rng
            }
        pure $ case result of
          RedirectsTo status href →
            let redirect_or_content = Left href in TransactionResults{..}
          TransactionResult status content →
            let redirect_or_content = Right (Just content) in TransactionResults{..}

  -- We check whether the signal has been raised before raising it ourselves
  -- because this is a popular variable and so we don't want a bunch of threads
  -- tripping over themselves in order to set it to True when it is already True
  -- (assuming, as I suspect, that the STM runtime is not smart enough to ignore
  -- idempotent writes).
  when account_changed $ liftIO $ atomically $
    readTVar accounts_changed_signal
    >>=
    flip unless (writeTVar accounts_changed_signal True)

  logIO (logs |> toList |> intercalate "\n")
  case redirect_or_content of
    Left href →
      setStatusAndRedirect status href
    Right maybe_content → do
      setStatusAndLog status
      case maybe_content of
        Nothing → pure ()
        Just content → getDevice >>= flip setContent content

apiTransaction ∷ Environment → Transaction TransactionResult → ActionM ()
apiTransaction = transactionWith (finishWithStatusMessage 403)

webTransaction ∷ Environment → Transaction TransactionResult → ActionM ()
webTransaction = transactionWith (const $ setStatusAndRedirect temporaryRedirect307 "/login")
