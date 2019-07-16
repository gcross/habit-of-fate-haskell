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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Logging where

import HabitOfFate.Prelude

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, readTVar)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

message_channel ∷ TChan String
{-# NOINLINE message_channel #-}
message_channel = unsafePerformIO newTChanIO

loggerThread ∷ TVar Bool → MVar () → IO ()
loggerThread stop_logging_signal stopped_logging_signal = go
 where
  go = do
    stop_logging ← logEnquedMessages
    if stop_logging
      then logEnquedMessages >> putMVar stopped_logging_signal ()
      else threadDelay (100 * 1000) >> go

  logEnquedMessages = do
    (stop_logging, messages) ← atomically $ do
      stop_logging ← readTVar stop_logging_signal
      first ← readTChan message_channel
      rest ←
        let receive messages =
              tryReadTChan message_channel
              >>=
              maybe (messages |> toList |> pure) ((messages `snoc`) >>> receive)
        in receive (mempty ∷ Seq String)
      pure (stop_logging, first:rest)
    header ← formatTime defaultTimeLocale "[%F %X] " <$> getCurrentTime
    hPutStrLn stderr $ intercalate "\n" [ header ⊕ message | message ← messages ]
    pure stop_logging

logIO ∷ MonadIO m ⇒ String → m ()
logIO message = message |> writeTChan message_channel |> atomically |> liftIO
