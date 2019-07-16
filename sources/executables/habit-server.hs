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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (newTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (finally, throwIO)
import qualified Data.ByteString as BS
import Data.Yaml hiding (Parser, (.=))
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Directory
import System.IO (hPutStrLn, stderr)
import qualified Web.Scotty as Scotty

import HabitOfFate.Logging (loggerThread, logIO)
import HabitOfFate.Server

data Configuration = Configuration
  { data_path ∷ FilePath
  , certificate_path ∷ FilePath
  , key_path ∷ FilePath
  }

data WritingInstruction = DoWrite | StopWriting

main ∷ IO ()
main = do
  stop_logging_signal ← newTVarIO False
  stopped_logging_signal ← newEmptyMVar
  void $ forkIO $ loggerThread stop_logging_signal stopped_logging_signal

  let configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> (strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the game data."
              , long "data"
              , action "file"
              ]
            )
        <*> (strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the certificate file."
              , long "cert"
              , long "certificate"
              , action "file"
              ]
            )
        <*> (strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the key file."
              , long "key"
              , action "file"
              ]
            )
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (fullDesc <> header "habit-server - server program for habit-of-fate"
      )
  logIO $ "Certificate file is located at " ⊕ certificate_path
  logIO $ "Key file is located at " ⊕ key_path

  accounts_tvar ←
    doesFileExist data_path
    >>=
    bool
      (do logIO $ "Creating new data file at " ⊕ data_path
          pure mempty
      )
      (do logIO $ "Reading existing data file at " ⊕ data_path
          BS.readFile data_path >>= decodeThrow
      )
    >>=
    (\accounts → atomically $ traverse newTVar accounts >>= newTVar)

  let writeData ∷ IO ()
      writeData =
        (atomically $ readTVar accounts_tvar)
        >>=
        (traverse readTVar >>> atomically)
        >>=
        encodeFile data_path
        >>
        logIO "Wrote data."

  accounts_changed_signal ← newTVarIO False
  stop_writing_signal ← newTVarIO False
  stopped_writing_signal ← newEmptyMVar

  let changedThread ∷ IO ()
      changedThread = do
        next ←
          atomically $ do
            should_stop ← readTVar stop_writing_signal
            should_write ← readTVar accounts_changed_signal
            writeTVar accounts_changed_signal False
            case (should_stop, should_write) of
              (True, _) → pure StopWriting
              (_, True) → pure DoWrite
              _ → retry
        writeData
        case next of
          DoWrite → changedThread
          StopWriting → logIO "Stopped writer thread." >> putMVar stopped_writing_signal ()

  void $ forkIO $ changedThread
  void $ forkIO $ forever $
    threadDelay (60 * 1000 * 1000) >> (atomically $ writeTVar accounts_changed_signal True)

  app ← makeApp accounts_tvar accounts_changed_signal
  done_mvar ← newEmptyMVar

  finally
    (do
      void $
        forkFinally
          (Scotty.scotty 80 $
            Scotty.matchAny (Scotty.function $ const (Just [])) $ do
              Just host ← Scotty.header "Host"
              Scotty.redirect $ "https://" ⊕ host
          )
          (either throwIO (const $ putMVar done_mvar ()))

      void $
        forkFinally
          (runTLS
            ((tlsSettings certificate_path key_path) {onInsecure = AllowInsecure} )
            (setPort 443 defaultSettings)
            app
          )
          (either throwIO (const $ putMVar done_mvar ()))

      takeMVar done_mvar
      takeMVar done_mvar
    )
    (do
      logIO "Getting the writer and the logger to stop cleanly..."
      atomically $ do
        writeTVar stop_writing_signal True
        writeTVar stop_logging_signal True
      takeMVar stopped_writing_signal
      takeMVar stopped_logging_signal
      hPutStrLn stderr "All done!"
    )
