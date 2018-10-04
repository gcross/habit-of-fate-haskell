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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, swapTVar)
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.Text.IO
import Data.Yaml hiding (Parser, (.=))
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Directory
import System.Exit
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging
import HabitOfFate.Server

data Configuration = Configuration
  { data_path ∷ FilePath
  , certificate_path ∷ FilePath
  , key_path ∷ FilePath
  }

exitFailureWithMessage ∷ Text → IO α
exitFailureWithMessage message = do
  putStrLn message
  exitFailure

writeDataOnChange ∷ String → TVar (Map Username (TVar Account)) → TVar Bool → IO α
writeDataOnChange data_path accounts_tvar changed_flag = forever $
  (atomically $
    swapTVar changed_flag False
    >>=
    bool retry (readTVar accounts_tvar >>= traverse readTVar)
  )
  >>=
  encodeFile data_path
  >>
  logIO "Wrote data."

main ∷ IO ()
main = do
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
          BS.readFile data_path >>= (decodeEither >>> either error pure)
      )
    >>=
    (\accounts → atomically $ traverse newTVar accounts >>= newTVar)
  accounts_changed_flag ← newTVarIO False
  forkIO $ writeDataOnChange data_path accounts_tvar accounts_changed_flag
  app ← makeApp accounts_tvar accounts_changed_flag
  done_mvar ← newEmptyMVar

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
