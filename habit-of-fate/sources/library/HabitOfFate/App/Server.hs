{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Log.Logger
import System.FilePath

import HabitOfFate.Server

import Paths_habit_of_fate

logInfo, logNotice, logWarning ∷ MonadIO m ⇒ String → m ()
logInfo = liftIO ∘ infoM "HabitOfFate.Server"
logNotice = liftIO ∘ noticeM "HabitOfFate.Server"
logWarning = liftIO ∘ warningM "HabitOfFate.Server"

data Configuration = Configuration
  { port ∷ Int
  , maybe_data_path ∷ Maybe FilePath
  , maybe_certificate_path ∷ Maybe FilePath
  , maybe_key_path ∷ Maybe FilePath
  , log_level ∷ Priority
  }

habitMain ∷ IO ()
habitMain = do
  let configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> option auto (mconcat
              [ metavar "PORT"
              , help "Port to listen on."
              , long "port"
              , short 'p'
              , value 8081
              ])
        <*> option auto (mconcat
              [ metavar "DIRECTORY"
              , help "Path to game and server data."
              , long "data"
              , action "directory"
              , value Nothing
              ])
        <*> option auto (mconcat
              [ metavar "FILE"
              , help "Path to the certificate file."
              , long "cert"
              , long "certificate"
              , action "file"
              , value Nothing
              ])
        <*> option auto (mconcat
              [ metavar "FILE"
              , help "Path to the key file."
              , long "key"
              , action "file"
              , value Nothing
              ])
        <*> option auto (mconcat
              [ metavar "LEVEL"
              , help "Log level."
              , long "log-level"
              , short 'l'
              , value NOTICE
              ])
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (   fullDesc
       <> header "habit-server - server program for habit-of-fate"
      )
  updateGlobalLogger rootLoggerName (setLevel log_level)
  logInfo $ printf "Listening on port %i" port
  certificate_path ←
    maybe
      (getDataFileName $ "data" </> "testing_certificate.pem")
      pure
      maybe_certificate_path
  logInfo $ printf "Using certificate file located at %s" certificate_path
  key_path ←
    maybe
      (getDataFileName $ "data" </> "testing_key.pem")
      pure
      maybe_key_path
  logInfo $ printf "Using key file located at %s" key_path
  let data_path = fromMaybe "/tmp/habit" maybe_data_path
  makeApp data_path
    >>=
    runTLS
      (tlsSettings certificate_path key_path)
      (setPort port defaultSettings)
