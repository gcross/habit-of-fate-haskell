{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.FilePath

import HabitOfFate.Logging
import HabitOfFate.Server

import Paths_habit_of_fate

data Configuration = Configuration
  { port ∷ Int
  , data_path ∷ FilePath
  , certificate_path ∷ FilePath
  , key_path ∷ FilePath
  , allow_insecure ∷ Bool
  }

habitMain ∷ IO ()
habitMain = do
  default_certificate_path ← getDataFileName $ "data" </> "testing_certificate.pem"
  default_key_path ← getDataFileName $ "data" </> "testing_key.pem"
  let default_data_path = "/tmp/habit"
      configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> option auto (mconcat
              [ metavar "PORT"
              , help "Port to listen on."
              , long "port"
              , short 'p'
              , value 8081
              ])
        <*> strOption (mconcat
              [ metavar "DIRECTORY"
              , help "Path to game and server data."
              , long "data"
              , action "directory"
              , value default_data_path
              ])
        <*> strOption (mconcat
              [ metavar "FILE"
              , help "Path to the certificate file."
              , long "cert"
              , long "certificate"
              , action "file"
              , value default_certificate_path
              ])
        <*> strOption (mconcat
              [ metavar "FILE"
              , help "Path to the key file."
              , long "key"
              , action "file"
              , value default_key_path
              ])
        <*> switch (mconcat
              [ help "Allow insecure connections."
              , long "allow-insecure"
              ])
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (   fullDesc       <> header "habit-server - server program for habit-of-fate"
      )
  logIO $ printf "Listening on port %i" port
  logIO $ printf "Using certificate file located at %s" certificate_path
  logIO $ printf "Using key file located at %s" key_path
  app ← makeApp data_path
  let tls_settings =
        (tlsSettings certificate_path key_path)
        { onInsecure =
            if allow_insecure
              then AllowInsecure
              else DenyInsecure ∘ encodeUtf8 ∘ pack $
                     "Insecure connections are not supported."
        }
  runTLS
    tls_settings
    (setPort port defaultSettings)
    app
