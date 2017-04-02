{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Exit
import System.FilePath

import HabitOfFate.Logging
import HabitOfFate.Server

import Paths_habit_of_fate

data Configuration = Configuration
  { port ∷ Int
  , maybe_data_path ∷ Maybe FilePath
  , maybe_certificate_path ∷ Maybe FilePath
  , maybe_key_path ∷ Maybe FilePath
  , test_mode ∷ Bool
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
        <*> (optional ∘ strOption $ mconcat
              [ metavar "DIRECTORY"
              , help "Path to game and server data."
              , long "data"
              , action "directory"
              ]
            )
        <*> (optional ∘ strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the certificate file."
              , long "cert"
              , long "certificate"
              , action "file"
              ]
            )
        <*> (optional ∘ strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the key file."
              , long "key"
              , action "file"
              ]
            )
        <*> switch (mconcat
              [ help "Enable test mode."
              , long "test-mode"
              , internal
              ])
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (   fullDesc       <> header "habit-server - server program for habit-of-fate"
      )
  data_path ←
    case maybe_data_path of
      Just data_path → pure data_path
      Nothing
        | test_mode → pure "/tmp/habit"
        | otherwise → do
            putStrLn "Must specify the data path via. --data"
            exitFailure
  (certificate_path, key_path) ←
    if test_mode
     then
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → pure (certificate_path, key_path)
         (Nothing, Nothing) → pure (default_certificate_path, default_key_path)
         _ → do
           putStrLn "When in test mode you must specify either both a certificate file and a key file or neither."
           exitFailure
     else
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → pure (certificate_path, key_path)
         (Just _, Nothing) → do
           putStrLn "You need to specify a key file via. --key"
           exitFailure
         (Nothing, Just _) → do
           putStrLn "You need to specify a certificate file via. --cert"
           exitFailure
         _ → do
           putStrLn "You need to specify both a certificate file via. --cert and a key file via. --key."
           exitFailure
  when test_mode $ logIO "Running in test mode.  DO NOT DO THIS IN PRODUCTION!!!"
  logIO $ printf "Listening on port %i" port
  logIO $ printf "Using certificate file located at %s" certificate_path
  logIO $ printf "Using key file located at %s" key_path
  app ← makeApp data_path
  let tls_settings =
        (tlsSettings certificate_path key_path)
        { onInsecure =
            if test_mode
              then AllowInsecure
              else DenyInsecure ∘ encodeUtf8 ∘ pack $
                     "Insecure connections are not supported."
        }
  runTLS
    tls_settings
    (setPort port defaultSettings)
    app
