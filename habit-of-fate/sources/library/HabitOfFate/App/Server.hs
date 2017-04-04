{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import qualified Data.ByteString as BS
import Data.Text.IO
import Data.UUID
import Data.Yaml hiding (Parser, (.=))
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Random
import Web.JWT hiding (header)

import HabitOfFate.Logging
import HabitOfFate.Server

import Paths_habit_of_fate

data Configuration = Configuration
  { port ∷ Int
  , maybe_data_path ∷ Maybe FilePath
  , maybe_password_secret_path ∷ Maybe FilePath
  , maybe_certificate_path ∷ Maybe FilePath
  , maybe_key_path ∷ Maybe FilePath
  , test_mode ∷ Bool
  }

exitFailureWithMessage ∷ Text → IO α
exitFailureWithMessage message = do
  putStrLn message
  exitFailure

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
              [ metavar "FILE"
              , help "Path to the game data."
              , long "data"
              , action "file"
              ]
            )
        <*> (optional ∘ strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the password secret file."
              , long "data"
              , action "file"
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
  (certificate_path, key_path) ←
    if test_mode
     then
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → pure (certificate_path, key_path)
         (Nothing, Nothing) → pure (default_certificate_path, default_key_path)
         _ →
           exitFailureWithMessage
             "When in test mode you must specify either both a certificate file and a key file or neither."
     else
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → pure (certificate_path, key_path)
         (Just _, Nothing) →
           exitFailureWithMessage
             "You need to specify a key file via. --key"
         (Nothing, Just _) →
           exitFailureWithMessage
             "You need to specify a certificate file via. --cert"
         _ →
           exitFailureWithMessage
             "You need to specify both a certificate file via. --cert and a key file via. --key."
  when test_mode $ logIO "Running in test mode.  DO NOT DO THIS IN PRODUCTION!!!"
  logIO $ printf "Listening on port %i" port
  logIO $ printf "Using certificate file located at %s" certificate_path
  logIO $ printf "Using key file located at %s" key_path

  (initial_accounts, saveAccounts) ←
    case maybe_data_path of
      Nothing → do
        if test_mode
          then do
            logIO "No data file specified; all game data will lost on exit."
            pure (mempty, const $ pure ())
          else exitFailureWithMessage "You need to specify the path to the data file."
      Just data_path → do
        logIO $ printf "Using data file located at %s" data_path
        initial_accounts ←
          doesFileExist data_path
          >>=
          bool
            (do logIO "Creating new data file"
                pure mempty
            )
            (do logIO "Reading existing data file"
                BS.readFile data_path >>= either error pure ∘ decodeEither
            )
        pure (initial_accounts, encodeFile data_path)

  password_secret ←
    case maybe_password_secret_path of
      Nothing → do
        if test_mode
          then do
            logIO "No password secret file specified; generating a random secret."
            secret ∘ toText <$> randomIO
          else exitFailureWithMessage "You need to specify the path to the data file."
      Just password_secret_path → do
        doesFileExist password_secret_path
        >>=
        bool
          (do logIO "Creating new secret"
              secret_uuid ← toText <$> randomIO
              writeFile password_secret_path secret_uuid
              return $ secret secret_uuid
          )
          (do logIO "Reading existing secret"
              secret <$> readFile password_secret_path
          )

  app ← makeApp password_secret initial_accounts saveAccounts
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
