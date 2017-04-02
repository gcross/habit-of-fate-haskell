{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Control.Lens
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Exit
import System.FilePath

import HabitOfFate.Logging
import HabitOfFate.Server

import Paths_habit_of_fate

data CertificateConfiguration =
    InvalidCertificateConfiguration String
  | DefaultCertificateConfiguration
  | CertificateConfiguration
    { certificate_path ∷ FilePath
    , key_path ∷ FilePath
    }

certificate_configuration_parser ∷ Parser CertificateConfiguration
certificate_configuration_parser =
  (
    (,) <$> (optional ∘ strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the certificate file. If present, must also specify the key file."
              , long "cert"
              , long "certificate"
              , action "file"
              ]
            )
        <*> (optional ∘ strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the key file. If present, must also specify the certificate file."
              , long "key"
              , action "file"
              ]
            )
  )
  <&>
  (\case
    (Nothing, Nothing) →
      DefaultCertificateConfiguration
    (Just certificate_path, Just key_path) →
      CertificateConfiguration certificate_path key_path
    (Just _, Nothing) →
      InvalidCertificateConfiguration
        "Must also specify the key file when specifying the certificate file."
    (Nothing, Just _) →
      InvalidCertificateConfiguration
        "Must also specify the certificate file when specifying the key file."
  )

data Configuration = Configuration
  { port ∷ Int
  , data_path ∷ FilePath
  , allow_insecure ∷ Bool
  , certificate_configuration ∷ CertificateConfiguration
  }

configuration_parser ∷ FilePath → Parser Configuration
configuration_parser default_data_path = Configuration
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
  <*> switch (mconcat
        [ help "Allow insecure connections."
        , long "allow-insecure"
        ])
  <*> certificate_configuration_parser

habitMain ∷ IO ()
habitMain = do
  Configuration{..} ←
    execParser $ info
      (configuration_parser "/tmp/habit" <**> helper)
      (   fullDesc       <> header "habit-server - server program for habit-of-fate"
      )
  (certificate_path, key_path) ←
    case certificate_configuration of
      InvalidCertificateConfiguration message → putStrLn message >> exitFailure
      DefaultCertificateConfiguration →
        (,) <$> getDataFileName ("data" </> "testing_certificate.pem")
            <*> getDataFileName ("data" </> "testing_key.pem")
      CertificateConfiguration certificate_path key_path → pure (certificate_path, key_path)
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
