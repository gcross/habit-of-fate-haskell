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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
import HabitOfFate.TH
import HabitOfFate.Server

testing_certificate = [textChar8|
-----BEGIN CERTIFICATE-----
MIICljCCAX4CCQCWg1jH1c0MhjANBgkqhkiG9w0BAQsFADANMQswCQYDVQQGEwJV
UzAeFw0xNzAyMTIwMjA3MDVaFw0xNzAzMTQwMjA3MDVaMA0xCzAJBgNVBAYTAlVT
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2IbA2w9ZZFD5oKqkvJAi
epkr2mMdzripBKhI0fUmAhaNd8hZ6c6eepyA1HUN8ITva58l22QJjdKxynoE7QHh
u2fPF6pdBF1RwfCDvUtlAcKqhiJrdxAA8EUa3aAIqNTfAR0kU6GLmj9AFn8Imt/z
7GGEi5m9/DvuQARDJnFmFeZhadyYvQQwtVg1EqCU2UsME9Q9GedurW8Ge0decfiO
HWbDH8TZEfHXXdtCxy1YSqoQ42+hxm4BGSN+L2ZLYfDLwcPTn6d5zHvK0M4L5C6l
hYYMQ7DNHwuW4jL1nU+P/vE6559IkRP9GpH8loqTbr7kre2p4cj2/UmhI8ohXSNv
fwIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQCAFzdHjv7mBE59vq5evoZvesC+N9aC
JDsuypBoOjBBR1LsKQYvcVm8dMzaUAnE01BJJJ+1w8uUtq2H1stLKEya5Mrv1sHH
O5Ap0EOaszZLapOvickRM0rTfYFHG1uYVChzejVKNkkhbuieqrtMbwphRS8M/S1S
EtFYYRJdTHhEByRVlaNY3Os9LXQarUXl+XTrS5jd9OipmjUR2sSPfxI7qTZJsMEp
s8mNSqXHyukrrhlAfX3p32ymjlA5eNgb888J9WXN072tmUKQi/B8OhUoxJdZPLMW
SPUUbH+1Dx/HvJ5Xc26WxYcgXP221HgzjpJcwPC7bpF6ODcsoejZxDi+
-----END CERTIFICATE-----
|]

testing_key = [textChar8|
-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA2IbA2w9ZZFD5oKqkvJAiepkr2mMdzripBKhI0fUmAhaNd8hZ
6c6eepyA1HUN8ITva58l22QJjdKxynoE7QHhu2fPF6pdBF1RwfCDvUtlAcKqhiJr
dxAA8EUa3aAIqNTfAR0kU6GLmj9AFn8Imt/z7GGEi5m9/DvuQARDJnFmFeZhadyY
vQQwtVg1EqCU2UsME9Q9GedurW8Ge0decfiOHWbDH8TZEfHXXdtCxy1YSqoQ42+h
xm4BGSN+L2ZLYfDLwcPTn6d5zHvK0M4L5C6lhYYMQ7DNHwuW4jL1nU+P/vE6559I
kRP9GpH8loqTbr7kre2p4cj2/UmhI8ohXSNvfwIDAQABAoIBAQC07wXBB/Z+6Vtv
cqjuGNN29v+6IhEKaSxzg9w19lCodggJDBZ2Vf4AHz9YSeg4EB2xJPARgGqrZGDE
/WmYU3Y5j+lxsR7BQunK0hyD1bi12+F67NA4Uds268gjYlNaIWeoGp38dIWfgzMe
mVCrenDuGh8UOIiVec8BF461VRUlPjCz+kuRsp29CktVdo2poLZHBToacfm4vHlS
iNFJJRB5Oeu39oUIOMjgQoH/8Lo0qAHbnaUX8uvQazrflNMUVAYM4gpSwMoZS/2b
24QkpOEwp5qk2ADG0x+J4kO9vDyLjhl95b9NzmeeK3hjoW+1n9Wy2dHuXPpOwhwq
FzFEKpuBAoGBAO0Jg+pBhNJAzBos229B9Whr85Q6Gk0HlVaLVmXsN2U6ubetBo7A
lQ8pBLbiG+FF2enSd2WkOnfqWu7sPKxhIAG2nvddTW7g69aeQbMFRNgbBPjtLQvw
r8eFRP+yKCrXbHLEjZ3tEhOhQ+XUnFzO6Qf4oIetSI+TG0OdiVEaiXevAoGBAOnZ
LiURWujhmKZji6c+v8DTmMcXpzEK7HgzscCWGsrXJYOiAKCsSMnPQKF2GV1DvdqS
pT996r5579Rgk5wn1+b3OtDn0XJeZfGUlCnAAAHuxehd7PG/EO4171YyW4jLZd5Q
NeYCnQvNK/PLs4WqvRvNhJuwFaIET2kUQH+nf6kxAoGABh+RzdJlePz9iMuR25zr
lwf64eBiX40LmZG96KIiiDXtKEaK/dMRxrN7kLHkoHwqdfTe6rxEWmaudK+bnaRg
rEqobqF1Z6Dd1sx9y/8d2StRhJCz8jQEhnX141kZ2ol4HrrfIa5HIkSvOVe1tlwN
/wu4MekTD8pwEN4X9wVlebcCgYBdQhd9AObYaN+Pu7M+B3BBXiFXhL9Qd6LGbRc+
Tp5wtkxnqnvFl++PS+/idE65OwLD4Ce2omizfb1/XOSBKgKYQZBbL5f/nFXbef18
iO1319llSc/suN3voeLI5VxLuZHujt5v1Cr9Qd19ZIc7j1PLerKfxCFcc4uXxFPU
lN/8gQKBgQCdgBqjjcNgMToy4Jz0P/afXtVxjiIKZkPG30W7Sdv15Ms7Feraw+hy
+SqqYT4Ovg3EnX+0IEqPJbB+vLgiDM6ndlcIBMQuADmIB0nkP2nfD+Xjl0izGY06
HCkX84dkDHTBizEpVcdZp1gVf/5+hUmFgG/w9EWGrRe2UxhwHJvtkA==
-----END RSA PRIVATE KEY-----
|]

data Configuration = Configuration
  { port ∷ Int
  , maybe_data_path ∷ Maybe FilePath
  , maybe_certificate_path ∷ Maybe FilePath
  , maybe_key_path ∷ Maybe FilePath
  , test_mode ∷ Bool
  }

exitFailureWithMessage ∷ Text → IO α
exitFailureWithMessage message = do
  putStrLn message
  exitFailure

main ∷ IO ()
main = do
  let configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> option auto (mconcat
              [ metavar "PORT"
              , help "Port to listen on."
              , long "port"
              , short 'p'
              , value 8081
              ])
        <*> (strOption >>> optional $ mconcat
              [ metavar "FILE"
              , help "Path to the game data."
              , long "data"
              , action "file"
              ]
            )
        <*> (strOption >>> optional $ mconcat
              [ metavar "FILE"
              , help "Path to the certificate file."
              , long "cert"
              , long "certificate"
              , action "file"
              ]
            )
        <*> (strOption >>> optional $ mconcat
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
  (certificate, key) ←
    if test_mode
     then
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → do
           logIO $ "Using certificate file located at " ⊕ certificate_path
           logIO $ "Using key file located at " ⊕ key_path
           (,) <$> BS8.readFile certificate_path <*> BS8.readFile key_path
         (Nothing, Nothing) → do
           logIO $ "Using testing certificate and key"
           pure (testing_certificate, testing_key)
         _ →
           exitFailureWithMessage
             "When in test mode you must specify either both a certificate file and a key file or neither."
     else
       case (maybe_certificate_path, maybe_key_path) of
         (Just certificate_path, Just key_path) → do
           logIO $ "Using certificate file located at " ⊕ certificate_path
           logIO $ "Using key file located at " ⊕ key_path
           (,) <$> BS8.readFile certificate_path <*> BS8.readFile key_path
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
  logIO $ "Listening on port " ⊕ show port

  (initial_accounts, saveAccounts) ←
    case maybe_data_path of
      Nothing
        | test_mode → do
            logIO "No data file specified; all game data will lost on exit."
            pure (mempty, const $ pure ())
        | otherwise →
            exitFailureWithMessage "You need to specify the path to the data file."
      Just data_path → do
        logIO $ "Using data file located at " ⊕ data_path
        initial_accounts ←
          doesFileExist data_path
          >>=
          bool
            (do logIO "Creating new data file"
                pure mempty
            )
            (do logIO "Reading existing data file"
                BS.readFile data_path >>= (decodeEither >>> either error pure)
            )
        pure (initial_accounts, encodeFile data_path)

  app ← makeApp test_mode initial_accounts saveAccounts
  let tls_settings =
        (tlsSettingsMemory certificate key)
        { onInsecure =
            if test_mode
              then AllowInsecure
              else DenyInsecure <<< encodeUtf8 <<< pack $
                     "Insecure connections are not supported."
        }
  runTLS
    tls_settings
    (setPort port defaultSettings)
    app
