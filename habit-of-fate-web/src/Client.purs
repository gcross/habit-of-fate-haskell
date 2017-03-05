module Client
  ( LoginInformation
  , createAccount
  , login
  ) where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Data.Argonaut.Encode
import Data.Generic (class Generic)
import Data.String
import Network.HTTP.Affjax
import Network.HTTP.StatusCode

type LoginInformation =
  { username :: String
  , password :: String
  , hostname :: String
  , port :: Int
  }

type SessionInformation =
  { hostname :: String
  , port :: Int
  , token :: String
  }

newtype LoginAccount = LoginAccount { username :: String, password :: String }
derive instance genericLoginAccount :: Generic LoginAccount

url_template :: String
url_template = "http://<HOSTNAME>:<PORT>/<ROUTE>"

createURL :: forall r. { hostname :: String, port :: Int | r } -> String -> URL
createURL server route =
  replace (Pattern "<HOSTNAME>") (Replacement server.hostname)
  >>>
  replace (Pattern "<PORT>") (Replacement $ show server.port)
  >>>
  replace (Pattern "<ROUTE>") (Replacement route)
  $
  url_template

type AffLoginOrCreateResult e = Aff (ajax :: AJAX, exception :: EXCEPTION | e) SessionInformation

responseStatusCode :: forall a. AffjaxResponse a -> Int
responseStatusCode response =
  case response.status of
    StatusCode code -> code

loginOrCreateAccount :: forall e. String -> LoginInformation -> AffLoginOrCreateResult e
loginOrCreateAccount route login_info = do
  response :: AffjaxResponse String <- post
    (createURL login_info (route <> "?username=" <> login_info.username <> "&password=" <> login_info.password))
    (gEncodeJson (LoginAccount { username: login_info.username, password: login_info.password }))
  let code = responseStatusCode response
  when (code == 409) $
    throwError $ error $ "Account \"" <> login_info.username <> "\" already exists!"
  when (code < 200 || code >= 300) $
    throwError $ error $ "Unexpected status code: " <> show code
  pure
    { hostname: login_info.hostname
    , port: login_info.port
    , token: response.response
    }

createAccount :: forall e. LoginInformation -> AffLoginOrCreateResult e
createAccount = loginOrCreateAccount "create"

login :: forall e. LoginInformation -> AffLoginOrCreateResult e
login = loginOrCreateAccount "login"
