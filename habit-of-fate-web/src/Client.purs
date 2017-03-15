module Client where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Array
import Data.Either
import Data.Generic
import Data.HTTP.Method
import Data.Maybe
import Data.String
import Data.StrMap
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode

import Unicode

type LoginInformation =
  { username ∷ String
  , password ∷ String
  , hostname ∷ String
  , port ∷ Int
  }

type SessionInformation =
  { hostname ∷ String
  , port ∷ Int
  , token ∷ String
  }

newtype LoginAccount = LoginAccount { username ∷ String, password ∷ String }
derive instance genericLoginAccount ∷ Generic LoginAccount

url_template ∷ String
url_template = "http://<HOSTNAME>:<PORT>/<ROUTE>"

createURL ∷ ∀ r. { hostname ∷ String, port ∷ Int | r } → String → URL
createURL server route =
  replace (Pattern "<HOSTNAME>") (Replacement server.hostname)
  >>>
  replace (Pattern "<PORT>") (Replacement $ show server.port)
  >>>
  replace (Pattern "<ROUTE>") (Replacement route)
  $
  url_template

type AffLoginOrCreateResult e = Aff (ajax ∷ AJAX, exception ∷ EXCEPTION | e) SessionInformation

responseStatusCode ∷ ∀ a. AffjaxResponse a → Int
responseStatusCode response =
  case response.status of
    StatusCode code → code

loginOrCreateAccount ∷ ∀ e. String → LoginInformation → AffLoginOrCreateResult e
loginOrCreateAccount route login_info = do
  response ∷ AffjaxResponse String ← post
    (createURL login_info (route ⊕ "?username=" ⊕ login_info.username ⊕ "&password=" ⊕ login_info.password))
    (gEncodeJson (LoginAccount { username: login_info.username, password: login_info.password }))
  let code = responseStatusCode response
  when (code == 409) <<< throwError <<< error $
    "Account \"" ⊕ login_info.username ⊕ "\" already exists!"
  when (code < 200 || code >= 300) <<< throwError <<< error $
    "Unexpected status code: " ⊕ show code
  pure
    { hostname: login_info.hostname
    , port: login_info.port
    , token: response.response
    }

createAccount ∷ ∀ e. LoginInformation → AffLoginOrCreateResult e
createAccount = loginOrCreateAccount "create"

login ∷ ∀ e. LoginInformation → AffLoginOrCreateResult e
login = loginOrCreateAccount "login"

type ClientAff r = Aff (ajax ∷ AJAX, console ∷ CONSOLE, exception ∷ EXCEPTION | r)

type Client α = ∀ r. ReaderT SessionInformation (ClientAff r) α

runClient ∷ ∀ r α. Client α → SessionInformation → ClientAff r α
runClient action session_information = runReaderT action session_information

sendRequest ∷ ∀ α β. (Requestable α, Respondable β) ⇒ Method → String → Array Int → α → Client β
sendRequest method path expected_codes content = do
  request ←
    (\session_info →
      (defaultRequest
        { method = Left method
        , url = createURL session_info path
        , content = Just content
        , headers = [RequestHeader "Authorization" ("Bearer " ⊕ session_info.token)]
        }
      )
    )
    <$>
    ask
  response ← liftAff $ affjax request
  case response.status of
    StatusCode code | not (code ∈ expected_codes) → throwError <<< error $
      "Status code not one of " ⊕ show expected_codes ⊕ ": " ⊕ show code
    _ → pure response.response

sendRequestAndReceiveJson ∷
  ∀ α β.
  (Requestable α, DecodeJson β) ⇒
  Method → String → Array Int → α → Client β
sendRequestAndReceiveJson method path expected_codes content = do
  string ← sendRequest method path expected_codes content
  case jsonParser string >>= decodeJson of
    Left message → do
      liftEff $ log string
      throwError <<< error $ message
    Right value → pure value

newtype Credits = Credits { success ∷ Number, failure ∷ Number }
derive instance eqCredits ∷ Eq Credits
derive instance genericCredits ∷ Generic Credits
derive instance ordCredits ∷ Ord Credits

instance decodeJsonCredits :: DecodeJson Credits where
  decodeJson = gDecodeJson

newtype Habit = Habit { name ∷ String, credits ∷ Credits }
derive instance eqHabit ∷ Eq Habit
derive instance genericHabit ∷ Generic Habit
derive instance ordHabit ∷ Ord Habit

instance decodeJsonHabit :: DecodeJson Habit where
  decodeJson = gDecodeJson

type HabitId = String
type Habits = StrMap Habit

fetchHabits ∷ Client Habits
fetchHabits = sendRequestAndReceiveJson GET "habits" [200] unit
