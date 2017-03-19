module Client where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Array
import Data.Dynamic
import Data.Either
import Data.Generic
import Data.HTTP.Method
import Data.Maybe
import Data.Monoid
import Data.String
import Data.StrMap
import Data.Typeable
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode
import Unicode
import Control.Monad.Eff.Console (CONSOLE, log)

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

responseStatusCode ∷ ∀ a. AffjaxResponse a → Int
responseStatusCode response =
  case response.status of
    StatusCode code → code

data Exception = Exception String Dynamic
instance showException ∷ Show Exception where
  show (Exception message _) = message

type Client r = ExceptT Exception (Aff (ajax ∷ AJAX | r))

runClient ∷ ∀ r α. Client (exception ∷ EXCEPTION | r) α → Aff (ajax ∷ AJAX, exception ∷ EXCEPTION | r) α
runClient client = runExceptT client >>= either (throwError <<< error <<< show) pure

throwDynamicException ∷ ∀ e m α. (MonadError Exception m, Show e, Typeable e) ⇒ e → m α
throwDynamicException exc = throwError $ Exception (show exc) (toDynamic exc)

fromException ∷ ∀ α. Typeable α ⇒ Exception → Maybe α
fromException (Exception _ dyn) = fromDynamic dyn

data UnableToContactServer = UnableToContactServer
instance showUnableToContactServer ∷ Show UnableToContactServer where
  show _ = "Unable to contact the server."
instance typeableUnableToContactServer ∷ Typeable UnableToContactServer where
  typeOf _ = mkTyRep "Client" "UnableToContactServer"

data UnexpectedStatusCode = UnexpectedStatusCode Int
instance showUnexpectedStatusCode ∷ Show UnexpectedStatusCode where
  show (UnexpectedStatusCode code) = "Unexpected status code: " ⊕ show code
instance typeableUnexpectedStatusCode ∷ Typeable UnexpectedStatusCode where
  typeOf _ = mkTyRep "Client" "UnexpectedStatusCode"

attemptRequest ∷
  ∀ r α β.
  (Requestable α, Respondable β) ⇒
  AffjaxRequest α →
  Client r (AffjaxResponse β)
attemptRequest request =
  (lift <<< attempt <<< affjax $ request)
  >>=
  either (const $ throwDynamicException UnableToContactServer) pure

loginOrCreateAccount ∷ ∀ r. String → LoginInformation → Client r SessionInformation
loginOrCreateAccount route login_info = do
  response ∷ AffjaxResponse String ← attemptRequest $
    defaultRequest
      { method = Left POST
      , url = createURL login_info $
              route
          <> "?username="
          <> login_info.username
          <> "&password="
          <> login_info.password
      , content = Just <<< gEncodeJson $
          LoginAccount
            { username: login_info.username
            , password: login_info.password
            }
      }
  let code = responseStatusCode response
  when (code < 200 || code >= 300) $
    throwDynamicException $ UnexpectedStatusCode code
  pure $
    { hostname: login_info.hostname
    , port: login_info.port
    , token: response.response
    }

data AccountAlreadyExists = AccountAlreadyExists
instance showAccountAlreadyExists ∷ Show AccountAlreadyExists where
  show _ = "Account already exists."
instance typeableAccountAlreadyExists ∷ Typeable AccountAlreadyExists where
  typeOf _ = mkTyRep "Client" "AccountAlreadyExists"

createAccount ∷ ∀ r. LoginInformation → Client r SessionInformation
createAccount login_info =
  catchJust
    (\exc →
      case fromException exc of
        Nothing → Nothing
        Just (UnexpectedStatusCode code)
          | code == 409 → Just $ throwDynamicException AccountAlreadyExists
          | otherwise → Nothing
    )
    (loginOrCreateAccount "create" login_info)
    id

data NoSuchUser = NoSuchUser
instance showNoSuchUser ∷ Show NoSuchUser where
  show _ = "No such user."
instance typeableNoSuchUser ∷ Typeable NoSuchUser where
  typeOf _ = mkTyRep "Client" "NoSuchUser"

data InvalidPassword = InvalidPassword
instance showInvalidPassword ∷ Show InvalidPassword where
  show _ = "Invalid password."
instance typeableInvalidPassword ∷ Typeable InvalidPassword where
  typeOf _ = mkTyRep "Client" "InvalidPassword"

login ∷ ∀ r. LoginInformation → Client r SessionInformation
login login_info =
  catchJust
    (\exc →
      case fromException exc of
        Nothing → Nothing
        Just (UnexpectedStatusCode code)
          | code == 403 → Just $ throwDynamicException InvalidPassword
          | code == 404 → Just $ throwDynamicException NoSuchUser
          | otherwise → Nothing
    )
    (loginOrCreateAccount "login" login_info)
    id

sendRequest ∷
  ∀ r α β.
  (Requestable α, Respondable β) ⇒
  SessionInformation → Method → String → Array Int → α →
  Client r β
sendRequest session_info method path expected_codes content = do
  response ← attemptRequest $
    defaultRequest
      { method = Left method
      , url = createURL session_info path
      , content = Just content
      , headers = [RequestHeader "Authorization" ("Bearer " ⊕ session_info.token)]
      }
  case response.status of
    StatusCode code | not (code ∈ expected_codes) →
      throwDynamicException $ UnexpectedStatusCode code
    _ → pure response.response

data InvalidJson = InvalidJson { string ∷ String, message ∷ String }
instance showInvalidJson ∷ Show InvalidJson where
  show (InvalidJson {message,string}) =
    "Invalid JSON (" ⊕ message ⊕ "): " ⊕ string
instance typeableInvalidJson ∷ Typeable InvalidJson where
  typeOf _ = mkTyRep "Client" "InvalidJson"

sendRequestAndReceiveJson ∷
  ∀ r α β.
  (Requestable α, DecodeJson β) ⇒
  SessionInformation → Method → String → Array Int → α →
  Client r β
sendRequestAndReceiveJson session_info method path expected_codes content = do
  string ← sendRequest session_info method path expected_codes content
  case jsonParser string >>= decodeJson of
    Left message →
      throwDynamicException $ InvalidJson { message: message, string: string }
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

fetchHabits ∷ ∀ r. SessionInformation → Client r Habits
fetchHabits session_info = sendRequestAndReceiveJson session_info GET "habits" [200] unit
