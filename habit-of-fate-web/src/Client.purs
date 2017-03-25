module Client where

import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.MonadZero
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Parser
import Data.Argonaut.Printer
import Data.Array hiding (fromFoldable)
import Data.Dynamic
import Data.Either
import Data.Generic
import Data.HTTP.Method hiding (fromString)
import Data.Maybe
import Data.Monoid
import Data.String hiding (null)
import Data.StrMap
import Data.Tuple
import Data.Typeable
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.RequestHeader
import Network.HTTP.StatusCode

import Unicode
import UUID

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

loginOrCreateAccount ∷
  ∀ r.
  String → LoginInformation →
  Client r SessionInformation
loginOrCreateAccount route login_info = do
  response ← attemptRequest $
    defaultRequest
      { method = Left POST
      , url = createURL login_info $
              route
          <> "?username="
          <> login_info.username
          <> "&password="
          <> login_info.password
      }
  let code = responseStatusCode response
  if (code < 200 || code >= 300)
    then throwDynamicException $ UnexpectedStatusCode code
    else pure $
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

noContent ∷ Maybe Unit
noContent = Nothing

type Response α = { status ∷ Int, body ∷ α }

sendRequest ∷
  ∀ r α β.
  (Requestable α, Respondable β) ⇒
  SessionInformation → Method → String → Maybe α →
  Client r (Response β)
sendRequest session_info method path maybe_content = do
  response ← attemptRequest $
    defaultRequest
      { method = Left method
      , url = createURL session_info path
      , content = maybe_content
      , headers = [RequestHeader "Authorization" ("Bearer " ⊕ session_info.token)]
      }
  case response.status of
    StatusCode code
      | code < 200 || code >= 300 → throwDynamicException $ UnexpectedStatusCode code
      | otherwise → pure $ { status: code, body: response.response }

data InvalidJson = InvalidJson { string ∷ String, message ∷ String }
instance showInvalidJson ∷ Show InvalidJson where
  show (InvalidJson {message,string}) =
    "Invalid JSON (" ⊕ message ⊕ "): " ⊕ string
instance typeableInvalidJson ∷ Typeable InvalidJson where
  typeOf _ = mkTyRep "Client" "InvalidJson"

sendRequestAndReceiveJson ∷
  ∀ r α β.
  (Requestable α, DecodeJson β) ⇒
  SessionInformation → Method → String → Maybe α →
  Client r (Response β)
sendRequestAndReceiveJson session_info method path maybe_content = do
  response ← sendRequest session_info method path maybe_content
  case jsonParser response.body >>= decodeJson of
    Left message →
      throwDynamicException $ InvalidJson { message: message, string: response.body }
    Right value → pure $ response { body = value }

assertNoExtraKeys ∷ JObject → Array String → Either String Unit
assertNoExtraKeys jobject expected_keys =
  when (not <<< null $ extra_keys) <<< Left $ "extra keys: " ⊕ show extra_keys
  where
    extra_keys = do
      key ← keys jobject
      guard $ key ∉ expected_keys

newtype Credits = Credits { success ∷ Number, failure ∷ Number }
derive instance eqCredits ∷ Eq Credits
derive instance genericCredits ∷ Generic Credits
derive instance ordCredits ∷ Ord Credits
instance showCredits ∷ Show Credits where
  show (Credits credits) =
      "Credits { success = "
    ⊕ show credits.success
    ⊕ ", failure = "
    ⊕ show credits.failure
    ⊕ " } "
instance decodeJsonCredits ∷ DecodeJson Credits where
  decodeJson json =
    case toObject json of
      Nothing → Left "expected an object"
      Just jobject → do
        failure ← jobject .? "failure"
        success ← jobject .? "success"
        assertNoExtraKeys jobject ["failure", "success"]
        pure $ Credits { failure: failure, success: success }
instance encodeJsonCredits ∷ EncodeJson Credits where
  encodeJson (Credits credits) =
    fromObject
    $
    fromFoldable
      [Tuple "success" $ fromNumber credits.success
      ,Tuple "failure" $ fromNumber credits.failure
      ]

newtype Habit = Habit { name ∷ String, credits ∷ Credits }
derive instance eqHabit ∷ Eq Habit
derive instance genericHabit ∷ Generic Habit
derive instance ordHabit ∷ Ord Habit
instance showHabit ∷ Show Habit where
  show (Habit habit) =
      "Habit { name = "
    ⊕ show habit.name
    ⊕ ", credits = "
    ⊕ show habit.credits
    ⊕ " } "
instance decodeJsonHabit ∷ DecodeJson Habit where
  decodeJson json =
    case toObject json of
      Nothing → Left "expected an object"
      Just jobject → do
        name ← jobject .? "name"
        credits ← jobject .? "credits" >>= decodeJson
        assertNoExtraKeys jobject ["name", "credits"]
        pure $ Habit { name: name, credits: credits }
instance encodeJsonHabit ∷ EncodeJson Habit where
  encodeJson (Habit habit) =
    fromObject
    $
    fromFoldable
      [Tuple "name" $ fromString habit.name
      ,Tuple "credits" $ encodeJson habit.credits
      ]

type HabitId = String
type Habits = StrMap Habit

getHabits ∷ ∀ r. SessionInformation → Client r Habits
getHabits session_info =
  sendRequestAndReceiveJson session_info GET "habits" noContent
  <#>
  (_.body)

data NoSuchHabit = NoSuchHabit
instance showNoSuchHabit ∷ Show NoSuchHabit where
  show _ = "No such habit."
instance typeableNoSuchHabit ∷ Typeable NoSuchHabit where
  typeOf _ = mkTyRep "Client" "NoSuchHabit"

getHabit ∷ ∀ r. SessionInformation → UUID → Client r Habit
getHabit session_info uuid =
  catchJust
    (\exc →
      case fromException exc of
        Nothing → Nothing
        Just (UnexpectedStatusCode code)
          | code == 404 → Just $ throwDynamicException NoSuchHabit
          | otherwise → Nothing
    )
    (sendRequestAndReceiveJson session_info GET ("habits/" ⊕ uuid) noContent
     <#>
     (_.body)
    )
    id

putHabit ∷ ∀ r. SessionInformation → UUID → Habit → Client r Unit
putHabit session_info uuid habit =
  sendRequest session_info PUT ("habits/" ⊕ uuid) (Just $ encodeJson habit)
  <#>
  (_.body)

data DeleteResult = HabitDeleted | NoHabitToDelete
derive instance eqDeleteResult ∷ Eq DeleteResult
derive instance ordDeleteResult ∷ Ord DeleteResult
instance showDeleteResult ∷ Show DeleteResult where
  show HabitDeleted = "HabitDeleted"
  show NoHabitToDelete = "NoHabitToDelete"

deleteHabit ∷ ∀ r. SessionInformation → UUID → Client r DeleteResult
deleteHabit session_info uuid =
  catchJust
    (\exc →
      case fromException exc of
        Nothing → Nothing
        Just (UnexpectedStatusCode code)
          | code == 404 → Just $ pure NoHabitToDelete
          | otherwise → Nothing
    )
    (sendRequest session_info DELETE ("habits/" ⊕ uuid) noContent
     <#>
     \(_ ∷ Response Unit) → HabitDeleted
    )
    id
