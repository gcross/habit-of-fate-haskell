{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import HabitOfFate.Prelude

import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Typeable
import Data.UUID
import qualified Data.UUID as UUID
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Text.XML

import HabitOfFate.Credits
import HabitOfFate.Account
import HabitOfFate.Habit
import HabitOfFate.Story

data SecureMode = Testing | Secure

data SessionInfo = SessionInfo
  { _request_template ∷ Request
  , _manager ∷ Manager
  }
makeLenses ''SessionInfo

loginOrCreateAccount ∷ String → String → String → SecureMode → ByteString → Int → IO (Either Status SessionInfo)
loginOrCreateAccount route username password secure_mode hostname port = do
  manager ← newManager $
    case secure_mode of
      Testing → defaultManagerSettings
      Secure → mkManagerSettings (TLSSettingsSimple True False False) Nothing
  let request_template_without_authorization = defaultRequest
        { method = renderStdMethod POST
        , host = hostname
        , secure = case secure_mode of
            Testing → False
            Secure → True
        , port = port
        }
  response ←
    flip httpLbs manager
    $
    request_template_without_authorization
      { path = encodeUtf8 (pack $ printf "/%s?username=%s&password=%s" route username password) }
  let code = responseStatusCode response
  if code >= 200 && code <= 299
    then return ∘ Right $
      SessionInfo
        (request_template_without_authorization
          { requestHeaders =
              [("Authorization", ("Bearer " ⊕) ∘ view strict ∘ responseBody $ response)]
          }
        )
        manager
    else return ∘ Left $ responseStatus response

createAccount ∷ String → String → SecureMode → ByteString → Int → IO (Maybe SessionInfo)
createAccount username password secure_mode hostname port =
  either (const Nothing) Just
  <$>
  loginOrCreateAccount "create" username password secure_mode hostname port

data LoginError = NoSuchAccount | InvalidPassword deriving (Eq, Ord, Show)

login ∷ String → String → SecureMode → ByteString → Int → IO (Either LoginError SessionInfo)
login username password secure_mode hostname port =
  (_Left %~
    (\case
      403 → InvalidPassword
      404 → NoSuchAccount
    )
    ∘
    statusCode
  )
  <$>
  loginOrCreateAccount "login" username password secure_mode hostname port

type Client = ReaderT SessionInfo IO

decodeUtf8InResponse ∷ Response LBS.ByteString → Text
decodeUtf8InResponse = decodeUtf8 ∘ LBS.toStrict ∘ responseBody

pathToHabit ∷ UUID → Text
pathToHabit = ("/habits/" ⊕) ∘ UUID.toText

makeRequest ∷ StdMethod → Text → Client Request
makeRequest std_method path = do
  view request_template
  <&>
  \template → template { method = renderStdMethod std_method, path = encodeUtf8 path }

addJSONBody ∷ ToJSON α ⇒ α → Request → Request
addJSONBody x request = request
  { requestHeaders = (hContentType, "application/json; charset=utf-8"):requestHeaders request
  , requestBody = RequestBodyLBS ∘ encode $ x
  }

data InvalidJSON = InvalidJSON String deriving (Typeable)
instance Show InvalidJSON where
  show (InvalidJSON doc) = "Invalid JSON: " ⊕ show doc
instance Exception InvalidJSON where

parseResponseBody ∷ FromJSON α ⇒ Response LBS.ByteString → Client α
parseResponseBody =
  either (throwM ∘ InvalidJSON) return
  ∘
  eitherDecode'
  ∘
  responseBody

responseStatusCode = statusCode ∘ responseStatus

data UnexpectedStatus = UnexpectedStatus [Int] Status deriving (Typeable)
instance Show UnexpectedStatus where
  show (UnexpectedStatus expected_codes status) =
    printf "Status code not one of %s: %s" (show expected_codes) (show status)
instance Exception UnexpectedStatus where

sendRequest ∷ [Int] → Request → Client (Response LBS.ByteString)
sendRequest expected_codes request = do
  response ← view manager >>= liftIO ∘ httpLbs request
  if responseStatusCode response ∈ expected_codes
    then return response
    else throwM ∘ UnexpectedStatus expected_codes ∘ responseStatus $ response

request ∷ StdMethod → Text → [Int] → Client (Response LBS.ByteString)
request method path expected_codes = do
  makeRequest method path
  >>=
  sendRequest expected_codes

requestWithJSON ∷ ToJSON α ⇒ StdMethod → Text → [Int] → α → Client (Response LBS.ByteString)
requestWithJSON method path expected_codes value = do
  (makeRequest method path <&> addJSONBody value)
  >>=
  sendRequest expected_codes

putHabit ∷ UUID → Habit → Client ()
putHabit habit_id habit =
  void $ requestWithJSON PUT (pathToHabit habit_id) [201,204] habit

data DeleteResult = HabitDeleted | NoHabitToDelete deriving (Eq, Ord, Read, Show)

deleteHabit ∷ UUID → Client DeleteResult
deleteHabit habit_id =
  (HabitDeleted <$ request DELETE (pathToHabit habit_id) [204])
  `catch`
  \e@(UnexpectedStatus _ status) →
    if statusCode status == 404
      then pure NoHabitToDelete
      else throwM e

fetchHabit ∷ UUID → Client (Maybe Habit)
fetchHabit habit_id =
  (
    request GET (pathToHabit habit_id) [200]
    >>=
    parseResponseBody
  )
  `catch`
  \e@(UnexpectedStatus _ status) →
    if statusCode status == 404
      then return Nothing
      else throwM e

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits = request GET "/habits" [200] >>= parseResponseBody

getCredits ∷ Client Credits
getCredits = request GET "/credits" [200] >>= parseResponseBody

markHabits ∷ [UUID] → [UUID] → Client Credits
markHabits success_habits failure_habits =
  requestWithJSON POST "/mark" [200] (HabitsToMark success_habits failure_habits)
  >>=
  parseResponseBody

runGame ∷ Client Story
runGame =
  request POST "/run" [200]
  >>=
  either throwM return ∘ parseText def ∘ decodeUtf8 ∘ responseBody
  >>=
  either error return ∘ parseStoryFromDocument
