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
import HabitOfFate.Data
import HabitOfFate.Habit
import HabitOfFate.Story

data SecureMode = Testing | Secure

data ServerInfo = ServerInfo
  { _server_hostname ∷ ByteString
  , _server_port ∷ Int
  , _secure_mode ∷ SecureMode
  , _manager ∷ Manager
  }
makeLenses ''ServerInfo

newServerInfo ∷ SecureMode → ByteString → Int → IO ServerInfo
newServerInfo secure_mode hostname port =
  fmap (ServerInfo hostname port secure_mode)
  ∘
  newManager
  $
  case secure_mode of
    Testing → defaultManagerSettings
    Secure → mkManagerSettings (TLSSettingsSimple True False False) Nothing

type Client = ReaderT ServerInfo IO

decodeUtf8InResponse ∷ Response LBS.ByteString → Text
decodeUtf8InResponse = decodeUtf8 ∘ LBS.toStrict ∘ responseBody

pathToHabit ∷ UUID → Text
pathToHabit = ("/habits/" ⊕) ∘ UUID.toText

makeRequest ∷ StdMethod → Text → Client Request
makeRequest std_method path = do
  server ← ask
  return $ defaultRequest
    { method = renderStdMethod std_method
    , host = server ^. server_hostname
    , secure = case server ^. secure_mode of
        Testing → False
        Secure → True
    , port = server ^. server_port
    , path = encodeUtf8 path
    }

addJSONBody ∷ ToJSON α ⇒ α → Request → Request
addJSONBody x request = request
  { requestHeaders = [(hContentType, "application/json; charset=utf-8")]
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

data UnexpectedStatus = UnexpectedStatus Status deriving (Typeable)
instance Show UnexpectedStatus where
  show (UnexpectedStatus status) = "Unexpected status: " ⊕ show status
instance Exception UnexpectedStatus where

sendRequest ∷ [Int] → Request → Client (Response LBS.ByteString)
sendRequest expected_codes request = do
  response ← view manager >>= liftIO ∘ httpLbs request
  if responseStatusCode response ∈ expected_codes
    then return response
    else throwM ∘ UnexpectedStatus ∘ responseStatus $ response

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
  void $ requestWithJSON PUT (pathToHabit habit_id) [200,201] habit

deleteHabit ∷ UUID → Client ()
deleteHabit habit_id =
  void $ request DELETE (pathToHabit habit_id) [204]

fetchHabit ∷ UUID → Client (Maybe Habit)
fetchHabit habit_id =
  (
    request GET (pathToHabit habit_id) [200]
    >>=
    parseResponseBody
  )
  `catch`
  \e@(UnexpectedStatus status) →
    if statusCode status == 404
      then return Nothing
      else throwM e

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits = request GET "/habits" [200] >>= parseResponseBody

getCredits ∷ Client Credits
getCredits = request GET "/mark" [200] >>= parseResponseBody

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
