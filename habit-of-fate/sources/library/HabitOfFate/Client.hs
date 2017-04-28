{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        , requestBody =
            RequestBodyBS
            ∘
            encodeUtf8
            ∘
            pack
            $
            [i|username=#{username}&password=#{password}|]
        }
  response ←
    flip httpLbs manager
    $
    request_template_without_authorization
      { path = encodeUtf8 ∘ pack $ [i|/api/#{route}|] }
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
pathToHabit = ("habits/" ⊕) ∘ UUID.toText

makeRequest ∷ StdMethod → Text → Client Request
makeRequest std_method path = do
  view request_template
  <&>
  \template → template { method = renderStdMethod std_method, path = encodeUtf8 $ "/api/" ⊕ path }

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

data UnexpectedStatus = UnexpectedStatus [Int] Int deriving (Typeable)
instance Show UnexpectedStatus where
  show (UnexpectedStatus expected_codes status) =
    [i|Status code not one of #{expected_codes}: #{status}|]
instance Exception UnexpectedStatus where

sendRequest ∷ Request → Client (Response LBS.ByteString)
sendRequest request =
  view manager
  >>=
  liftIO ∘ httpLbs request

request ∷ StdMethod → Text → Client (Response LBS.ByteString)
request method path =
  makeRequest method path
  >>=
  sendRequest

requestWithJSON ∷ ToJSON α ⇒ StdMethod → Text → α → Client (Response LBS.ByteString)
requestWithJSON method path value = do
  (makeRequest method path <&> addJSONBody value)
  >>=
  sendRequest

data PutResult = HabitCreated | HabitReplaced deriving (Eq, Ord, Read, Show)

putHabit ∷ UUID → Habit → Client PutResult
putHabit habit_id habit = do
  response ← requestWithJSON PUT (pathToHabit habit_id) habit
  case responseStatusCode response of
    201 → pure HabitCreated
    204 → pure HabitReplaced
    code → throwM $ UnexpectedStatus [201,204] code

data DeleteResult = HabitDeleted | NoHabitToDelete deriving (Eq, Ord, Read, Show)

deleteHabit ∷ UUID → Client DeleteResult
deleteHabit habit_id = do
  response ← request DELETE $ pathToHabit habit_id
  case responseStatusCode response of
    204 → pure HabitDeleted
    404 → pure NoHabitToDelete
    code → throwM $ UnexpectedStatus [204,404] code

getHabit ∷ UUID → Client (Maybe Habit)
getHabit habit_id = do
  response ← request GET $ pathToHabit habit_id
  case responseStatusCode response of
    200 → parseResponseBody response
    404 → pure Nothing
    code → throwM $ UnexpectedStatus [200,404] code

getHabits ∷ Client (Map UUID Habit)
getHabits = do
  response ← request GET "habits"
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

getCredits ∷ Client Credits
getCredits = do
  response ← request GET "credits"
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

markHabits ∷ [UUID] → [UUID] → Client Credits
markHabits success_habits failure_habits = do
  response ← requestWithJSON POST "mark" (HabitsToMark success_habits failure_habits)
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

runGame ∷ Client Story
runGame = do
  response ← request POST "run"
  case responseStatusCode response of
    200 →
      (either throwM return ∘ parseText def ∘ decodeUtf8 ∘ responseBody $ response)
      >>=
      (either error return ∘ parseStoryFromDocument)
    code → throwM $ UnexpectedStatus [200] code
