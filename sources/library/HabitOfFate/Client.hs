{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import HabitOfFate.Prelude

import Control.Exception
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Typeable
import Data.UUID (UUID, fromText, toText)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import System.Log.Logger
import System.IO.Error
import Text.XML

import HabitOfFate.Credits
import HabitOfFate.Data
import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Story

decodeUtf8Lazy = decodeUtf8 ∘ (^. strict)

debug = liftIO ∘ debugM "HabitOfFate.Client" ∘ unpack

data ServerInfo = ServerInfo
  { _hostname ∷ ByteString
  , _port ∷ Int
  }
makeLenses ''ServerInfo

type Client = ReaderT ServerInfo IO

data ClientException = ClientException Int String deriving (Eq, Typeable)

instance Show ClientException where
  show (ClientException code message) =
    printf "Received unexpected response code %i: %s" code message

instance Exception ClientException where

expectSuccess ∷ Response α → Client ()
expectSuccess response =
  unless (code >= 200 && code <= 299)
  ∘
  throwM
  $
  ClientException code (unpack $ decodeUtf8 message)
  where
    Status code message = getResponseStatus response

pathToHabit ∷ UUID → Text
pathToHabit = ("/habits/" ⊕) ∘ toText

makeRequest ∷ Text → Text → Client Request
makeRequest method path = do
  server ← ask
  return
    ∘
    setRequestMethod (encodeUtf8 method)
    ∘
    setRequestPath (encodeUtf8 path)
    ∘
    setRequestHost (server ^. hostname)
    ∘
    setRequestPort (server ^. port)
    $
    defaultRequest

parseDoc ∷ FromJSON α ⇒ Response LBS.ByteString → Client α
parseDoc response =
  case eitherDecode (getResponseBody response) of
    Left error_message →
      fail
      $
      printf "Failed parsing JSON with error message %s: %s"
        error_message
        (decodeUtf8Lazy ∘ getResponseBody $ response ∷ Text)
    Right value → return value

request ∷ Text → Text → Client (Response LBS.ByteString)
request method path = do
  response ← makeRequest method path >>= httpLBS
  expectSuccess response
  return response

requestMaybe ∷ Text → Text → Client (Maybe (Response LBS.ByteString))
requestMaybe method path = do
  response ← makeRequest method path >>= httpLBS
  if statusCode (getResponseStatus response) == 404
    then return Nothing
    else do
      expectSuccess response
      return ∘ Just $ response

requestWithBody ∷ Text → Text → Value → Client (Response LBS.ByteString)
requestWithBody method path value = do
  response ←
    (makeRequest method path <&> setRequestBodyJSON value)
    >>=
    httpLBS
  expectSuccess response
  return response

postHabit ∷ Habit → Client UUID
postHabit habit =
  requestWithBody "POST" "/habits" (toJSON habit)
  >>=
  (\body →
    maybe
      (fail $ printf "Error parsing UUID: %s" body)
      return
    $
    fromText body
  )
  ∘
  decodeUtf8Lazy
  ∘
  getResponseBody

deleteHabit ∷ UUID → Client ()
deleteHabit uuid =
  void $ request "DELETE" (pathToHabit uuid)

fetchHabit ∷ UUID → Client (Maybe Habit)
fetchHabit uuid = do
  requestMaybe "GET" (pathToHabit uuid)
  >>=
  \case
    Nothing → return Nothing
    Just response → do
      debug $ "Result of fetch was " ⊕ (decodeUtf8Lazy ∘ getResponseBody $ response)
      parseDoc response

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits =
  request "GET" "/habits"
  >>=
  fmap (
    mapFromList
    ∘
    map (view uuid &&& identity)
  )
  ∘
  parseDoc

getCredits ∷ Client Credits
getCredits = request "GET" "/mark" >>= parseDoc

markHabits ∷ [UUID] → [UUID] → Client Credits
markHabits success_habits failure_habits =
  requestWithBody "POST" "/mark" (toJSON $ HabitsToMark success_habits failure_habits)
  >>=
  parseDoc

runGame ∷ Client Story
runGame =
  request "POST" "/run"
  >>=
  either throwM return
  ∘
  (
    parseLBS def
    >=>
    (_Left %~ (toException ∘ userError))
    ∘
    parseStoryFromDocument
  )
  ∘
  getResponseBody
