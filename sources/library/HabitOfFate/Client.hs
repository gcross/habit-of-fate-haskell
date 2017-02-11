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
import Data.UUID (UUID, fromText, toText)
import qualified Data.UUID as UUID
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import Text.XML

import HabitOfFate.Credits
import HabitOfFate.Data
import HabitOfFate.Habit
import HabitOfFate.Story

data ServerInfo = ServerInfo
  { _hostname ∷ ByteString
  , _port ∷ Int
  }
makeLenses ''ServerInfo

type Client = ReaderT ServerInfo IO

decodeUtf8InResponse ∷ Response LBS.ByteString → Text
decodeUtf8InResponse = decodeUtf8 ∘ LBS.toStrict ∘ getResponseBody

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
  getResponseBody

data UnexpectedStatus = UnexpectedStatus Status deriving (Typeable)
instance Show UnexpectedStatus where
  show (UnexpectedStatus status) = "Unexpected status: " ⊕ show status
instance Exception UnexpectedStatus where

sendRequest ∷ [Int] → Request → Client (Response LBS.ByteString)
sendRequest expected_codes =
  liftIO ∘ httpLBS
  >=>
  (\response →
    if getResponseStatusCode response ∈ expected_codes
      then return response
      else throwM ∘ UnexpectedStatus ∘ getResponseStatus $ response
  )

request ∷ Text → Text → [Int] → Client (Response LBS.ByteString)
request method path expected_codes = do
  makeRequest method path
  >>=
  sendRequest expected_codes

requestWithJSON ∷ ToJSON α ⇒ Text → Text → [Int] → α → Client (Response LBS.ByteString)
requestWithJSON method path expected_codes value = do
  (makeRequest method path <&> setRequestBodyJSON value)
  >>=
  sendRequest expected_codes

postHabit ∷ Habit → Client UUID
postHabit habit =
  requestWithJSON "POST" "/habits" [200,201] habit
  <&>
  \response →
    if UUID.null (habit ^. uuid)
      then
        let uuid_text = decodeUtf8InResponse response
        in fromMaybe (error $ "Invalid UUID: " ⊕ show uuid_text) (fromText uuid_text)
      else habit ^. uuid

deleteHabit ∷ UUID → Client ()
deleteHabit habit_id =
  void $ request "DELETE" (pathToHabit habit_id) [204]

fetchHabit ∷ UUID → Client (Maybe Habit)
fetchHabit habit_id =
  (
    request "GET" (pathToHabit habit_id) [200]
    >>=
    parseResponseBody
  )
  `catch`
  \e@(UnexpectedStatus status) →
    if statusCode status == 404
      then return Nothing
      else throwM e

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits =
  request "GET" "/habits" [200]
  >>=
  fmap (
    mapFromList
    ∘
    map (view uuid &&& identity)
  )
  ∘
  parseResponseBody

getCredits ∷ Client Credits
getCredits = request "GET" "/mark" [200] >>= parseResponseBody

markHabits ∷ [UUID] → [UUID] → Client Credits
markHabits success_habits failure_habits =
  requestWithJSON "POST" "/mark" [200] (HabitsToMark success_habits failure_habits)
  >>=
  parseResponseBody

runGame ∷ Client Story
runGame =
  request "POST" "/run" [200]
  >>=
  either throwM return ∘ parseText def ∘ decodeUtf8 ∘ getResponseBody
  >>=
  either error return ∘ parseStoryFromDocument
