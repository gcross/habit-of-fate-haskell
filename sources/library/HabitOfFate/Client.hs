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

import HabitOfFate.Habit
import HabitOfFate.JSON

decodeUtf8Lazy = decodeUtf8 ∘ (^. strict)

debug = liftIO ∘ debugM "HabitOfFate.Client" ∘ (^. from packed)

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
  ClientException code (decodeUtf8 message ^. unpacked)
  where
    Status code message = getResponseStatus response

failParse ∷ String → Client α
failParse = fail ∘ ("Failed parse: " ⊕)

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

parseDoc ∷ Response LBS.ByteString → JSONParser α → Client α
parseDoc response parser =
  case eitherDecode (getResponseBody response) of
    Left error_message → failParse error_message
    Right doc → either failParse return ∘ runJSONParser doc $ parser

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

createHabit ∷ Habit → Client UUID
createHabit habit = do
  response ←
    requestWithBody "PUT" "/habits"
    $
    runJSONCreator $ do
      addObject "data" $ do
        addText "type" "habit"
        add "attributes" habit
  case getResponseHeader "Location" response of
    [] → failParse "No location returned for created habit."
    [url] →
      let url_as_text = decodeUtf8 url
      in case fromText ∘ Text.takeWhileEnd (/= '/') $ url_as_text of
        Nothing →
          failParse
          $
          "Last part of location did not end with a UUID: " ⊕ (url_as_text ^. from packed)
        Just uuid → return uuid
    _ → failParse "Multiple locations returned for created habit."

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
      parseDoc response ∘ retrieveObject "data" $ do
        checkTypeIs "habit"
        checkIdIfPresentIs uuid
        retrieve "attributes"

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits =
  mapFromList
  <$>
  (
    request "GET" "/habits"
    >>=
    (flip parseDoc
      ∘
      retrieveObjects "data"
      $
      do checkTypeIs "habit"
         (,) <$> retrieve "id" <*> retrieve "attributes"
    )
  )

getCredits ∷ Client (Double, Double)
getCredits =
  request "GET" "/mark"
  >>=
  (flip parseDoc $ (,) <$> retrieve "success" <*> retrieve "failure")

markHabits ∷ [UUID] → [UUID] → Client ()
markHabits success_habits failure_habits =
  void
  ∘
  requestWithBody "POST" "/mark"
  $
  runJSONCreator $ do
    add "success" success_habits
    add "failure" failure_habits

replaceHabit ∷ UUID → Habit → Client ()
replaceHabit uuid habit =
  void
  ∘
  requestWithBody "PUT" (pathToHabit uuid)
  $
  runJSONCreator $ do
    addObject "data" $ do
      addText "type" "habit"
      add "attributes" habit

runGame ∷ Client Text
runGame =
  (decodeUtf8Lazy ∘ getResponseBody)
  <$>
  request "POST" "/run"
