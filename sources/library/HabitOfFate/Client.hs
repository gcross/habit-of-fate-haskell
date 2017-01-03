{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import Prelude hiding (id)

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID, fromText, toText)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import System.Log.Logger
import Text.Printf

import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.TH
import HabitOfFate.Unicode

debug = liftIO ∘ debugM "HabitOfFate.Client"

data ServerInfo = ServerInfo
  { _hostname ∷ ByteString
  , _port ∷ Int
  }
makeLenses ''ServerInfo

type Client = ReaderT ServerInfo IO

expectSuccess ∷ Response α → Client ()
expectSuccess response =
  unless (code >= 200 && code <= 299)
  ∘
  fail
  $
  printf "Received error code %i: %s"
    code
    (decodeUtf8 message)
  where
    Status code message = getResponseStatus response

failParse ∷ String → Client α
failParse = fail ∘ ("Failed parse: " ⊕)

pathToHabit ∷ UUID → S.Text
pathToHabit = ("/habits/" ⊕) ∘ toText

makeRequest ∷ S.Text → S.Text → Client Request
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

parseDoc ∷ Response LBS.ByteString → UnmakeJSON α → Client α
parseDoc response parser =
  case eitherDecode (getResponseBody response) of
    Left error_message → failParse error_message
    Right doc → either failParse return ∘ unmakeJSON doc $ parser

request ∷ S.Text → S.Text → Client (Response LBS.ByteString)
request method path = do
  response ← makeRequest method path >>= httpLBS
  expectSuccess response
  return response

requestWithBody ∷ S.Text → S.Text → Value → Client (Response LBS.ByteString)
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
    makeJSON $ do
      addObject "data" $ do
        addText "type" "habit"
        add "attributes" habit
  case getResponseHeader "Location" response of
    [] → failParse "No location returned for created habit."
    [url] →
      let url_as_text = decodeUtf8 url
      in case fromText ∘ S.takeWhileEnd (/= '/') $ url_as_text of
        Nothing →
          failParse
          $
          "Last part of location did not end with a UUID: " ⊕ S.unpack url_as_text
        Just uuid → return uuid
    _ → failParse "Multiple locations returned for created habit."

deleteHabit ∷ UUID → Client ()
deleteHabit uuid =
  void $ request "DELETE" (pathToHabit uuid)

fetchHabit ∷ UUID → Client Habit
fetchHabit uuid = do
  response ← request "GET" (pathToHabit uuid)
  debug $ "Result of fetch was " ⊕ (LBS.unpack ∘ getResponseBody $ response)
  parseDoc response ∘ retrieveObject "data" $ do
    checkTypeIs "habit"
    checkIdIfPresentIs uuid
    retrieve "attributes"

fetchHabits ∷ Client (Map UUID Habit)
fetchHabits =
  Map.fromList
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

replaceHabit ∷ UUID → Habit → Client ()
replaceHabit uuid habit =
  void
  ∘
  requestWithBody "PUT" (pathToHabit uuid)
  $
  makeJSON $ do
    addObject "data" $ do
      addText "type" "habit"
      add "attributes" habit
