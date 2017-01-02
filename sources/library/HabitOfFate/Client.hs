{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import Prelude hiding (id)

import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (unless, when)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Data.UUID (UUID, fromText, toText)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import System.Log.Logger
import Text.Printf

import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Unicode

debug = debugM "HabitOfFate.Client"

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

failParse ∷ String → IO α
failParse = fail ∘ ("Failed parse: " ⊕)

createHabit ∷ ByteString → Int → Habit → IO UUID
createHabit hostname port habit = do
  response ←
    httpLBS
    ∘
    setRequestBodyJSON (
      makeJSON $ do
        addObject "data" $ do
          addText "type" "habit"
          add "attributes" habit
    )
    ∘
    setRequestMethod "PUT"
    ∘
    setRequestHost hostname
    ∘
    setRequestPort port
    ∘
    setRequestPath "/habits"
    $
    defaultRequest
  expectSuccess response
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

deleteHabit ∷ ByteString → Int → UUID → IO ()
deleteHabit hostname port uuid = do
  response ←
    httpLBS
    ∘
    setRequestMethod "DELETE"
    ∘
    setRequestHost hostname
    ∘
    setRequestPort port
    ∘
    setRequestPath (encodeUtf8 $ "/habits/" ⊕ toText uuid)
    $
    defaultRequest
  expectSuccess response

fetchHabit ∷ ByteString → Int → UUID → IO Habit
fetchHabit hostname port uuid = do
  response ←
    httpLBS
    ∘
    setRequestMethod "GET"
    ∘
    setRequestHost hostname
    ∘
    setRequestPort port
    ∘
    setRequestPath (encodeUtf8 $ "/habits/" ⊕ toText uuid)
    $
    defaultRequest
  expectSuccess response
  debug $
    "Result of fetch was " ⊕ (LBS.unpack ∘ getResponseBody $ response)
  case eitherDecode (getResponseBody response) of
    Left error_message → failParse error_message
    Right doc → either failParse return ∘ unmakeJSON doc ∘ retrieveObject "data" $ do
      checkTypeIs "habit"
      checkIdIfPresentIs uuid
      retrieve "attributes"

fetchHabits ∷ ByteString → Int → IO (Map UUID Habit)
fetchHabits hostname port = do
  response ←
    httpLBS
    ∘
    setRequestMethod "GET"
    ∘
    setRequestHost hostname
    ∘
    setRequestPort port
    ∘
    setRequestPath "/habits"
    $
    defaultRequest
  expectSuccess response
  case eitherDecode (getResponseBody response) of
    Left error_message → failParse error_message
    Right doc →
      Map.fromList
      <$>
      (
        either failParse return
        ∘
        unmakeJSON doc
        ∘
        retrieveObjects "data"
        $
        do checkTypeIs "habit"
           (,) <$> retrieve "id" <*> retrieve "attributes"
      )
