{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import Prelude hiding (id)

import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (unless, when)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Data.UUID (UUID, fromText, toText)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))

import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Unicode

data ClientException =
    UnexpectedStatusCode Int S.Text
  | UnableToParseResponse S.Text
  deriving (Read,Show,Typeable)

instance Exception ClientException

createHabit ∷ ByteString → Int → Habit → IO UUID
createHabit hostname port habit = do
  response ←
    httpLBS
    ∘
    setRequestBodyJSON (
      makeDocWithWrappedObject
      ∘
      makeWrappedObject "habit"
      $
      habit
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
  let Status code message = getResponseStatus response
  unless (code == 201) $
    throwIO $ UnexpectedStatusCode code (decodeUtf8 message)
  case getResponseHeader "Location" response of
    [] → throwIO $ UnableToParseResponse "No location returned for created habit."
    [url] →
      let url_as_text = decodeUtf8 url
      in case fromText ∘ S.takeWhileEnd (/= '/') $ url_as_text of
        Nothing →
          throwIO
          ∘
          UnableToParseResponse
          $
          "Last part of location did not end with a UUID: " ⊕ url_as_text
        Just uuid → return uuid
    _ → throwIO $ UnableToParseResponse "Multiple locations returned for created habit."

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
  let Status code message = getResponseStatus response
  unless (code == 200) $
    throwIO $ UnexpectedStatusCode code (decodeUtf8 message)
  case eitherDecode (getResponseBody response) of
    Left error_message → throwIO $ UnableToParseResponse (S.pack error_message)
    Right doc → case doc ^. _data of
      MultipleWrappedObjects _ → throwIO $ UnableToParseResponse "Document had multiple habits."
      SingleWrappedObject w → do
        when (maybe False (== uuid) $ w ^. id) $
          throwIO $ UnableToParseResponse "A habit did not have an associated id."
        return $ w ^. attributes

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
  let Status code message = getResponseStatus response
  unless (code == 200) $
    throwIO $ UnexpectedStatusCode code (decodeUtf8 message)
  case eitherDecode (getResponseBody response) of
    Left error_message → throwIO $ UnableToParseResponse (S.pack error_message)
    Right doc → case doc ^. _data of
      SingleWrappedObject _ → throwIO $ UnableToParseResponse "Document only had a single habit."
      MultipleWrappedObjects ws →
        Map.fromList
        <$>
        mapM
          (\w →
            case w ^. id of
              Nothing → throwIO $ UnableToParseResponse "A habit did not have an associated id."
              Just uuid → return (uuid, w ^. attributes)
          )
          ws
