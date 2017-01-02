{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import Prelude hiding (id)

import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (unless)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as S
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))

import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Unicode

data FetchException =
    UnexpectedStatusCode Int S.Text
  | UnableToParseResponse S.Text
  deriving (Read,Show,Typeable)

instance Exception FetchException

fetchHabits ∷ ByteString → Int → IO (Map UUID Habit)
fetchHabits hostname port = do
  response ←
    httpJSON
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
  case parseEither parseJSON (getResponseBody response) of
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
