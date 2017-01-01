{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Client where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Text as S
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))

import HabitOfFate.Habit
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
  case parseEither parseHabitsDoc (getResponseBody response) of
    Left error_message → throwIO $ UnableToParseResponse (S.pack error_message)
    Right habits → return habits
