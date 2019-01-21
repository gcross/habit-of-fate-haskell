{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.API where

import HabitOfFate.Prelude

import Blaze.ByteString.Builder
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as Lazy
import Data.Time.LocalTime (LocalTime)
import Data.Typeable
import Data.UUID hiding (toByteString)
import qualified Data.UUID as UUID
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types
import Web.Cookie

import HabitOfFate.Data.Account
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged

data SecureMode = Testing | Secure

type SessionInfo = Request

loginOrCreateAccount ∷ String → String → String → SecureMode → ByteString → Int → IO (Either Status SessionInfo)
loginOrCreateAccount route username password secure_mode hostname port = do
  let request_template_without_authorization = defaultRequest
        { method = renderStdMethod POST
        , host = hostname
        , secure = case secure_mode of
            Testing → False
            Secure → True
        , port = port
        , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        , requestBody =
            [i|username=#{username}&password=#{password}|]
            |> pack
            |> encodeUtf8
            |> RequestBodyBS
        }
  response ←
    httpLBS
    $
    request_template_without_authorization
      { path = [i|/api/#{route}|] |> pack |> encodeUtf8 }
  let code = responseStatusCode response
  pure $
    if code >= 200 && code <= 299
      then
        maybe
          (Left internalServerError500)
          (\token → Right $
            request_template_without_authorization
            { requestHeaders =
              [("Cookie", toLazyByteString >>> view strict $
                renderCookiesText [("token", token)])]
            }
          )
          (
            (response |> responseHeaders |> lookup "Set-Cookie")
            >>=
            (parseCookiesText >>> lookup "token")
          )
    else Left $ responseStatus response

createAccount ∷ String → String → SecureMode → ByteString → Int → IO (Maybe SessionInfo)
createAccount username password secure_mode hostname port =
  either (const Nothing) Just
  <$>
  loginOrCreateAccount "create" username password secure_mode hostname port

data LoginError = NoSuchAccount | InvalidPassword | UnexpectedLoginErrorCode Int deriving (Eq, Ord, Show)

login ∷ String → String → SecureMode → ByteString → Int → IO (Either LoginError SessionInfo)
login username password secure_mode hostname port =
  (_Left %~ (
    statusCode
    >>>
    (\case
      403 → InvalidPassword
      404 → NoSuchAccount
      code → UnexpectedLoginErrorCode code
    )
  ))
  <$>
  loginOrCreateAccount "login" username password secure_mode hostname port

type InnerClientAction = ReaderT SessionInfo

newtype SessionT m α = SessionT { unwrapSessionT ∷ InnerClientAction m α }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

type SessionIO = SessionT IO

runSessionT ∷ MonadIO m ⇒ SessionT m α → SessionInfo → m α
runSessionT action session =
  (action <* logout) |> unwrapSessionT |> flip runReaderT session

instance MonadBase IO m ⇒ MonadBase IO (SessionT m) where
  liftBase = liftBase >>> SessionT

instance MonadBaseControl IO SessionIO where
  type StM SessionIO α = StM (InnerClientAction IO) α
  liftBaseWith f = SessionT $ liftBaseWith $ \r → f (unwrapSessionT >>> r)
  restoreM = restoreM >>> SessionT

getRequestTemplate ∷ Monad m ⇒ SessionT m Request
getRequestTemplate = ask |> SessionT

decodeUtf8InResponse ∷ Response LBS.ByteString → Text
decodeUtf8InResponse = responseBody >>> LBS.toStrict >>> decodeUtf8

pathToGroup ∷ UUID → Text
pathToGroup = UUID.toText >>> ("groups/" ⊕)

pathToHabit ∷ UUID → Text
pathToHabit = UUID.toText >>> ("habits/" ⊕)

makeRequest ∷ Monad m ⇒ StdMethod → Text → SessionT m Request
makeRequest std_method path = do
  getRequestTemplate
  <&>
  \template → template
    { method = renderStdMethod std_method
    , path = encodeUtf8 $ "/api/" ⊕ path
    }

addJSONBody ∷ ToJSON α ⇒ α → Request → Request
addJSONBody x request = request
  { requestHeaders = (hContentType, "application/json; charset=utf-8"):requestHeaders request
  , requestBody = x |> encode |> RequestBodyLBS
  }

data InvalidJSON = InvalidJSON String deriving (Typeable)
instance Show InvalidJSON where
  show (InvalidJSON doc) = "Invalid JSON: " ⊕ show doc
instance Exception InvalidJSON where

parseResponseBody ∷
  (MonadThrow m, FromJSON α) ⇒ Response LBS.ByteString → SessionT m α
parseResponseBody =
  responseBody
  >>>
  eitherDecode'
  >>>
  either (InvalidJSON >>> throwM) pure

responseStatusCode = responseStatus >>> statusCode

data UnexpectedStatus = UnexpectedStatus [Int] Int deriving (Typeable)
instance Show UnexpectedStatus where
  show (UnexpectedStatus expected_codes status) =
    [i|Status code not one of #{expected_codes}: #{status}|]
instance Exception UnexpectedStatus where

sendRequest ∷ MonadIO m ⇒ Request → SessionT m (Response LBS.ByteString)
sendRequest request = httpLBS request

sendRequestForJSON ∷ (MonadIO m, FromJSON α) ⇒ Request → SessionT m (Response (Either JSONException α))
sendRequestForJSON request = httpJSONEither request

request ∷ MonadIO m ⇒ StdMethod → Text → SessionT m (Response LBS.ByteString)
request method path = makeRequest method path >>= sendRequest

requestForJSON ∷ (MonadIO m, FromJSON α) ⇒ StdMethod → Text → SessionT m (Response (Either JSONException α))
requestForJSON method path = makeRequest method path >>= sendRequestForJSON

logout ∷ MonadIO m ⇒ SessionT m ()
logout = void $ request POST "logout"

requestWithJSONForJSON ∷
  (MonadIO m, ToJSON α, FromJSON β) ⇒ StdMethod → Text → α → SessionT m (Response (Either JSONException β))
requestWithJSONForJSON method path value =
  (makeRequest method path <&> addJSONBody value)
  >>=
  sendRequestForJSON

data PutResult = ResourceCreated | ResourceReplaced deriving (Eq, Ord, Read, Show)

putResource ∷ (MonadIO m, MonadThrow m, ToJSON α) ⇒ (UUID → Text) → UUID → α → SessionT m PutResult
putResource pathToResource resource_id resource = do
  response ←
    makeRequest PUT (pathToResource resource_id)
    >>=
    (setRequestBodyJSON resource >>> sendRequest)
  case responseStatusCode response of
    201 → pure ResourceCreated
    204 → pure ResourceReplaced
    code → throwM $ UnexpectedStatus [201,204] code

putHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → Habit → SessionT m PutResult
putHabit = putResource pathToHabit

putGroup ∷ (MonadIO m, MonadThrow m) ⇒ UUID → Group → SessionT m PutResult
putGroup = putResource pathToGroup

data DeleteResult = HabitDeleted | NoHabitToDelete deriving (Eq, Ord, Read, Show)

deleteHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → SessionT m DeleteResult
deleteHabit habit_id = do
  response ← request DELETE $ pathToHabit habit_id
  case responseStatusCode response of
    204 → pure HabitDeleted
    404 → pure NoHabitToDelete
    code → throwM $ UnexpectedStatus [204,404] code

getResource ∷ (MonadIO m, MonadThrow m, FromJSON α) ⇒ (UUID → Text) → UUID → SessionT m (Maybe α)
getResource pathToResource resource_id = do
  response ← requestForJSON GET $ pathToResource resource_id
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    404 → pure Nothing
    code → throwM $ UnexpectedStatus [200,404] code

getHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → SessionT m (Maybe Habit)
getHabit = getResource pathToHabit

getGroup ∷ (MonadIO m, MonadThrow m) ⇒ UUID → SessionT m (Maybe Group)
getGroup = getResource pathToGroup

getResources ∷ (MonadIO m, MonadThrow m, FromJSON α) ⇒ Text → SessionT m (ItemsSequence α)
getResources path = do
  response ← requestForJSON GET path
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    code → throwM $ UnexpectedStatus [200] code

getHabits ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m (ItemsSequence Habit)
getHabits = getResources "habits"

getGroups ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m (ItemsSequence Group)
getGroups = getResources "groups"

getMarks ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m (Tagged [Scale])
getMarks = do
  response ← requestForJSON GET "marks"
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    code → throwM $ UnexpectedStatus [200] code

getDeadlines ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m [(UUID, [LocalTime])]
getDeadlines = do
  response ← requestForJSON GET "deadlines"
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    code → throwM $ UnexpectedStatus [200] code

getConfiguration ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m Configuration
getConfiguration = do
  response ← requestForJSON GET "configuration"
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    code → throwM $ UnexpectedStatus [200] code

putConfiguration ∷ (MonadIO m, MonadThrow m) ⇒ Configuration → SessionT m ()
putConfiguration configuration = do
  response ←
    makeRequest PUT "configuration"
    >>=
    (setRequestBodyJSON configuration >>> sendRequest)
  case responseStatusCode response of
    204 → pure ()
    code → throwM $ UnexpectedStatus [204] code

resetConfiguration ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m ()
resetConfiguration = do
  response ← makeRequest DELETE "configuration" >>= sendRequest
  case responseStatusCode response of
    204 → pure ()
    code → throwM $ UnexpectedStatus [204] code

markHabits ∷ (MonadIO m, MonadThrow m) ⇒ [(UUID, Tagged Int)] → SessionT m (Tagged [Scale])
markHabits marks = do
  response ←
    requestWithJSONForJSON
      POST
      "marks"
      marks
  case responseStatusCode response of
    200 → either throwM pure $ responseBody response
    code → throwM $ UnexpectedStatus [200] code

getQuestStatus ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m Lazy.Text
getQuestStatus = do
  response ← request GET "status"
  case responseStatusCode response of
    200 → response |> responseBody |> decodeUtf8 |> pure
    code → throwM $ UnexpectedStatus [200] code

runGame ∷ (MonadIO m, MonadThrow m) ⇒ SessionT m Lazy.Text
runGame = do
  response ← request POST "run"
  case responseStatusCode response of
    200 → response |> responseBody |> decodeUtf8 |> pure
    code → throwM $ UnexpectedStatus [200] code
