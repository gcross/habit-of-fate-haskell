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

module HabitOfFate.Client where

import HabitOfFate.Prelude

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
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

type InnerClientAction = ReaderT SessionInfo

newtype ClientT m α = ClientT { unwrapClientT ∷ InnerClientAction m α }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadThrow
    , MonadTrans
    )

type ClientIO = ClientT IO

runClientT ∷ ClientT m α → SessionInfo → m α
runClientT = runReaderT ∘ unwrapClientT

instance MonadBase IO m ⇒ MonadBase IO (ClientT m) where
  liftBase = ClientT ∘ liftBase

instance MonadBaseControl IO ClientIO where
  type StM ClientIO α = StM (InnerClientAction IO) α
  liftBaseWith f = ClientT $ liftBaseWith $ \r → f (r ∘ unwrapClientT)
  restoreM = ClientT ∘ restoreM

getManager ∷ Monad m ⇒ ClientT m Manager
getManager = ClientT $ view manager

getRequestTemplate ∷ Monad m ⇒ ClientT m Request
getRequestTemplate = ClientT $ view request_template

decodeUtf8InResponse ∷ Response LBS.ByteString → Text
decodeUtf8InResponse = decodeUtf8 ∘ LBS.toStrict ∘ responseBody

pathToHabit ∷ UUID → Text
pathToHabit = ("habits/" ⊕) ∘ UUID.toText

makeRequest ∷ Monad m ⇒ StdMethod → Text → ClientT m Request
makeRequest std_method path = do
  getRequestTemplate
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

parseResponseBody ∷
  (MonadThrow m, FromJSON α) ⇒ Response LBS.ByteString → ClientT m α
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

sendRequest ∷ MonadIO m ⇒ Request → ClientT m (Response LBS.ByteString)
sendRequest request = getManager >>= liftIO ∘ httpLbs request

request ∷ MonadIO m ⇒ StdMethod → Text → ClientT m (Response LBS.ByteString)
request method path = makeRequest method path >>= sendRequest

requestWithJSON ∷
  (MonadIO m, ToJSON α) ⇒ StdMethod → Text → α → ClientT m (Response LBS.ByteString)
requestWithJSON method path value =
  (makeRequest method path <&> addJSONBody value)
  >>=
  sendRequest

data PutResult = HabitCreated | HabitReplaced deriving (Eq, Ord, Read, Show)

putHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → Habit → ClientT m PutResult
putHabit habit_id habit = do
  response ← requestWithJSON PUT (pathToHabit habit_id) habit
  case responseStatusCode response of
    201 → pure HabitCreated
    204 → pure HabitReplaced
    code → throwM $ UnexpectedStatus [201,204] code

data DeleteResult = HabitDeleted | NoHabitToDelete deriving (Eq, Ord, Read, Show)

deleteHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → ClientT m DeleteResult
deleteHabit habit_id = do
  response ← request DELETE $ pathToHabit habit_id
  case responseStatusCode response of
    204 → pure HabitDeleted
    404 → pure NoHabitToDelete
    code → throwM $ UnexpectedStatus [204,404] code

getHabit ∷ (MonadIO m, MonadThrow m) ⇒ UUID → ClientT m (Maybe Habit)
getHabit habit_id = do
  response ← request GET $ pathToHabit habit_id
  case responseStatusCode response of
    200 → parseResponseBody response
    404 → pure Nothing
    code → throwM $ UnexpectedStatus [200,404] code

getHabits ∷ (MonadIO m, MonadThrow m) ⇒ ClientT m (Map UUID Habit)
getHabits = do
  response ← request GET "habits"
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

getCredits ∷ (MonadIO m, MonadThrow m) ⇒ ClientT m Credits
getCredits = do
  response ← request GET "credits"
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

markHabits ∷ (MonadIO m, MonadThrow m) ⇒ [UUID] → [UUID] → ClientT m Credits
markHabits success_habits failure_habits = do
  response ← requestWithJSON POST "mark" (HabitsToMark success_habits failure_habits)
  case responseStatusCode response of
    200 → parseResponseBody response
    code → throwM $ UnexpectedStatus [200] code

runGame ∷ (MonadIO m, MonadThrow m) ⇒ ClientT m Story
runGame = do
  response ← request POST "run"
  case responseStatusCode response of
    200 →
      (either throwM return ∘ parseText def ∘ decodeUtf8 ∘ responseBody $ response)
      >>=
      (either error return ∘ parseStoryFromDocument)
    code → throwM $ UnexpectedStatus [200] code
