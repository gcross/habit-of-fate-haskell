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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Server
  ( Username(..)
  , makeApp
  , makeAppRunningInTestMode
  ) where

import HabitOfFate.Prelude hiding (div, id, log)

import Data.Aeson hiding ((.=))
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.Operational (Program, interpretWithMonad)
import qualified Control.Monad.Operational as Operational
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Set (minView)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock
import Data.UUID hiding (null)
import GHC.Conc.Sync (unsafeIOToSTM)
import Network.HTTP.Types.Status
import Network.Wai
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius (cassius, renderCss)
import Text.Hamlet (hamlet)
import Web.Cookie
import Web.Scotty
  ( ActionM
  , Param
  , Parsable
  , addHeader
  , params
  , parseParam
  , finish
  , rescue
  , scottyApp
  , status
  )
import qualified Web.Scotty as Scotty

import HabitOfFate.Account hiding (_habits)
import HabitOfFate.Credits
import HabitOfFate.Game
import HabitOfFate.Habit
import HabitOfFate.Logging
import HabitOfFate.Story

import Paths_habit_of_fate (getDataFileName)

--------------------------------------------------------------------------------
-------------------------------- Miscellaneous ---------------------------------
--------------------------------------------------------------------------------

instance Parsable UUID where
  parseParam = view strict >>> fromText >>> maybe (Left "badly formed UUID") Right

newtype Username = Username { unwrapUsername ∷ Text } deriving
  ( Eq
  , FromJSONKey
  , Ord
  , Parsable
  , Read
  , Show
  , ToJSONKey
  )

instance FromJSON Username where
  parseJSON = parseJSON >>> fmap Username

instance ToJSON Username where
  toJSON = unwrapUsername >>> toJSON
  toEncoding = unwrapUsername >>> toEncoding

newtype Cookie = Cookie Text deriving (Eq,FromJSON,Ord,Parsable,Read,Show,ToJSON)

data Environment = Environment
  { accounts_tvar ∷ TVar (Map Username (TVar Account))
  , cookies_tvar ∷ TVar (Map Cookie (UTCTime, Username))
  , expirations_tvar ∷ TVar (Set (UTCTime, Cookie))
  , write_request_var ∷ TMVar ()
  }

readTVarMonadIO ∷ MonadIO m ⇒ TVar α → m α
readTVarMonadIO = readTVarIO >>> liftIO

scottyHTML = ($ renderPageURL) >>> renderHtml >>> decodeUtf8 >>> Scotty.html
scottyCSS = ($ renderPageURL) >>> renderCss >>> Scotty.text

--------------------------------------------------------------------------------
---------------------------- Shared Scotty Actions -----------------------------
--------------------------------------------------------------------------------

----------------------------------- Queries ------------------------------------

lookupHabit ∷ (ActionMonad m, MonadState Account m) ⇒ UUID → m Habit
lookupHabit habit_id = do
  use (habits . at habit_id)
  >>=
  maybe raiseNoSuchHabit return

param ∷ Parsable α ⇒ Lazy.Text → ActionM α
param name =
  Scotty.param name
  `rescue`
  (const
   $
   do Scotty.text $ "Missing parameter: \"" ⊕ name ⊕ "\""
      status badRequest400
      finish
  )

paramOrBlank ∷ Lazy.Text → ActionM Text
paramOrBlank name = Scotty.param name `rescue` ("" |> pure |> const)

bodyJSON ∷ FromJSON α ⇒ ActionM α
bodyJSON = do
  body_ ← Scotty.body
  case eitherDecode' body_ of
    Left message → do
      logIO [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body_}|]
      finishWithStatusMessage 400 "Bad request: Invalid JSON"
    Right value → pure value

authorizeWith ∷ (∀ α. String → ActionM α) → Environment → ActionM (Username, TVar Account)
authorizeWith actionWhenAuthFails Environment{..} = do
  let handleForbidden ∷ String → Maybe α → ActionM α
      handleForbidden message = maybe (actionWhenAuthFails message) pure
  cookie_header ← Scotty.header "Cookie" >>= handleForbidden "No cookie header."
  cookie ∷ Cookie ←
    cookie_header
      |> view strict
      |> encodeUtf8
      |> parseCookiesText
      |> lookup "token"
      |> handleForbidden "No authorization token in the cookies."
      |> fmap Cookie
  current_time ← liftIO getCurrentTime
  (liftIO <<< atomically <<< runExceptT $ do
    cookies ← lift $ readTVar cookies_tvar
    (expiration_time, username) ←
      cookies
        |> lookup cookie
        |> maybe (throwError "Authorization token unrecognized.") pure
    (expiration_time, cookie)
        |> deleteSet
        |> modifyTVar expirations_tvar
        |> lift
    when (expiration_time < current_time) $ throwError "Authorization token expired."
    accounts ← lift $ readTVar accounts_tvar
    account_tvar ←
      accounts
      |> lookup username
      |> maybe
          (do lift $ modifyTVar cookies_tvar (deleteMap cookie)
              throwError "Token no longer refers to an existing user."
          )
          pure
    lift $ do
      let new_expected_time = addUTCTime (30*86400) current_time
      modifyTVar cookies_tvar $ insertMap cookie (new_expected_time, username)
      modifyTVar expirations_tvar $ insertSet (new_expected_time, cookie)
    pure (username, account_tvar)
   ) >>= either actionWhenAuthFails pure

----------------------------------- Results ------------------------------------

finishWithStatus ∷ Status → ActionM α
finishWithStatus s = do
  logIO $ "Finished with status: " ⊕ show s
  status s
  finish

finishWithStatusMessage ∷ Int → String → ActionM α
finishWithStatusMessage code = pack >>> encodeUtf8 >>> Status code >>> finishWithStatus

valueOrRedirectToLogin ∷ Maybe α → ActionM α
valueOrRedirectToLogin = maybe (Scotty.redirect "/login") pure

setContent ∷ Content → ActionM ()
setContent NoContent = pure ()
setContent (TextContent t) = Scotty.text t
setContent (HtmlContent h) = h |> toHtml |> renderHtml |> decodeUtf8 |> Scotty.html
setContent (JSONContent j) = Scotty.json j

setStatusAndLog ∷ Status → ActionM ()
setStatusAndLog status_@(Status code message) = do
  status status_
  let result
        | code < 200 || code >= 300 = "failed"
        | otherwise = "succeeded"
  logIO $ [i|Request #{result} - #{code} #{decodeUtf8 >>> unpack $ message}|]

data ProgramResult = ProgramResult Status Content

data Content =
    NoContent
  | TextContent Lazy.Text
  | HtmlContent Html
  | ∀ α. ToJSON α ⇒ JSONContent α

returnNothing ∷ Monad m ⇒ Status → m ProgramResult
returnNothing s = return $ ProgramResult s NoContent

returnLazyText ∷ Monad m ⇒ Status → Lazy.Text → m ProgramResult
returnLazyText s = TextContent >>> ProgramResult s >>> return

returnText ∷ Monad m ⇒ Status → Text → m ProgramResult
returnText s = view (from strict) >>> returnLazyText s

returnJSON ∷ (ToJSON α, Monad m) ⇒ Status → α → m ProgramResult
returnJSON s = JSONContent >>> ProgramResult s >>> return

data Page = Habits
renderPageURL ∷ Page → Text
renderPageURL Habits = "/habits"

returnHTML ∷ Monad m ⇒ Status → ((Page → Text) → Html) → m ProgramResult
returnHTML s = ($ renderPageURL) >>> HtmlContent >>> ProgramResult s >>> return

logRequest ∷ ActionM ()
logRequest = do
  r ← Scotty.request
  logIO $ [i|URL requested: #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]

--------------------------------------------------------------------------------
--------------------------------- Action Monad ---------------------------------
--------------------------------------------------------------------------------

------------------------------------ Common ------------------------------------

data CommonInstruction α where
  GetBodyInstruction ∷ CommonInstruction Lazy.ByteString
  GetParamsInstruction ∷ CommonInstruction [Param]
  RaiseStatusInstruction ∷ Status → CommonInstruction α
  LogInstruction ∷ String → CommonInstruction ()

class Monad m ⇒ ActionMonad m where
  singletonCommon ∷ CommonInstruction α → m α

getBody ∷ ActionMonad m ⇒ m Lazy.ByteString
getBody = singletonCommon GetBodyInstruction

getBodyJSON ∷ (FromJSON α, ActionMonad m) ⇒ m α
getBodyJSON = do
  body ← getBody
  case eitherDecode' $ body of
    Left message → do
      log [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body}|]
      raiseStatus 400 "Bad request: Invalid JSON"
    Right json → pure json

getParams ∷ ActionMonad m ⇒ m [Param]
getParams = singletonCommon GetParamsInstruction

getParam ∷ (Parsable α, ActionMonad m) ⇒ Lazy.Text → m α
getParam param_name = do
  params_ ← getParams
  case lookup param_name params_ of
    Nothing → raiseStatus 400 [i|Bad request: Missing parameter %{param_name}|]
    Just value → case parseParam value of
      Left _ →
        raiseStatus
          400
          [i|Bad request: Parameter #{param_name} has invalid format #{value}|]
      Right x → return x

getParamMaybe ∷ (Parsable α, ActionMonad m) ⇒ Lazy.Text → m (Maybe α)
getParamMaybe param_name =
  getParams
  <&>
  (lookup param_name >=> (parseParam >>> either (const Nothing) return))

paramMaybe ∷ Parsable α ⇒ Lazy.Text → ActionM (Maybe α)
paramMaybe param_name = (param param_name <&> Just) `rescue` (const $ pure Nothing)

raiseStatus ∷ ActionMonad m ⇒ Int → String → m α
raiseStatus code =
  pack
  >>>
  encodeUtf8
  >>>
  Status code
  >>>
  RaiseStatusInstruction
  >>>
  singletonCommon

raiseNoSuchHabit = raiseStatus 404 "Not found: No such habit"

log ∷ ActionMonad m ⇒ String → m ()
log = LogInstruction >>> singletonCommon 

------------------------------------ Reader ------------------------------------

data ReaderInstruction α where
  ReaderCommonInstruction ∷ CommonInstruction α → ReaderInstruction α
  ReaderViewInstruction ∷ ReaderInstruction Account

newtype ReaderProgram α = ReaderProgram
  { unwrapReaderProgram ∷ Program ReaderInstruction α }
  deriving (Applicative, Functor, Monad)

instance ActionMonad ReaderProgram where
  singletonCommon = ReaderCommonInstruction >>> Operational.singleton >>> ReaderProgram

instance MonadReader Account (ReaderProgram) where
  ask = ReaderProgram $ Operational.singleton ReaderViewInstruction
  local = error "if you see this, then ReaderProgram needs to have a local method"

readerWith ∷ (∀ α. String → ActionM α) → Environment → ReaderProgram ProgramResult → ActionM ()
readerWith actionWhenAuthFails environment (ReaderProgram program) = do
  logRequest
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  params_ ← params
  body_ ← Scotty.body
  account ← account_tvar |> readTVarMonadIO
  let interpret ∷ ReaderInstruction α → ExceptT Status (Writer (Seq String)) α
      interpret (ReaderCommonInstruction GetBodyInstruction) = pure body_
      interpret (ReaderCommonInstruction GetParamsInstruction) = pure params_
      interpret (ReaderCommonInstruction (RaiseStatusInstruction s)) = throwError s
      interpret (ReaderCommonInstruction (LogInstruction message)) =
        void ([i|[#{username}]: #{message}|] |> singleton |> tell)
      interpret ReaderViewInstruction = pure account
      (error_or_result, logs) =
        program
          |> interpretWithMonad interpret
          |> runExceptT
          |> runWriter
  traverse_ logIO logs
  case error_or_result of
    Left status_ →
      setStatusAndLog status_
    Right (ProgramResult status_ content) → do
      setStatusAndLog status_
      setContent content

------------------------------------ Writer ------------------------------------

data WriterInstruction α where
  WriterCommonInstruction ∷ CommonInstruction α → WriterInstruction α
  WriterGetAccountInstruction ∷ WriterInstruction Account
  WriterPutAccountInstruction ∷ Account → WriterInstruction ()

newtype WriterProgram α = WriterProgram
  { unwrapWriterProgram ∷ Program WriterInstruction α }
  deriving (Applicative, Functor, Monad)

instance ActionMonad WriterProgram where
  singletonCommon = WriterCommonInstruction >>> Operational.singleton >>> WriterProgram

instance MonadState Account WriterProgram where
  get = Operational.singleton WriterGetAccountInstruction |> WriterProgram
  put = WriterPutAccountInstruction >>> Operational.singleton >>> WriterProgram

writerWith ∷ (∀ α. String → ActionM α) → Environment → WriterProgram ProgramResult → ActionM ()
writerWith actionWhenAuthFails (environment@Environment{..}) (WriterProgram program) = do
  logRequest
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  params_ ← params
  body_ ← Scotty.body
  let interpret ∷
        WriterInstruction α →
        StateT Account (ExceptT Status (RandT StdGen (Writer (Seq String)))) α
      interpret (WriterCommonInstruction GetBodyInstruction) = pure body_
      interpret (WriterCommonInstruction GetParamsInstruction) = pure params_
      interpret (WriterCommonInstruction (RaiseStatusInstruction s)) = throwError s
      interpret (WriterCommonInstruction (LogInstruction message)) =
        void ([i|[#{username}]: #{message}|] |> singleton |> tell)
      interpret WriterGetAccountInstruction = get
      interpret (WriterPutAccountInstruction new_account) = put new_account
  initial_generator ← liftIO newStdGen
  (status_, maybe_content, logs) ← atomically >>> liftIO $ do
    old_account ← readTVar account_tvar
    let (error_or_result, logs) =
          interpretWithMonad interpret program
            |> flip runStateT old_account
            |> runExceptT
            |> flip evalRandT initial_generator
            |> runWriter
    case error_or_result of
      Left status_ → pure (status_, Nothing, logs)
      Right (ProgramResult status_ content, new_account) → do
        writeTVar account_tvar new_account
        tryPutTMVar write_request_var ()
        pure (status_, Just content, logs)
  traverse_ logIO logs
  setStatusAndLog status_
  maybe (pure ()) setContent maybe_content

--------------------------------------------------------------------------------
------------------------------ Server Application ------------------------------
--------------------------------------------------------------------------------

data RunGameState = RunGameState
  { _run_quests ∷ Seq Quest
  , _run_quest_events ∷ Seq Event
  }
makeLenses ''RunGameState

makeAppWithTestMode ∷
  Bool →
  Map Username Account →
  (Map Username Account → IO ()) →
  IO Application
makeAppWithTestMode test_mode initial_accounts saveAccounts = do
  liftIO $ hSetBuffering stderr LineBuffering

  logIO $ "Starting server..."

  accounts_tvar ← atomically $
    traverse newTVar initial_accounts >>= newTVar

  cookies_tvar ← newTVarIO mempty
  expirations_tvar ← newTVarIO mempty

  forkIO <<< forever $
    let go =
          (
            atomically $ do
              current_time ← unsafeIOToSTM getCurrentTime
              expirations ← readTVar expirations_tvar
              case minView expirations of
                Nothing → pure False
                Just (first@(first_time, first_cookie), rest) → do
                  if first_time < current_time
                    then do
                      unsafeIOToSTM $ logIO [i|Dropping cookie #{first} at #{current_time}.|]
                      modifyTVar cookies_tvar $ deleteMap first_cookie
                      writeTVar expirations_tvar rest
                      pure True
                    else
                      pure False
          )
          >>=
          \case { False → threadDelay (60 * 1000 * 1000) >> go; _ → go }
    in go

  let createAndReturnCookie ∷ Username → ActionM ()
      createAndReturnCookie username = do
        Cookie token ← liftIO $ do
          current_time ← getCurrentTime
          cookie ← (pack >>> Cookie) <$> (replicateM 20 $ randomRIO ('A','z'))
          atomically $ do
            let expiration_time = addUTCTime (30*86400) current_time
            modifyTVar cookies_tvar $ insertMap cookie (expiration_time, username)
            modifyTVar expirations_tvar $ insertSet (expiration_time, cookie)
          pure cookie
        def
          { setCookieName="token"
          , setCookieValue=encodeUtf8 token
          , setCookieHttpOnly=True
          , setCookieSameSite=Just sameSiteStrict
          , setCookieSecure=not test_mode
          }
          |> renderSetCookie
          |> Builder.toLazyByteString
          |> decodeUtf8
          |> Scotty.setHeader "Set-Cookie"

  write_request_var ← newEmptyTMVarIO
  forever >>> forkIO $
    (atomically $ do
      takeTMVar write_request_var
      readTVar accounts_tvar >>= traverse readTVar
    )
    >>=
    saveAccounts

  let environment = Environment{..}
      apiReader = readerWith (finishWithStatusMessage 403) environment
      apiWriter = writerWith (finishWithStatusMessage 403) environment
      wwwReader = readerWith (const $ Scotty.redirect "/login") environment
      wwwWriter = writerWith (const $ Scotty.redirect "/login") environment

  scottyApp $ do
-------------------------------- Create Account --------------------------------
    Scotty.post "/api/create" $ do
      logRequest
      username ← param "username"
      password ← param "password"
      logIO $ [i|Request to create an account for "#{username}".|]
      liftIO >>> join $ do
        new_account ← newAccount password
        atomically $ do
          accounts ← readTVar accounts_tvar
          if member username accounts
            then pure $ do
              logIO $ [i|Account "#{username}" already exists!|]
              status conflict409
            else do
              account_tvar ← newTVar new_account
              modifyTVar accounts_tvar $ insertMap username account_tvar
              pure $ do
                logIO $ [i|Account "#{username}" successfully created!|]
                status created201
                createAndReturnCookie username
    let createAccountAction = do
          logRequest
          username@(Username username_) ← Username <$> paramOrBlank "username"
          password1 ← paramOrBlank "password1"
          password2 ← paramOrBlank "password2"
          error_message ∷ Text ←
            if ((not . onull $ password1) && password1 == password2)
              then do
                logIO $ [i|Request to create an account for "#{username_}".|]
                liftIO >>> join $ do
                  new_account ← newAccount password1
                  atomically $ do
                    accounts ← readTVar accounts_tvar
                    if member username accounts
                      then pure $ do
                        logIO $ [i|Account "#{username_}" already exists!|]
                        status conflict409
                        pure "This account already exists."
                      else do
                        account_tvar ← newTVar new_account
                        modifyTVar accounts_tvar $ insertMap username account_tvar
                        pure $ do
                          logIO $ [i|Account "#{username_}" successfully created!|]
                          createAndReturnCookie username
                          Scotty.redirect "/"
              else pure $
                if onull username_
                  then
                    if onull password1
                      then ""
                      else "Did not specify username."
                  else
                    case (password1, password2) of
                      ("", "") → "Did not type the password."
                      ("", _) → "Did not type the password twice."
                      (_, "") → "Did not type the password twice."
                      _ | password1 == password2 → ""
                      _ | otherwise → "The passwords did not agree."
          scottyHTML [hamlet|
<head>
  <title>Habit of Fate - Account Creation
  <link rel="stylesheet" type="text/css" href="css/enter.css"/>
<body>
  <div class="enter">
    <div class="tabs">
      <span class="inactive"><a href="/login">Login</a>
      <span class="active"> Create
    <form method="post">
      <div>
        <div class="fields"> <input type="text" name="username" value="#{username_}" placeholder="Username">
        <div class="fields"> <input type="password" name="password1" placeholder="Password">
        <div class="fields"> <input type="password" name="password2" placeholder="Password (again)">
      $if (not . onull) error_message
        <div id="error-message">#{error_message}
      <div>
        <input class="submit" type="submit" formmethod="post" value="Create Account">
|]
    Scotty.get "/create" createAccountAction
    Scotty.post "/create" createAccountAction
------------------------------------ Login -------------------------------------
    Scotty.post "/api/login" $ do
      logRequest
      username ← param "username"
      password ← param "password"
      logIO $ [i|Request to log into an account with "#{username}".|]
      account_tvar ←
        (accounts_tvar |> readTVarMonadIO |> fmap (lookup username))
        >>=
        maybe (finishWithStatusMessage 404 "Not Found: No such account") return
      (
        readTVarMonadIO account_tvar
        >>=
        (
          passwordIsValid password
          >>>
          bool (finishWithStatusMessage 403 "Forbidden: Invalid password") (logIO "Login successful.")
        )
        >>
        createAndReturnCookie username
       )
    let loginAction = do
          logRequest
          username@(Username username_) ← Username <$> paramOrBlank "username"
          password ← paramOrBlank "password"
          error_message ∷ Text ←
            if onull username_
              then pure ""
              else do
                logIO [i|Request to log in "#{username_}".|]
                accounts ← readTVarMonadIO accounts_tvar
                case lookup username accounts of
                  Nothing → do
                    logIO [i|No account has username #{username_}.|]
                    pure "No account has that username."
                  Just account_tvar → do
                    account ← readTVarMonadIO account_tvar
                    if passwordIsValid password account
                      then do
                        logIO [i|Successfully logged in #{username_}.|]
                        createAndReturnCookie username
                        Scotty.redirect "/habits"
                      else do
                        logIO [i|Incorrect password for #{username_}.|]
                        pure "No account has that username."
          scottyHTML [hamlet|
<head>
  <title>Habit of Fate - Login
  <link rel="stylesheet" type="text/css" href="css/enter.css"/>
<body>
  <div class="enter">
    <div class="tabs">
      <span class="active"> Login
      <span class="inactive"><a href="/create">Create</a>
    <form method="post">
      <div>
        <div class="fields"> <input type="text" name="username" value="#{username_}" placeholder="Username">
        <div class="fields"> <input type="password" name="password" placeholder="Password">
      $if (not . onull) error_message
        <div id="error-message">#{error_message}
      <div>
        <input class="submit" type="submit" formmethod="post" value="Login">
|]
    Scotty.get "/login" loginAction
    Scotty.post "/login" loginAction
------------------------------------ Logout ------------------------------------
    let logoutAction = do
          logRequest
          maybe_cookie_header ← Scotty.header "Cookie"
          case maybe_cookie_header of
            Nothing → pure ()
            Just cookie_header →
              let maybe_cookie =
                    cookie_header
                      |> view strict
                      |> encodeUtf8
                      |> parseCookiesText
                      |> lookup "token"
                      |> fmap Cookie
              in case maybe_cookie of
                Nothing → pure ()
                Just cookie → (liftIO <<< atomically) $ do
                  cookies ← readTVar cookies_tvar
                  case lookup cookie cookies of
                    Nothing → pure ()
                    Just (expiration_time, _) →
                      modifyTVar expirations_tvar $ deleteSet (expiration_time, cookie)
                  writeTVar cookies_tvar $ deleteMap cookie cookies
    Scotty.post "/api/logout" logoutAction
    Scotty.post "/logout" $ logoutAction >> Scotty.redirect "/login"
-------------------------------- Get All Habits --------------------------------
    Scotty.get "/api/habits" <<< apiReader $ do
      log "Requested all habits."
      view habits >>= returnJSON ok200
    Scotty.get "/habits" <<< wwwReader $ do
      habits_ ← view habits
      returnHTML ok200 [hamlet|
<head>
  <title>Habit of Fate - List of Habits
<body>
  <table>
    $forall (uuid, habit) <- mapToList habits_
      <tr>
        <td> <a href="/habits/#{show uuid}">#{habit ^. name}
|]
---------------------------------- Get Habit -----------------------------------
    let habitPage ∷ Monad m ⇒ UUID → Lazy.Text → Lazy.Text → Lazy.Text → Habit → m ProgramResult
        habitPage habit_id name_error difficulty_error importance_error habit = returnHTML ok200 [hamlet|
<head>
  <title>Editing a habit
<body>
  <form method="post">
    <div>
      <table>
        <tr>
          <td> Name:
          <td>
            <input type="text" name="name" value="#{habit ^. name}"/>
          <td> #{name_error}
        <tr>
          <td> Difficulty:
          <td>
            <select>
              $forall scale <- scales
                <option value="#{scale}"> #{displayScale scale} Difficulty
          <td> #{difficulty_error}
        <tr>
          <td> Importance:
          <td>
            <select>
              $forall scale <- scales
                <option value="#{scale}"> #{displayScale scale} Importance
          <td> #{importance_error}
    <div>
      <input type="submit"/> Submit
      <button onclick="window.location.href='/habits'"> Cancel
|]
    Scotty.get "/api/habits/:habit_id" <<< apiReader $ do
      habit_id ← getParam "habit_id"
      log $ [i|Requested habit with id #{habit_id}.|]
      (view habits <&> lookup habit_id)
        >>= maybe raiseNoSuchHabit (returnJSON ok200)

    Scotty.get "/habits/:habit_id" <<< wwwReader $ do
      habit_id ← getParam "habit_id"
      log $ [i|Web GET request for habit with id #{habit_id}.|]
      (view habits <&> (lookup habit_id >>> fromMaybe def))
        >>= habitPage habit_id "" "" ""

    Scotty.post "/habits/:habit_id" <<< wwwWriter $ do
      habit_id ← getParam "habit_id"
      log $ [i|Web POST request for habit with id #{habit_id}.|]
      (unparsed_name, maybe_name, name_error) ← getParamMaybe "name" <&> \case
            Nothing → ("", Nothing, "No value for the name was present.")
            Just unparsed_name
              | onull unparsed_name → (unparsed_name, Nothing, "Name must not be blank.")
              | otherwise → (unparsed_name, Just (pack unparsed_name), "")
      let getScale param_name = getParamMaybe param_name <&> \case
            Nothing → ("", Nothing, "No value for the " ⊕ param_name ⊕ " was present.")
            Just unparsed_value →
              case readMaybe unparsed_value of
                Nothing → (unparsed_name, Nothing, "Invalid value for the " ⊕ param_name ⊕ ".")
                Just value → (unparsed_value, Just value, "")
      (unparsed_difficulty, maybe_difficulty, difficulty_error) ← getScale "difficulty"
      (unparsed_importance, maybe_importance, importance_error) ← getScale "importance"
      case Habit <$> maybe_name <*> (Difficulty <$> maybe_difficulty) <*> (Importance <$> maybe_importance) of
        Nothing → habitPage habit_id name_error difficulty_error importance_error def
        Just new_habit → habits . at habit_id <<.= Nothing >> returnNothing noContent204
-------------------------------- Delete Habit ---------------------------------
    Scotty.delete "/api/habits/:habit_id" <<< apiWriter $ do
      habit_id ← getParam "habit_id"
      log $ [i|Requested to delete habit with id #{habit_id}.|]
      habit_was_there ← isJust <$> (habits . at habit_id <<.= Nothing)
      returnNothing $
        if habit_was_there
          then noContent204
          else notFound404
---------------------------------- Put Habit -----------------------------------
    let apiWriteAction = do
          habit_id ← getParam "habit_id"
          log $ [i|Requested to put habit with id #{habit_id}.|]
          habit ← getBodyJSON
          habit_was_there ← isJust <$> (habits . at habit_id <<.= Just habit)
          returnNothing $
            if habit_was_there
              then noContent204
              else created201
    Scotty.post "/api/habits/:habit_id" <<< apiWriter $ apiWriteAction
    Scotty.put "/api/habits/:habit_id" <<< apiWriter $ apiWriteAction
--------------------------------- Get Credits ----------------------------------
    Scotty.get "/api/credits" <<< apiReader $ do
      log $ "Requested credits."
      view (game . credits) >>= returnJSON ok200
--------------------------------- Mark Habits ----------------------------------
    Scotty.post "/api/mark" <<< apiWriter $ do
      marks ← getBodyJSON
      let markHabits ∷
            (HabitsToMark → [UUID]) →
            (Habit → Scale) →
            Lens' Account Double →
            WriterProgram Double
          markHabits getUUIDs getScale value_lens = do
            old_value ← use value_lens
            increment ∷ Double ←
              marks
                |> getUUIDs
                |> mapM (lookupHabit >>> fmap (getScale >>> scaleFactor))
                |> fmap sum
            value_lens <.= old_value + increment
      log $ [i|Marking #{marks ^. successes} successes and #{marks ^. failures} failures.|]
      (Credits
          <$> (markHabits (^. successes) (^. difficulty) (game . credits . success))
          <*> (markHabits (^. failures ) (^. importance) (game . credits . failure))
       ) >>= returnJSON ok200
----------------------------------- Run Game -----------------------------------
    Scotty.post "/api/run" <<< apiWriter $ do
      let go d = do
            let r = runAccount d
            run_quest_events %= (⊢ r ^. story . to createEvent)
            if stillHasCredits (r ^. new_data)
              then do
                when (r ^. quest_completed) $
                  (run_quest_events <<.= mempty)
                  >>=
                  (createQuest >>> (⊣) >>> (run_quests %=))
                go (r ^. new_data)
              else return (r ^. new_data)
      (new_d, s) ←
        get
        >>=
        (
          go
          >>>
          flip runStateT (RunGameState
            { _run_quests = mempty
            , _run_quest_events = mempty
            })
        )
      put new_d
      returnLazyText ok200 $!! (
        s ^. run_quests ⊢ s ^. run_quest_events . to createQuest
        |> createStory
        |> renderStoryToText
       )
------------------------------------- Home -------------------------------------
    Scotty.get "/home" <<< scottyHTML $ [hamlet|
<head>
  <title>Habit of Fate
<body>
  <a href="/login">Login
  <a href="/create">Create
|]
------------------------------------- Root -------------------------------------
    Scotty.get "/" $ Scotty.redirect "/habits"
--------------------------------- Style Sheets ---------------------------------
    Scotty.get "/css/enter.css" $ do
      addHeader "Content-Type" "text/css"
      scottyCSS [cassius|
body
  background: #a2aeff
  font-family: Arial

.enter
  display: flex
  flex-direction: column
  justify-content: center;
  padding: 8% 0 0
  margin: auto
  width: 600px

.tabs
  bottom: 10px
  color: white
  display: flex
  flex-direction: row
  flex-wrap: nowrap
  margin-bottom: -10px
  margin-right: 20px
  position: relative

.active
  background: #728fff
  padding: 10px

.inactive
  background: #476bff
  padding: 10px

  a
    color: white

span.inactive:hover
    background: #5d7dff

form
  background: #728fff
  color: white
  font-family: Arial
  font-size: 20
  padding: 10px

  .fields
    display: flex
    flex-direction: column
    padding-bottom: 10px

    input
      background: #c3d0ff
      border: 0
      font-family: Arial
      font-size: 20
      padding: 5px

  #error-message
    color: #9b0000
    padding-bottom: 10px
|]
---------------------------------- Not Found -----------------------------------
    Scotty.notFound $ do
      r ← Scotty.request
      logIO [i|URL not found! #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]
      Scotty.next

makeApp ∷
  Map Username Account →
  (Map Username Account → IO ()) →
  IO Application
makeApp = makeAppWithTestMode False

makeAppRunningInTestMode ∷
  Map Username Account →
  (Map Username Account → IO ()) →
  IO Application
makeAppRunningInTestMode = makeAppWithTestMode True
