{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Server (makeApp) where

import HabitOfFate.Prelude hiding (log)

import Data.Aeson hiding ((.=))
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad.Operational (Program, ProgramViewT(..))
import qualified Control.Monad.Operational as Operational
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy as Lazy
import Data.UUID
import Data.Yaml hiding (Parser, (.=))
import Network.HTTP.Types.Status
import Network.Wai
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr)
import Web.JWT hiding (decode, header)
import Web.Scotty
  ( ActionM
  , Param
  , Parsable
  , body
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
import HabitOfFate.Logging
import HabitOfFate.Story

instance Parsable UUID where
  parseParam = maybe (Left "badly formed UUID") Right ∘ fromText ∘ view strict

data CommonInstructionInstruction α where
  GetBodyInstruction ∷ CommonInstructionInstruction Lazy.ByteString
  GetParamsInstruction ∷ CommonInstructionInstruction [Param]
  RaiseStatusInstruction ∷ Status → CommonInstructionInstruction α
  LogInstruction ∷ String → CommonInstructionInstruction ()

class Monad m ⇒ ActionMonad m where
  singletonCommon ∷ CommonInstructionInstruction α → m α

finishWithStatus ∷ Status → ActionM α
finishWithStatus s = do
  logIO $ "Finished with status: " ⊕ show s
  status s
  finish

finishWithStatusMessage ∷ Int → String → ActionM α
finishWithStatusMessage code = finishWithStatus ∘ Status code ∘ encodeUtf8 ∘ pack

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

raiseStatus ∷ ActionMonad m ⇒ Int → String → m α
raiseStatus code =
  singletonCommon
  ∘
  RaiseStatusInstruction
  ∘
  Status code
  ∘
  encodeUtf8
  ∘
  pack

raiseNoSuchHabit = raiseStatus 404 "Not found: No such habit"

log ∷ ActionMonad m ⇒ String → m ()
log = singletonCommon ∘ LogInstruction

data ReaderInstruction α where
  ReaderCommonInstruction ∷ CommonInstructionInstruction α → ReaderInstruction α
  ReaderViewInstruction ∷ ReaderInstruction Account

newtype ReaderProgram α = ReaderProgram
  { unwrapReaderProgram ∷ Program ReaderInstruction α }
  deriving (Applicative, Functor, Monad)

instance ActionMonad ReaderProgram where
  singletonCommon = ReaderProgram ∘ Operational.singleton ∘ ReaderCommonInstruction

instance MonadReader Account (ReaderProgram) where
  ask = ReaderProgram $ Operational.singleton ReaderViewInstruction
  local = error "if you see this, then ReaderProgram needs to have a local method"

data WriterInstruction α where
  WriterCommonInstruction ∷ CommonInstructionInstruction α → WriterInstruction α
  WriterGetAccountInstruction ∷ WriterInstruction Account
  WriterPutAccountInstruction ∷ Account → WriterInstruction ()

newtype WriterProgram α = WriterProgram
  { unwrapWriterProgram ∷ Program WriterInstruction α }
  deriving (Applicative, Functor, Monad)

instance ActionMonad WriterProgram where
  singletonCommon = WriterProgram ∘ Operational.singleton ∘ WriterCommonInstruction

instance MonadState Account WriterProgram where
  get = WriterProgram $ Operational.singleton WriterGetAccountInstruction
  put = WriterProgram ∘ Operational.singleton ∘ WriterPutAccountInstruction

data ProgramResult = ProgramResult Status Content

data Content =
    NoContent
  | TextContent Lazy.Text
  | ∀ α. ToJSON α ⇒ JSONContent α

returnNothing ∷ Monad m ⇒ Status → m ProgramResult
returnNothing s = return $ ProgramResult s NoContent

returnLazyText ∷ Monad m ⇒ Status → Lazy.Text → m ProgramResult
returnLazyText s = return ∘ ProgramResult s ∘ TextContent

returnText ∷ Monad m ⇒ Status → Text → m ProgramResult
returnText s = returnLazyText s ∘ view (from strict)

returnJSON ∷ (ToJSON α, Monad m) ⇒ Status → α → m ProgramResult
returnJSON s = return ∘ ProgramResult s ∘ JSONContent

data AccountStatus = AccountExists | AccountCreated

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

setContent ∷ Content → ActionM ()
setContent NoContent = pure ()
setContent (TextContent t) = Scotty.text t
setContent (JSONContent j) = Scotty.json j

setStatusAndLog ∷ Status → ActionM ()
setStatusAndLog status_@(Status code message) = do
  status status_
  let result
        | code < 200 || code >= 300 = "failed"
        | otherwise = "succeeded"
  logIO $ [i|Request #{result} - #{code} #{unpack ∘ decodeUtf8 $ message}|]

bodyJSON ∷ FromJSON α ⇒ ActionM α
bodyJSON = do
  body_ ← body
  case eitherDecode' body_ of
    Left message → do
      logIO [i|Error parsing JSON for reason "#{message}#:\n#{decodeUtf8 body_}|]
      finishWithStatusMessage 400 "Bad request: Invalid JSON"
    Right value → pure value

makeApp ∷
  Secret →
  Map Text Account →
  (Map Text Account → IO ()) →
  IO Application
makeApp password_secret initial_accounts saveAccounts = do
  liftIO $ hSetBuffering stderr LineBuffering
  accounts_tvar ∷ TVar (Map Text (TVar Account)) ← atomically $
    traverse newTVar initial_accounts >>= newTVar
  write_request ← newEmptyTMVarIO
  liftIO ∘ forkIO ∘ forever $
    (atomically $ do
      takeTMVar write_request
      readTVar accounts_tvar >>= traverse readTVar
    )
    >>=
    saveAccounts
  logIO $ "Starting server..."
  let expected_iss = fromJust $ stringOrURI "habit-of-fate"

      authorize ∷ ActionM (String, TVar Account)
      authorize =
        Scotty.header "Authorization"
        >>=
        maybe
          (finishWithStatusMessage 403 "Forbidden: No authorization token.")
          return
        >>=
        \case
          (words → ["Bearer", token]) → return token
          header →
            finishWithStatusMessage
              403
              [i|Forbidden: Unrecognized authorization header: #{header}|]
        >>=
        maybe
          (finishWithStatusMessage 403 "Forbidden: Unable to verify key")
          return
        ∘
        decodeAndVerifySignature password_secret
        ∘
        view strict
        >>=
        (\case
          (claims → JWTClaimsSet { iss = Just observed_iss, sub = Just username })
            | observed_iss == expected_iss →
                ((fmap ∘ lookup ∘ pack ∘ show $ username) ∘ liftIO ∘ readTVarIO $ accounts_tvar)
                >>=
                maybe
                  (finishWithStatusMessage 404 "Not Found: No such account")
                  (pure ∘ (show username,))
          _ → finishWithStatusMessage 403 "Forbidden: Token does not grant access to this resource"
        )

      reader ∷ ReaderProgram ProgramResult → ActionM ()
      reader (ReaderProgram program) = do
        (username, account_tvar) ← authorize
        params_ ← params
        body_ ← body
        account ← liftIO ∘ readTVarIO $ account_tvar
        let interpret ∷
              Program ReaderInstruction α →
              ExceptT Status (Writer (Seq String)) α
            interpret (Operational.view → Return result) = pure result
            interpret (Operational.view → instruction :>>= rest) = case instruction of
              ReaderCommonInstruction common_instruction → case common_instruction of
                GetBodyInstruction → interpret (rest body_)
                GetParamsInstruction → interpret (rest params_)
                RaiseStatusInstruction s → throwError s
                LogInstruction message → do
                  tell ∘ singleton $ [i|[#{username}]: #{message}|]
                  interpret (rest ())
              ReaderViewInstruction → interpret (rest account)
            (error_or_result, logs) = runWriter ∘ runExceptT ∘ interpret $ program
        traverse_ logIO logs
        case error_or_result of
          Left status_ →
            setStatusAndLog status_
          Right (ProgramResult status_ content) → do
            setStatusAndLog status_
            setContent content

      writer ∷ WriterProgram ProgramResult → ActionM ()
      writer (WriterProgram program) = do
        (username, account_tvar) ← authorize
        params_ ← params
        body_ ← body
        let interpret ∷
              Program WriterInstruction α →
              StateT Account (ExceptT Status (Writer (Seq String))) α
            interpret (Operational.view → Return result) = pure result
            interpret (Operational.view → instruction :>>= rest) = case instruction of
              WriterCommonInstruction common_instruction → case common_instruction of
                GetBodyInstruction → interpret (rest body_)
                GetParamsInstruction → interpret (rest params_)
                RaiseStatusInstruction s → throwError s
                LogInstruction message → do
                  tell ∘ singleton $ [i|[#{username}]: #{message}|]
                  interpret (rest ())
              WriterGetAccountInstruction → get >>= interpret ∘ rest
              WriterPutAccountInstruction new_account → put new_account >> interpret (rest ())
        (status_, maybe_content, logs) ← liftIO ∘ atomically $ do
          old_account ← readTVar account_tvar
          let (error_or_result, logs) =
                runWriter
                ∘
                runExceptT
                ∘
                flip runStateT old_account
                $
                interpret program
          case error_or_result of
            Left status_ → pure (status_, Nothing, logs)
            Right (ProgramResult status_ content, new_account) → do
              writeTVar account_tvar new_account
              pure (status_, Just content, logs)
        traverse_ logIO logs
        setStatusAndLog status_
        maybe (pure ()) setContent maybe_content

      withHabit habit_id f = do
        use habits
        >>=
        maybe raiseNoSuchHabit ((habits . at habit_id .=) ∘ f)
        ∘
        lookup habit_id
      lookupHabit habit_id = do
        use (habits . at habit_id)
        >>=
        maybe raiseNoSuchHabit return

  scottyApp $ do
    Scotty.notFound $ do
      r ← Scotty.request
      logIO $ [i|URL requested: #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]
      Scotty.next
    Scotty.post "/api/create" $ do
      LoginInformation{login_username,login_password} ← bodyJSON
      logIO $ [i|Request to create an account for "#{login_username}" with password "#{login_password}"|]
      account_status ← liftIO $ do
        new_account ← newAccount login_password
        atomically $ do
          accounts ← readTVar accounts_tvar
          if member login_username accounts
            then return AccountExists
            else do
              account_tvar ← newTVar new_account
              modifyTVar accounts_tvar $ insertMap login_username account_tvar
              return AccountCreated
      case account_status of
        AccountExists → do
          logIO $ [i|Account "#{login_username}" already exists!|]
          status conflict409
        AccountCreated → do
          logIO $ [i|Account "#{login_username}" successfully created!|]
          status created201
          Scotty.text ∘ view (from strict) ∘ encodeSigned HS256 password_secret $ def
            { iss = Just expected_iss
            , sub = Just (fromJust $ stringOrURI login_username)
            }
    Scotty.post "/api/login" $ do
      LoginInformation{login_username,login_password} ← bodyJSON
      logIO $ [i|Request to log into an account with "#{login_username}" with password "#{login_password}"|]
      (
        (fmap (lookup login_username) ∘ liftIO ∘ readTVarIO $ accounts_tvar)
        >>=
        maybe (finishWithStatusMessage 404 "Not Found: No such account") return
        >>=
        liftIO ∘ readTVarIO
        >>=
        bool (finishWithStatusMessage 403 "Forbidden: Invalid password") (logIO "Login successful.")
        ∘
        passwordIsValid login_password
       )
      Scotty.text ∘ view (from strict) ∘ encodeSigned HS256 password_secret $ def
        { iss = Just expected_iss
        , sub = Just (fromJust $ stringOrURI login_username)
        }
    Scotty.get "/api/habits" ∘ reader $ do
      log "Requested all habits."
      view habits >>= returnJSON ok200
    Scotty.get "/api/habits/:habit_id" ∘ reader $ do
      habit_id ← getParam "habit_id"
      log $ [i|Requested habit with id #{habit_id}.|]
      habits_ ← view habits
      case lookup habit_id habits_ of
        Nothing → raiseNoSuchHabit
        Just habit → returnJSON ok200 habit
    Scotty.delete "/api/habits/:habit_id" ∘ writer $ do
      habit_id ← getParam "habit_id"
      log $ [i|Requested to delete habit with id #{habit_id}.|]
      habit_was_there ← isJust <$> (habits . at habit_id <<.= Nothing)
      returnNothing $
        if habit_was_there
          then noContent204
          else notFound404
    Scotty.put "/api/habits/:habit_id" ∘ writer $ do
      habit_id ← getParam "habit_id"
      log $ [i|Requested to put habit with id #{habit_id}.|]
      habit ← getBodyJSON
      habit_was_there ← isJust <$> (habits . at habit_id <<.= Just habit)
      returnNothing $
        if habit_was_there
          then noContent204
          else created201
    Scotty.get "/api/credits" ∘ reader $ do
      log $ "Requested credits."
      view (game . credits) >>= returnJSON ok200
    Scotty.post "/api/mark" ∘ writer $ do
      let markHabits ∷ [UUID] → Lens' Credits Double → WriterProgram Double
          markHabits uuids which_credits = do
            habits ← mapM lookupHabit uuids
            new_credits ←
              (+ sum (map (view $ credits . which_credits) habits))
              <$>
              use (game . credits . which_credits)
            game . credits . which_credits .= new_credits
            return new_credits
      marks ← getBodyJSON
      log $ [i|Marked #{marks ^. successes} successes and #{marks ^. failures} failures.|]
      (Credits
          <$> markHabits (marks ^. successes) success
          <*> markHabits (marks ^. failures ) failure
       ) >>= returnJSON ok200
    Scotty.post "/api/run" ∘ writer $ do
      let go d = do
            let r = runAccount d
            l_ #quest_events %= (|> r ^. story . to createEvent)
            if stillHasCredits (r ^. new_data)
              then do
                when (r ^. quest_completed) $
                  (l_ #quest_events <<.= mempty)
                  >>=
                  (l_ #quests %=) ∘ flip (|>) ∘ createQuest
                go (r ^. new_data)
              else return (r ^. new_data)
      (new_d, s) ←
        get
        >>=
        flip runStateT
          ( #quests := (mempty ∷ Seq Quest)
          , #quest_events := (mempty ∷ Seq Event)
          )
        ∘
        go
      put new_d
      returnLazyText ok200 $!! (
        renderStoryToText
        ∘
        createStory
        $
        s ^. l_ #quests |> s ^. l_ #quest_events . to createQuest
        )
    Scotty.notFound $ do
      r ← Scotty.request
      logIO $ [i|URL not found! #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]
      Scotty.next
