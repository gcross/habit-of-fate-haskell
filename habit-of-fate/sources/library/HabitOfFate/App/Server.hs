{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad.Operational (Program, ProgramViewT(..))
import qualified Control.Monad.Operational as Operational
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Text.IO
import qualified Data.Text.Lazy as Lazy
import Data.UUID
import Data.Yaml hiding ((.=))
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.WarpTLS
import System.Environment
import System.Directory
import System.FilePath
import System.Log.Logger
import System.Random
import Web.JWT hiding (decode, header)
import Web.Scotty hiding (delete, get, post, put)
import qualified Web.Scotty as Scotty

import HabitOfFate.Credits
import HabitOfFate.Account hiding (_habits)
import HabitOfFate.Story

instance Parsable UUID where
  parseParam = maybe (Left "badly formed UUID") Right ∘ fromText ∘ view strict

info, notice ∷ MonadIO m ⇒ String → m ()
info = liftIO ∘ infoM "HabitOfFate.Server"
notice = liftIO ∘ noticeM "HabitOfFate.Server"

getDataFilePath ∷ IO FilePath
getDataFilePath =
  getArgs >>= \case
    [] → getHomeDirectory <&> (</> ".habit")
    [filepath] → return filepath
    _ → error "Only one argument may be provided."

data CommonInstructionInstruction α where
  GetBodyInstruction ∷ CommonInstructionInstruction Lazy.ByteString
  GetParamsInstruction ∷ CommonInstructionInstruction [Param]
  RaiseStatusInstruction ∷ Status → CommonInstructionInstruction α

class Monad m ⇒ ActionMonad m where
  singletonCommon ∷ CommonInstructionInstruction α → m α

finishWithStatus ∷ Status → ActionM α
finishWithStatus s = status s >> finish

finishWithStatusMessage ∷ Int → String → ActionM α
finishWithStatusMessage code = finishWithStatus ∘ Status code ∘ encodeUtf8 ∘ pack

getBody ∷ ActionMonad m ⇒ m Lazy.ByteString
getBody = singletonCommon GetBodyInstruction

getBodyJSON ∷ (FromJSON α, ActionMonad m) ⇒ m α
getBodyJSON =
  getBody
  >>=
  maybe (raiseStatus badRequest400) return ∘ decode ∘ view strict

getParams ∷ ActionMonad m ⇒ m [Param]
getParams = singletonCommon GetParamsInstruction

getParam ∷ (Parsable α, ActionMonad m) ⇒ Lazy.Text → m α
getParam param_name = do
  params_ ← getParams
  case lookup param_name params_ of
    Nothing → raiseStatus internalServerError500
    Just value → case parseParam value of
      Left _ → raiseStatus badRequest400
      Right x → return x

raiseStatus ∷ ActionMonad m ⇒ Status → m α
raiseStatus = singletonCommon ∘ RaiseStatusInstruction

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

loadAccounts ∷ FilePath → IO (Map Text (TVar Account))
loadAccounts =
  liftIO ∘ BS.readFile
  >=>
  either error return . decodeEither
  >=>
  traverse newTVarIO

saveAccounts ∷ FilePath → Map Text (TVar Account) → IO ()
saveAccounts filepath =
  traverse readTVarIO
  >=>
  encodeFile filepath

makeApp ∷ FilePath → IO Application
makeApp dirpath = do
  info $ "Data and configuration files are located at " ⊕ dirpath
  createDirectoryIfMissing True dirpath
  let data_filepath = dirpath </> "data"
  accounts_tvar ∷ TVar (Map Text (TVar Account)) ←
    doesFileExist data_filepath
    >>=
    bool (do info "Creating new data file"
             singletonMap "bitslayer" <$> (newAccount "password" >>= newTVarIO)
         )
         (do info "Reading existing data file"
             loadAccounts data_filepath
         )
    >>=
    newTVarIO
  let secret_filepath = dirpath </> "secret"
  key ←
    doesFileExist secret_filepath
    >>=
    bool (do info "Creating new secret"
             key ← toText <$> randomIO
             writeFile secret_filepath key
             return $ secret key
         )
         (do info "Reading existing secret"
             secret <$> readFile secret_filepath
         )
  write_request ← newEmptyTMVarIO
  liftIO ∘ forkIO ∘ forever $ do
    atomically $ takeTMVar write_request
    readTVarIO accounts_tvar >>= saveAccounts data_filepath

  key ← secret ∘ toText <$> randomIO
  notice $ "Starting server..."
  let postprocess ∷ Either Status ProgramResult → ActionM ()
      postprocess (Left s) = status s
      postprocess (Right (ProgramResult s content)) = do
        status s
        case content of
          NoContent → return ()
          TextContent t → Scotty.text t
          JSONContent j → Scotty.json j

      expected_iss = fromJust $ stringOrURI "habit-of-fate"

      authorize ∷ ActionM (TVar Account)
      authorize =
        header "Authorization"
        >>=
        maybe
          (finishWithStatusMessage 403 "Forbidden: No authorization token.")
          return
        >>=
        \case
          (words → ["Bearer", token]) → return token
          header → finishWithStatusMessage 403 (printf "Forbidden: Unrecognized authorization header: %s" header)
        >>=
        maybe
          (finishWithStatusMessage 403 "Forbidden: Unable to verify key")
          return
        ∘
        decodeAndVerifySignature key
        ∘
        view strict
        >>=
        (\case
          (claims → JWTClaimsSet { iss = Just observed_iss, sub = Just username })
            | observed_iss == expected_iss →
                ((fmap ∘ lookup ∘ pack ∘ show $ username) ∘ liftIO ∘ readTVarIO $ accounts_tvar)
                >>=
                maybe (finishWithStatusMessage 404 "Not Found: No such account") return
          _ → finishWithStatusMessage 403 "Forbidden: Token does not grant access to this resource"
        )

      reader ∷ ReaderProgram ProgramResult → ActionM ()
      reader (ReaderProgram program) = do
        account_tvar ← authorize
        params_ ← params
        body_ ← body
        account ← liftIO ∘ readTVarIO $ account_tvar
        let interpret (Operational.view → Return result) = return result
            interpret (Operational.view → instruction :>>= rest) = case instruction of
              ReaderCommonInstruction common_instruction → case common_instruction of
                GetBodyInstruction → interpret (rest body_)
                GetParamsInstruction → interpret (rest params_)
                RaiseStatusInstruction s → throwError s
              ReaderViewInstruction → interpret (rest account)
        postprocess (interpret program)

      writer ∷ WriterProgram ProgramResult → ActionM ()
      writer (WriterProgram program) = do
        account_tvar ← authorize
        params_ ← params
        body_ ← body
        let interpret (Operational.view → Return result) account = return (result, account)
            interpret (Operational.view → instruction :>>= rest) account = case instruction of
              WriterCommonInstruction common_instruction → case common_instruction of
                GetBodyInstruction → interpret (rest body_) account
                GetParamsInstruction → interpret (rest params_) account
                RaiseStatusInstruction s → throwError s
              WriterGetAccountInstruction → interpret (rest account) account
              WriterPutAccountInstruction new_account → interpret (rest ()) new_account
        (liftIO ∘ atomically $ do
          old_account ← readTVar account_tvar
          case interpret program old_account of
            Left s → return $ Left s
            Right (result, new_account) → do
              writeTVar account_tvar new_account
              return (Right result)
         ) >>= postprocess

      withHabit habit_id f = do
        use habits
        >>=
        maybe (raiseStatus notFound404) ((habits . at habit_id .=) ∘ f)
        ∘
        lookup habit_id
      lookupHabit habit_id = do
        use (habits . at habit_id)
        >>=
        maybe (raiseStatus notFound404) return
  scottyApp $ do
    Scotty.post "/login" $ do
      username ← param "username"
      password ← param "password"
      (
        (fmap (lookup username) ∘ liftIO ∘ readTVarIO $ accounts_tvar)
        >>=
        maybe (finishWithStatusMessage 404 "Not Found: No such account") return
        >>=
        liftIO ∘ readTVarIO
        >>=
        bool (finishWithStatusMessage 403 "Forbidden: Invalid password") (return ())
        ∘
        passwordIsValid password
       )
      Scotty.text ∘ view (from strict) ∘ encodeSigned HS256 key $ def
        { iss = Just expected_iss
        , sub = Just (fromJust $ stringOrURI username)
        }
    Scotty.get "/habits" ∘ reader $
      view habits >>= returnJSON ok200
    Scotty.get "/habits/:habit_id" ∘ reader $ do
      habit_id ← getParam "habit_id"
      habits_ ← view habits
      case lookup habit_id habits_ of
        Nothing → raiseStatus notFound404
        Just habit → returnJSON ok200 habit
    Scotty.delete "/habits/:habit_id" ∘ writer $ do
      habit_id ← getParam "habit_id"
      habit_was_there ← isNothing <$> (habits . at habit_id <<.= Nothing)
      returnNothing $
        if habit_was_there
          then ok200
          else notFound404
    Scotty.put "/habits/:habit_id" ∘ writer $ do
      habit_id ← getParam "habit_id"
      habit ← getBodyJSON
      habit_was_there ← isNothing <$> (habits . at habit_id <<.= Just habit)
      returnNothing $
        if habit_was_there
          then accepted202
          else created201
    Scotty.get "/credits" ∘ reader $
      view (game . credits) >>= returnJSON ok200
    Scotty.post "/mark" ∘ writer $ do
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
      (Credits
          <$> markHabits (marks ^. successes) success
          <*> markHabits (marks ^. failures ) failure
       ) >>= returnJSON ok200
    Scotty.post "/run" ∘ writer $ do
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

habitMain ∷ IO ()
habitMain = do
  dirpath ← getAccountFilePath
  makeApp dirpath
    >>=
    runTLS
      (tlsSettings (dirpath </> "certificate.pem") (dirpath </> "key.pem"))
      (setPort 8081 defaultSettings)
