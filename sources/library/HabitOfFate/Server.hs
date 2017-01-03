{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Prelude hiding (id)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson hiding (json)
import Data.Aeson.Types (parseEither)
import Data.Bool
import Data.List hiding (delete)
import qualified Data.Map as Map
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.UUID
import Text.Printf
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory
import System.Log.Logger
import System.Random
import Web.Scotty

import HabitOfFate.Data hiding (_habits)
import qualified HabitOfFate.Game as Game
import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.TH
import HabitOfFate.Unicode
import HabitOfFate.Utils

newtype HabitId = HabitId UUID

instance Parsable HabitId where
  parseParam = fmap HabitId ∘ maybe (Left "badly formed UUID") Right ∘ fromText ∘ L.toStrict

info, notice ∷ MonadIO m ⇒ String → m ()
info = liftIO ∘ infoM "HabitOfFate.Server"
notice = liftIO ∘ noticeM "HabitOfFate.Server"

tellChar ∷ MonadWriter Builder m ⇒ Char → m ()
tellChar = tell ∘ B.singleton

tellNewline ∷ MonadWriter Builder m ⇒ m ()
tellNewline = tellChar '\n'

tellString ∷ MonadWriter Builder m ⇒ String → m ()
tellString = tell ∘ B.fromString

tellText ∷ MonadWriter Builder m ⇒ S.Text → m ()
tellText = tell ∘ B.fromText

tellLine ∷ MonadWriter Builder m ⇒ S.Text → m ()
tellLine t = do
  tellText t
  tellNewline

tellSeparator ∷ MonadWriter Builder m ⇒ S.Text → m ()
tellSeparator sep = do
  tellText $ S.replicate 80 sep
  tellNewline

tellQuestSeparator ∷ MonadWriter Builder m ⇒ m ()
tellQuestSeparator = tellSeparator "="

tellEventSeparator ∷ MonadWriter Builder m ⇒ m ()
tellEventSeparator = tellSeparator "-"

tellParagraph ∷ MonadWriter Builder m ⇒ [String] → m ()
tellParagraph = go 0
  where
    go _ [] = tellChar '\n'
    go cols words@(word:rest)
      | cols == 0 = do
          tellString word
          go (length word) rest
      | cols + 1 + length word > 80 = do
          tellChar '\n'
          go 0 words
      | otherwise = do
          tellChar ' '
          tellString word
          go (cols + 1 + length word) rest

tellParagraphs ∷ MonadWriter Builder m ⇒ [[String]] → m ()
tellParagraphs (paragraph:rest) = do
    tellParagraph paragraph
    go rest
  where
    go [] = return ()
    go (paragraph:rest) = do
      tellChar '\n'
      tellParagraph paragraph
      go rest

data ActionError = ActionError Status (Maybe S.Text)
  deriving (Eq,Ord,Show)

type ServerAction = ExceptT ActionError STM

act ∷ ServerAction (ActionM ()) → ActionM ()
act =
  liftIO ∘ atomically ∘ runExceptT
  >=>
  either
    (\(ActionError code maybe_message) → do
      status code
      maybe (return ()) (text ∘ L.fromStrict) maybe_message
    )
    identity

act' ∷ ServerAction α → ActionM ()
act' run = act (run >> return (return ()))

throwActionError code = throwError $ ActionError code Nothing
throwActionErrorWithMessage code message = throwError $ ActionError code (Just message)

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

unmakeJSONAction ∷ Value → UnmakeJSON α → ServerAction α
unmakeJSONAction value action =
  either
    (throwActionErrorWithMessage badRequest400 ∘ S.pack)
    return
  $
  unmakeJSON value action

makeApp ∷ FilePath → IO Application
makeApp filepath = do
  let url_prefix = "http://localhost:8081/" ∷ S.Text
  info $ "Data file is located at " ++ filepath
  data_var ←
    doesFileExist filepath
    >>=
    bool (do info "Creating new data file"
             newData
         )
         (do info "Reading existing data file"
             readData filepath
         )
    >>=
    newTVarIO
  write_request ← newEmptyTMVarIO
  liftIO ∘ forkIO ∘ forever $
    (atomically $ do
      takeTMVar write_request
      readTVar data_var
    ) >>= writeData filepath
  notice $ "Starting server..."
  let withHabit ∷ UUID → (Habit → Maybe Habit) → ServerAction ()
      withHabit habit_id f = do
        d ← lift $ readTVar data_var
        case d ^. habits . at habit_id of
          Nothing → throwActionErrorWithMessage notFound404 ∘ toText $ habit_id
          Just habit → lift ∘ modifyTVar' data_var $ habits . at habit_id .~ f habit
      lookupHabit ∷ UUID → ServerAction Habit
      lookupHabit habit_id = do
        d ← lift $ readTVar data_var
        case d ^. habits . at habit_id of
          Nothing → throwActionErrorWithMessage badRequest400 ∘ toText $ habit_id
          Just habit → return habit
      submitWriteDataRequest = void $ tryPutTMVar write_request ()
  scottyApp $ do
    get "/habits" $ do
      liftIO (readTVarIO data_var)
      >>=
      json
      ∘
      (\hs → makeJSON $ do
        addObject "links" ∘ add "self" $ url_prefix ⊕ "habits"
        add "data" $
          map
           (\(uuid, habit) → makeJSON $ do
             add "id" uuid
             addText "type" "habit"
             add "attributes" habit
           )
           (Map.toList hs)
      )
      ∘
      (^. habits)
    get "/habits/:id" $ do
      HabitId habit_id ← param "id"
      info $ "Fetching habit " ⊕ show habit_id
      let url = url_prefix ⊕ "habits/" ⊕ toText habit_id
      act $ do
        habit ← lookupHabit habit_id
        return ∘ json ∘ makeJSON $ do
          addObject "links" ∘ add "self" $ url
          addObject "data" $ do
            add "id" habit_id
            addText "type" "habit"
            add "attributes" habit
    put "/habits/:id" $ do
      doc ← jsonData
      HabitId habit_id ← param "id"
      info $ "Replacing habit " ⊕ show habit_id
      act' $
        (unmakeJSONAction doc ∘ retrieveObject "data" $ do
          checkTypeIs "habit"
          checkIdIfPresentIs habit_id
          retrieve "attributes"
        )
        >>=
        withHabit habit_id ∘ const ∘ Just
    delete "/habits/:id" $ do
      HabitId habit_id ← param "id"
      act $ do
        withHabit habit_id ∘ const $ Nothing
        return $ status noContent204
    put "/habits" $ do
      doc ← jsonData
      random_uuid ← liftIO randomIO
      act $ do
        (maybe_uuid, habit) ← unmakeJSONAction doc ∘ retrieveObject "data" $ do
          checkTypeIs "habit"
          (,) <$> retrieveMaybe "id" <*> retrieve "attributes"
        d ← lift $ readTVar data_var
        uuid ← case maybe_uuid of
          Nothing → return random_uuid
          Just uuid →
            if Map.member uuid (d ^. habits)
              then
                throwActionErrorWithMessage conflict409
                ∘
                S.pack
                $
                printf "A habit with id %s already exists" (show uuid)
              else return uuid
        lift $ do
          writeTVar data_var $ d & habits . at uuid .~ Just habit
          submitWriteDataRequest
        return $ do
          info $ "Created habit " ⊕ show uuid
          let url = url_prefix ⊕ "habit/" ⊕ toText uuid
          addHeader "Location" ∘ (^. from strict) $ url
          status created201
          json ∘ makeJSON ∘ addObject "data" $ do
            add "id" uuid
            addText "type" "habit"
            add "attributes" habit
            addObject "links" ∘ add "self" $ url
    get "/mark" $ do
      d ← liftIO $ readTVarIO data_var
      json ∘ makeJSON $ do
        add "success" (d ^. game . Game.success_credits)
        add "failure" (d ^. game . Game.failure_credits)
    post "/mark" $ do
      doc ← jsonData
      act' $ do
        (success_habits, failure_habits) ← unmakeJSONAction doc $
          (,) <$> retrieve "success" <*> retrieve "failure"
        let markHabits uuids habit_credits game_credits =
              mapM lookupHabit uuids
              >>=
              lift
                ∘
                modifyTVar' data_var
                ∘
                (game . game_credits +~)
                ∘
                sum
                ∘
                map (^. habit_credits)
        markHabits success_habits success_credits Game.success_credits
        markHabits failure_habits failure_credits Game.failure_credits
        lift $ submitWriteDataRequest
    post "/run" $
      (liftIO ∘ atomically $ do
        d ← readTVar data_var
        if stillHasCredits d
          then do
            let go d = do
                  let r = runData d
                  tellParagraphs (r ^. paragraphs)
                  if stillHasCredits (r ^. new_data)
                    then do
                      tellNewline
                      if r ^. quest_completed
                        then do
                          tellQuestSeparator
                          tellLine "A new quest begins..."
                          tellQuestSeparator
                          tellNewline
                        else do
                          tellEventSeparator
                          tellNewline
                      go (r ^. new_data)
                    else return (r ^. new_data)
            (new_d, b) ← runWriterT ∘ go $ d
            writeTVar data_var new_d
            tryPutTMVar write_request ()
            return ∘ B.toLazyText $ b
          else do
            return "No credits."
      ) >>= text

habitMain ∷ IO ()
habitMain = getDataFilePath >>= makeApp >>= run 8081
