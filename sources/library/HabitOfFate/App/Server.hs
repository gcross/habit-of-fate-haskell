{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Data.Aeson hiding ((.=), json)
import Data.UUID
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory
import System.Log.Logger
import System.Random
import Web.Scotty
import qualified Web.Scotty as Scotty

import HabitOfFate.Data hiding (_habits)
import qualified HabitOfFate.Game as Game
import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Story

newtype HabitId = HabitId UUID

instance Parsable HabitId where
  parseParam = fmap HabitId ∘ maybe (Left "badly formed UUID") Right ∘ fromText ∘ (^. strict)

info, notice ∷ MonadIO m ⇒ String → m ()
info = liftIO ∘ infoM "HabitOfFate.Server"
notice = liftIO ∘ noticeM "HabitOfFate.Server"

data ActionError = ActionError Status (Maybe Text)
  deriving (Eq,Ord,Show)

type ServerAction = ExceptT ActionError STM

act ∷ ServerAction (ActionM ()) → ActionM ()
act =
  liftIO ∘ atomically ∘ runExceptT
  >=>
  either
    (\(ActionError code maybe_message) → do
      status code
      maybe (return ()) (Scotty.text ∘ (^. from strict)) maybe_message
    )
    identity

act' ∷ ServerAction α → ActionM ()
act' run = act (run >> return (return ()))

throwActionError code = throwError $ ActionError code Nothing
throwActionErrorWithMessage code message = throwError $ ActionError code (Just message)

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

runJSONParserAction ∷ Value → JSONParser α → ServerAction α
runJSONParserAction value action =
  either
    (throwActionErrorWithMessage badRequest400 ∘ (^. packed))
    return
  $
  runJSONParser value action

makeApp ∷ FilePath → IO Application
makeApp filepath = do
  let url_prefix = "http://localhost:8081/" ∷ Text
  info $ "Data file is located at " ⊕ filepath
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
          Nothing → throwActionErrorWithMessage notFound404 ∘ toText $ habit_id
          Just habit → return habit
      submitWriteDataRequest = void $ tryPutTMVar write_request ()
  scottyApp $ do
    Scotty.get "/habits" $ do
      liftIO (readTVarIO data_var)
      >>=
      json
      ∘
      (\hs → runJSONCreator $ do
        addObject "links" ∘ add "self" $ url_prefix ⊕ "habits"
        add "data" $
          fmap
           (\(uuid, habit) → runJSONCreator $ do
             add "id" uuid
             addText "type" "habit"
             add "attributes" habit
           )
           (mapToList hs)
      )
      ∘
      (^. habits)
    Scotty.get "/habits/:id" $ do
      HabitId habit_id ← param "id"
      info $ "Fetching habit " ⊕ show habit_id
      let url = url_prefix ⊕ "habits/" ⊕ toText habit_id
      act $ do
        habit ← lookupHabit habit_id
        return ∘ json ∘ runJSONCreator $ do
          addObject "links" ∘ add "self" $ url
          addObject "data" $ do
            add "id" habit_id
            addText "type" "habit"
            add "attributes" habit
    Scotty.put "/habits/:id" $ do
      doc ← jsonData
      HabitId habit_id ← param "id"
      info $ "Replacing habit " ⊕ show habit_id
      act' $
        (runJSONParserAction doc ∘ retrieveObject "data" $ do
          checkTypeIs "habit"
          checkIdIfPresentIs habit_id
          retrieve "attributes"
        )
        >>=
        withHabit habit_id ∘ const ∘ Just
    Scotty.delete "/habits/:id" $ do
      HabitId habit_id ← param "id"
      act $ do
        withHabit habit_id ∘ const $ Nothing
        return $ status noContent204
    Scotty.put "/habits" $ do
      doc ← jsonData
      random_uuid ← liftIO randomIO
      act $ do
        (maybe_uuid, habit) ← runJSONParserAction doc ∘ retrieveObject "data" $ do
          checkTypeIs "habit"
          (,) <$> retrieveMaybe "id" <*> retrieve "attributes"
        d ← lift $ readTVar data_var
        uuid ← case maybe_uuid of
          Nothing → return random_uuid
          Just uuid →
            if member uuid (d ^. habits)
              then
                throwActionErrorWithMessage conflict409
                ∘
                (^. packed)
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
          json ∘ runJSONCreator ∘ addObject "data" $ do
            add "id" uuid
            addText "type" "habit"
            add "attributes" habit
            addObject "links" ∘ add "self" $ url
    Scotty.get "/mark" $ do
      d ← liftIO $ readTVarIO data_var
      json ∘ runJSONCreator $ do
        add "success" (d ^. game . Game.success_credits)
        add "failure" (d ^. game . Game.failure_credits)
    Scotty.post "/mark" $ do
      doc ← jsonData
      act' $ do
        (success_habits, failure_habits) ← runJSONParserAction doc $
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
    Scotty.post "/run" $
      (liftIO ∘ atomically $ do
        d ← readTVar data_var
        let quests = _1 ∷ Lens' (Seq Quest, Seq Event) (Seq Quest)
            quest_events = _2 ∷ Lens' (Seq Quest, Seq Event) (Seq Event)
            go d = do
              let r = runData d
              quest_events %= (|> r ^. story . to createEvent)
              if stillHasCredits (r ^. new_data)
                then do
                  when (r ^. quest_completed) $
                    (quest_events <<.= mempty)
                    >>=
                    (quests %=) ∘ flip (|>) ∘ createQuest
                  go (r ^. new_data)
                else return (r ^. new_data)
        (new_d, s) ← flip runStateT (mempty,mempty) ∘ go $ d
        writeTVar data_var new_d
        tryPutTMVar write_request ()
        return $!! (
          renderStoryToText
          ∘
          createStory
          $
          s ^. quests |> s ^. quest_events . to createQuest
          )
      ) >>= Scotty.text

habitMain ∷ IO ()
habitMain = getDataFilePath >>= makeApp >>= run 8081
