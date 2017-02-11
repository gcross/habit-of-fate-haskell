{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Data.UUID
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory
import System.Log.Logger
import System.Random
import Web.Scotty
import qualified Web.Scotty as Scotty

import HabitOfFate.Credits
import HabitOfFate.Data hiding (_habits)
import HabitOfFate.Game
import HabitOfFate.Habit
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
        case lookup habit_id (d ^. habits) of
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
      json ∘ toList ∘ view habits
    Scotty.get "/habits/:id" $ do
      HabitId habit_id ← param "id"
      info $ "Fetching habit " ⊕ show habit_id
      act ∘ fmap json ∘ lookupHabit $ habit_id
    Scotty.delete "/habits/:id" $ do
      HabitId habit_id ← param "id"
      act $ do
        withHabit habit_id ∘ const $ Nothing
        return $ status noContent204
    Scotty.post "/habits" $ do
      habit ← jsonData
      random_uuid ← liftIO randomIO
      act $ do
        let habit_id
              | UUID.null (habit ^. uuid) = random_uuid
              | otherwise = habit ^. uuid
        lift $ do
          modifyTVar' data_var $ habits . at habit_id .~ Just habit
          submitWriteDataRequest
        return $ do
          info $ "Posted habit " ⊕ show habit_id
          let url = url_prefix ⊕ "habit/" ⊕ toText habit_id
          addHeader "Location" ∘ (^. from strict) $ url
          status created201
          Scotty.text ∘ view (from strict) ∘ UUID.toText $ habit_id
    Scotty.get "/mark" $ do
      d ← liftIO $ readTVarIO data_var
      json $ d ^. game . credits
    Scotty.post "/mark" $ do
      marks ← jsonData
      act $ do
        let markHabits ∷ [UUID] → (Lens' Credits Double) → ServerAction Double
            markHabits uuids which_credits = do
              habits ← mapM lookupHabit uuids
              lift $ do
                new_credits ←
                  (+ sum (map (view $ credits . which_credits) habits))
                  ∘
                  view (game . credits . which_credits)
                  <$>
                  readTVar data_var
                modifyTVar' data_var $ game . credits . which_credits .~ new_credits
                return new_credits
        new_credits ←
          Credits
            <$> markHabits (marks ^. successes) success
            <*> markHabits (marks ^. failures ) failure
        lift $ submitWriteDataRequest
        return $ json new_credits
    Scotty.post "/run" $
      (liftIO ∘ atomically $ do
        d ← readTVar data_var
        let go d = do
              let r = runData d
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
          flip runStateT
            ( #quests := (mempty ∷ Seq Quest)
            , #quest_events := (mempty ∷ Seq Event)
            )
          $
          go d
        writeTVar data_var new_d
        tryPutTMVar write_request ()
        return $!! (
          renderStoryToText
          ∘
          createStory
          $
          s ^. l_ #quests |> s ^. l_ #quest_events . to createQuest
          )
      ) >>= Scotty.text

habitMain ∷ IO ()
habitMain = getDataFilePath >>= makeApp >>= run 8081
