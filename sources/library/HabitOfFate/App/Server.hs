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
import Data.Proxy
import qualified Data.Text.Lazy as LazyText
import Data.UUID
import qualified Data.UUID as UUID
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import System.Directory
import System.FilePath
import System.Log.Logger
import Web.HttpApiData

import Servant.API hiding (addHeader)
import Servant.Server

import HabitOfFate.Credits
import HabitOfFate.Account hiding (_habits)
import HabitOfFate.Habit
import HabitOfFate.Story

newtype HabitId = HabitId UUID

instance ToHttpApiData UUID.UUID where
    toUrlPiece = UUID.toText
    toHeader   = UUID.toASCIIBytes

instance FromHttpApiData UUID.UUID where
    parseUrlPiece = maybe (Left "invalid UUID") Right . UUID.fromText
    parseHeader   = maybe (Left "invalid UUID") Right . UUID.fromASCIIBytes

info, notice ∷ MonadIO m ⇒ String → m ()
info = liftIO ∘ infoM "HabitOfFate.Server"
notice = liftIO ∘ noticeM "HabitOfFate.Server"

type ServerAction = ExceptT ServantErr STM

act ∷ ServerAction α → Handler α
act =
  liftIO ∘ atomically ∘ runExceptT
  >=>
  either throwError return

type HabitAPI =
       "habits" :>
  (      Get '[JSON] (Map UUID Habit)
    :<|> Capture "habit_id" UUID :> Get '[JSON] Habit
    :<|> Capture "habit_id" UUID :> DeleteNoContent '[JSON] NoContent
    :<|> Capture "habit_id" UUID :> ReqBody '[JSON] Habit :> Put '[JSON] NoContent
  )
  :<|> "mark" :>
  (      Get '[JSON] Credits
    :<|> ReqBody '[JSON] HabitsToMark :> Post '[JSON] Credits
  )
  :<|> "run" :> Post '[PlainText] LazyText.Text
habitAPI ∷ Proxy HabitAPI
habitAPI = Proxy

makeApp ∷ FilePath → IO Application
makeApp dirpath = do
  info $ "Data and configuration files are located at " ⊕ dirpath
  createDirectoryIfMissing True dirpath
  let data_filepath = dirpath </> "data"
  data_var ←
    doesFileExist data_filepath
    >>=
    bool (do info "Creating new data file"
             newAccount
         )
         (do info "Reading existing data file"
             readAccount data_filepath
         )
    >>=
    newTVarIO
  write_request ← newEmptyTMVarIO
  liftIO ∘ forkIO ∘ forever $
    (atomically $ do
      takeTMVar write_request
      readTVar data_var
    ) >>= writeAccount data_filepath
  notice $ "Starting server..."
  let withHabit ∷ UUID → (Habit → Maybe Habit) → ServerAction ()
      withHabit habit_id f = do
        d ← lift $ readTVar data_var
        case lookup habit_id (d ^. habits) of
          Nothing → throwError err404
          Just habit → lift ∘ modifyTVar' data_var $ habits . at habit_id .~ f habit
      lookupHabit ∷ UUID → ServerAction Habit
      lookupHabit habit_id = do
        d ← lift $ readTVar data_var
        case d ^. habits . at habit_id of
          Nothing → throwError err404
          Just habit → return habit
      submitWriteDataRequest = void $ tryPutTMVar write_request ()
      getHabits = view habits <$> liftIO (readTVarIO data_var)
      getHabit = act ∘ lookupHabit
      deleteHabit habit_id =
        fmap (const NoContent)
        ∘
        act
        ∘
        withHabit habit_id
        ∘
        const
        $
        Nothing
      putHabit habit_id habit = act ∘ lift $ do
        modifyTVar' data_var $ habits . at habit_id .~ Just habit
        submitWriteDataRequest
        return NoContent
      getCredits = liftIO (readTVarIO data_var) <&> view (game . credits)
      markHabits marks = act $ do
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
        return new_credits
      runGame = liftIO ∘ atomically $ do
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
          readTVar data_var
          >>=
          flip runStateT
            ( #quests := (mempty ∷ Seq Quest)
            , #quest_events := (mempty ∷ Seq Event)
            )
          ∘
          go
        writeTVar data_var new_d
        tryPutTMVar write_request ()
        return $!! (
          renderStoryToText
          ∘
          createStory
          $
          s ^. l_ #quests |> s ^. l_ #quest_events . to createQuest
         )
  return ∘ serve habitAPI $
         (getHabits :<|> getHabit :<|> deleteHabit :<|> putHabit)
    :<|> (getCredits :<|> markHabits)
    :<|> runGame

habitMain ∷ IO ()
habitMain = do
  dirpath ← getAccountFilePath
  makeApp dirpath
    >>=
    runTLS
      (tlsSettings (dirpath </> "certificate.pem") (dirpath </> "key.pem"))
      (setPort 8081 defaultSettings)
