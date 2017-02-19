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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.App.Server where

import HabitOfFate.Prelude

import Control.Concurrent
import Control.DeepSeq
import Data.Proxy
import qualified Data.Text.Lazy as LazyText
import Data.UUID
import qualified Data.UUID as UUID
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
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

type ServerAction = StateT Account (Either ServantErr)

type GetHabits = Get '[JSON] (Map UUID Habit)
type GetHabit = Capture "habit_id" UUID :> Get '[JSON] Habit
type DeleteHabit = Capture "habit_id" UUID :> DeleteNoContent '[JSON] NoContent
type PutHabit = Capture "habit_id" UUID :> ReqBody '[JSON] Habit :> Put '[JSON] NoContent

type GetCredits = Get '[JSON] Credits
type MarkHabits = ReqBody '[JSON] HabitsToMark :> Post '[JSON] Credits

type RunGame = Post '[PlainText] LazyText.Text

type HabitAPI =
       "habits" :> (GetHabits :<|> GetHabit :<|> DeleteHabit :<|> PutHabit)
  :<|> "mark" :> (GetCredits :<|> MarkHabits)
  :<|> "run" :> RunGame

habitAPI ∷ Proxy HabitAPI
habitAPI = Proxy

makeApp ∷ FilePath → IO Application
makeApp dirpath = do
  info $ "Data and configuration files are located at " ⊕ dirpath
  createDirectoryIfMissing True dirpath
  let data_filepath = dirpath </> "data"
  data_mvar ←
    doesFileExist data_filepath
    >>=
    bool (do info "Creating new data file"
             newAccount
         )
         (do info "Reading existing data file"
             readAccount data_filepath
         )
    >>=
    newMVar
  write_request ← newEmptyMVar
  liftIO ∘ forkIO ∘ forever $ do
    takeMVar write_request
    readMVar data_mvar >>= writeAccount data_filepath
  notice $ "Starting server..."
  let run ∷ ServerAction α → Handler α
      run action = join ∘ liftIO ∘ modifyMVar data_mvar $ \old_x →
        case runStateT action old_x of
          Left e → return (old_x, throwError e)
          Right (result, new_x) → do
            tryPutMVar write_request ()
            return (new_x, return result)

      withHabit ∷ UUID → (Habit → Maybe Habit) → ServerAction ()
      withHabit habit_id f = do
        use habits
        >>=
        maybe (throwError err404) ((habits . at habit_id .=) ∘ f)
        ∘
        lookup habit_id

      lookupHabit ∷ UUID → ServerAction Habit
      lookupHabit habit_id = do
        use (habits . at habit_id)
        >>=
        maybe (throwError err404) return

      readData = liftIO $ readMVar data_mvar

      getHabits ∷ Server GetHabits
      getHabits = view habits <$> readData

      getHabit ∷ Server GetHabit
      getHabit habit_id =
        readData
        >>=
        maybe (throwError err404) return ∘ view (habits . at habit_id)

      deleteHabit ∷ Server DeleteHabit
      deleteHabit habit_id =
        fmap (const NoContent)
        ∘
        run
        ∘
        withHabit habit_id
        ∘
        const
        $
        Nothing

      putHabit ∷ Server PutHabit
      putHabit habit_id habit = run $ do
        habits . at habit_id .= Just habit
        return NoContent

      getCredits ∷ Server GetCredits
      getCredits = view (game . credits) <$> readData

      markHabits ∷ Server MarkHabits
      markHabits marks = run $ do
        let markHabits ∷ [UUID] → (Lens' Credits Double) → ServerAction Double
            markHabits uuids which_credits = do
              habits ← mapM lookupHabit uuids
              new_credits ←
                (+ sum (map (view $ credits . which_credits) habits))
                <$>
                use (game . credits . which_credits)
              game . credits . which_credits .= new_credits
              return new_credits
        new_credits ←
          Credits
            <$> markHabits (marks ^. successes) success
            <*> markHabits (marks ^. failures ) failure
        return new_credits

      runGame ∷ Server RunGame
      runGame = run $ do
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
