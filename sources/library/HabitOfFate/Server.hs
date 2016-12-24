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

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Bool
import Data.IORef
import Data.List hiding (delete)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String
import Data.Text.Lazy (Text, pack, toStrict)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder
import Data.UUID
import Network.HTTP.Types.Status
import System.Directory
import System.Log.Logger
import System.IO
import System.Random
import Web.Scotty

import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data hiding (_habits)
import qualified HabitOfFate.Game as Game
import HabitOfFate.TH
import HabitOfFate.Unicode

data SuccessesAndFailures = SuccessesAndFailures
  { _success ∷ [UUID]
  , _failure ∷ [UUID]
  }
deriveJSON ''SuccessesAndFailures
makeLenses ''SuccessesAndFailures

data Marks = Marks
  { _habits ∷ SuccessesAndFailures
  }
deriveJSON ''Marks

marked_habits ∷ Lens' Marks SuccessesAndFailures
marked_habits = lens _habits (\m h → m { _habits = h})

instance Parsable UUID where
  parseParam = maybe (Left "badly formed UUID") Right ∘ fromText ∘ toStrict

deleteAt ∷ Int → Seq α → Seq α
deleteAt i s = Seq.take i s ⊕ Seq.drop (i+1) s

insertAt ∷ Int → α → Seq α → Seq α
insertAt i x s = (Seq.take i s |> x) ⊕ Seq.drop i s

info = infoM "HabitOfFate.Server"
notice = noticeM "HabitOfFate.Server"

jsonObject = json ∘ object

port = 8081

tellChar ∷ MonadWriter Builder.Builder m ⇒ Char → m ()
tellChar = tell ∘ Builder.singleton

tellNewline ∷ MonadWriter Builder.Builder m ⇒ m ()
tellNewline = tellChar '\n'

tellString ∷ MonadWriter Builder.Builder m ⇒ String → m ()
tellString = tell ∘ Builder.fromString

tellText ∷ MonadWriter Builder.Builder m ⇒ Text → m ()
tellText = tell ∘ Builder.fromLazyText

tellLine ∷ MonadWriter Builder.Builder m ⇒ Text → m ()
tellLine t = do
  tellText t
  tellNewline

tellSeparator ∷ MonadWriter Builder.Builder m ⇒ Text → m ()
tellSeparator sep = do
  tellText $ Text.replicate 80 sep
  tellNewline

tellQuestSeparator ∷ MonadWriter Builder.Builder m ⇒ m ()
tellQuestSeparator = tellSeparator "="

tellEventSeparator ∷ MonadWriter Builder.Builder m ⇒ m ()
tellEventSeparator = tellSeparator "-"

tellParagraph ∷ MonadWriter Builder.Builder m ⇒ [String] → m ()
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

tellParagraphs ∷ MonadWriter Builder.Builder m ⇒ [[String]] → m ()
tellParagraphs (paragraph:rest) = do
    tellParagraph paragraph
    go rest
  where
    go [] = return ()
    go (paragraph:rest) = do
      tellChar '\n'
      tellParagraph paragraph
      go rest

data ActionError = ActionError Status Text
  deriving (Eq,Ord,Show)

act ∷ ExceptT ActionError IO (ActionM ()) → ActionM ()
act =
  liftIO ∘ runExceptT
  >=>
  either
    (\(ActionError code message) → do
      status code
      text message
    )
    id

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

habitMain = do
  filepath ← getDataFilePath
  info $ "Data file is located at " ++ filepath
  data_ref ←
    doesFileExist filepath
    >>=
    bool (do info "Creating new data file"
             newData
         )
         (do info "Reading existing data file"
             readData filepath
         )
    >>=
    newIORef
  write_request ← newEmptyMVar
  file_writer ← liftIO ∘ forkIO ∘ forever $
    withMVar write_request ∘ const $ readIORef data_ref >>= writeData filepath
  let writeDataToFile = liftIO ∘ void $ tryPutMVar write_request ()
      modifyAndWriteData f = liftIO $ do
        modifyIORef data_ref f
        writeDataToFile
      withIndexAndData ∷ Lens' Habit UUID → Lens' Data (Seq Habit) → (Int → Seq Habit → Seq Habit) → ActionM ()
      withIndexAndData uuid_lens collection f = do
        old_data ← liftIO $ readIORef data_ref
        id ← param "id"
        case Seq.findIndexL (hasId uuid_lens id) (old_data ^. collection) of
          Nothing → status notFound404
          Just index → modifyAndWriteData (habits %~ f index)
      readHabit maybe_uuid =
        body <&> eitherDecode'
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the document: " ⊕ error_message
            finish
          )
          (return ∘ parseEither ((.: "data") >=> habitFromDoc maybe_uuid))
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the Habit: " ⊕ error_message
            finish
          )
          return
      lookupHabit habit_id = do
        d ← liftIO $ readIORef data_ref
        case find (hasId uuid habit_id) (d ^. habits) of
          Nothing → do
            status badRequest400
            text ∘ pack $ "No habit with id " ⊕ show habit_id
            finish
          Just habit → return habit
  notice $ "Starting server at " ++ show port
  let writeDataToFile = liftIO $ readIORef data_ref >>= writeData filepath
      modifyAndWriteData f = liftIO $ do
        modifyIORef data_ref f
        writeDataToFile
      withIndexAndData ∷ Getter α UUID → Lens' Data (Seq α) → (Int → Seq α → Seq α) → ActionM ()
      withIndexAndData uuid_lens collection f = do
        old_data ← liftIO $ readIORef data_ref
        id ← param "id"
        case (Seq.findIndexL (hasId uuid_lens id) (old_data ^. collection)) of
          Nothing → status notFound404
          Just index → modifyAndWriteData $ collection %~ f index
      readHabit maybe_uuid =
        body <&> eitherDecode'
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the document: " ⊕ error_message
            finish
          )
          (return ∘ parseEither ((.: "data") >=> habitFromDoc maybe_uuid))
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the Habit: " ⊕ error_message
            finish
          )
          return
      lookupHabit habit_id = do
        d ← liftIO $ readIORef data_ref
        case find (hasId uuid habit_id) (d ^. habits) of
          Nothing → do
            status badRequest400
            text ∘ pack $ "No habit with id " ⊕ show habit_id
            finish
          Just habit → return habit
      lookupHabit' habit_id = do
        d ← liftIO $ readIORef data_ref
        case find (hasId uuid habit_id) (d ^. habits) of
          Nothing → throwError ∘ ActionError badRequest400 ∘ pack ∘ show $ habit_id
          Just habit → return habit
  scotty port $ do
    get "/habits" $ do
      user_habits ← (^. habits) <$> liftIO (readIORef data_ref)
      jsonObject
        [ "links" .= object ["self" .= String "http://localhost:8081/habits"]
        , "data" .= fmap habitToDoc user_habits
        ]
    get "/habits/:id" $ do
      habit_id ← param "id"
      act $ do
        habit ← lookupHabit' habit_id
        return ∘ jsonObject $
          [ "links" .= object
              ["self" .= String ("http://localhost:8081/habit/" ⊕ toText habit_id)
              ]
          , "data" .= habitToDoc habit
          ]
    put "/habits/:id" $ do
      habit ← readHabit Nothing
      withIndexAndData uuid habits $ (.~ habit) ∘ ix
    delete "/habits/:id" $ do
      withIndexAndData uuid habits $ deleteAt
    get "/move/habit/:id/:index" $ do
      new_index ← param "index"
      withIndexAndData uuid habits $ \index habits →
        insertAt new_index (Seq.index habits index) (deleteAt index habits)
    put "/habits" $ do
      habit ← liftIO randomIO >>= readHabit ∘ Just
      modifyAndWriteData $ habits %~ (|> habit)
    post "/mark" $ do
      marks ← jsonData
      let markHabits uuids_lens habit_credits game_credits =
            mapM lookupHabit (marks ^. uuids_lens)
            >>=
            modifyAndWriteData
              ∘
              (game . game_credits +~)
              ∘
              sum
              ∘
              map (^. habit_credits)
      markHabits (marked_habits . success) success_credits Game.success_credits
      markHabits (marked_habits . failure) failure_credits Game.failure_credits
    post "/run" $ do
      liftIO (readIORef data_ref) >>= flip unless finish ∘ stillHasCredits
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
      ((), b) ← runWriterT $
        liftIO (readIORef data_ref) >>= go >>= liftIO ∘ writeIORef data_ref
      text ∘ Builder.toLazyText $ b
      liftIO writeDataToFile
