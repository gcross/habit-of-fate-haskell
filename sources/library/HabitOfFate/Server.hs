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
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Lens hiding ((.=))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Bool
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
      maybe (return ()) text maybe_message
    )
    id


act' ∷ ServerAction α → ActionM ()
act' run = act (run >> return (return ()))

throwActionError code = throwError $ ActionError code Nothing
throwActionErrorWithMessage code message = throwError $ ActionError code (Just message)

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

habitMain = do
  filepath ← getDataFilePath
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
  file_writer ← liftIO ∘ forkIO ∘ forever $
    (atomically $ do
      takeTMVar write_request
      readTVar data_var
    ) >>= writeData filepath
  notice $ "Starting server at " ++ show port
  let withIndexAndData ∷ Getter α UUID → UUID → Lens' Data (Seq α) → (Int → Seq α → Seq α) → ServerAction ()
      withIndexAndData uuid_lens item_id collection f = do
        d ← lift $ readTVar data_var
        case Seq.findIndexL (hasId uuid_lens item_id) (d ^. collection) of
          Nothing → throwActionError notFound404
          Just index → lift $ writeTVar data_var $ d & collection %~ f index
      readHabit maybe_uuid =
        return ∘ eitherDecode'
        >=>
        either
          (
            throwActionErrorWithMessage badRequest400
            ∘
            pack
            ∘
            ("Error when parsing the document: " ⊕)
          )
          (return ∘ parseEither ((.: "data") >=> habitFromDoc maybe_uuid))
        >=>
        either
          (
            throwActionErrorWithMessage badRequest400
            ∘
            pack
            ∘
            ("Error when parsing the habit: " ⊕)
          )
          return
      lookupHabit ∷ UUID → ServerAction Habit
      lookupHabit habit_id = do
        d ← lift $ readTVar data_var
        case find (hasId uuid habit_id) (d ^. habits) of
          Nothing → throwActionErrorWithMessage badRequest400 ∘ pack ∘ show $ habit_id
          Just habit → return habit
      modifyAndWriteData f = lift $ do
        modifyTVar data_var f
        tryPutTMVar write_request ()
  scotty port $ do
    get "/habits" $ do
      user_habits ← (^. habits) <$> liftIO (readTVarIO data_var)
      jsonObject
        [ "links" .= object ["self" .= String "http://localhost:8081/habits"]
        , "data" .= fmap habitToDoc user_habits
        ]
    get "/habits/:id" $ do
      habit_id ← param "id"
      act $ do
        habit ← lookupHabit habit_id
        return ∘ jsonObject $
          [ "links" .= object
              ["self" .= String ("http://localhost:8081/habit/" ⊕ toText habit_id)
              ]
          , "data" .= habitToDoc habit
          ]
    put "/habits/:id" $ do
      habit_body ← body
      habit_id ← param "id"
      act' $ do
        habit ← readHabit Nothing habit_body
        withIndexAndData uuid habit_id habits $ (.~ habit) ∘ ix
    delete "/habits/:id" $ do
      habit_id ← param "id"
      act' $ withIndexAndData uuid habit_id habits deleteAt
    get "/move/habit/:id/:index" $ do
      habit_id ← param "id"
      new_index ← param "index"
      act' $ do
        withIndexAndData uuid habit_id habits $ \index habits →
          insertAt new_index (Seq.index habits index) (deleteAt index habits)
    put "/habits" $ do
      habit_body ← body
      seed ← liftIO randomIO
      act' $ do
        habit ← readHabit (Just seed) habit_body
        modifyAndWriteData $ habits %~ (|> habit)
    post "/mark" $ do
      marks ← jsonData
      act' $ do
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
            return ∘ Builder.toLazyText $ b
          else do
            return "No credits."
      ) >>= text
