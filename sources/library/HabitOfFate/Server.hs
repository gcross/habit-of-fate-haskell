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
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson hiding (json)
import Data.Aeson.Types (parseEither)
import Data.Bool
import Data.List hiding (delete)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.UUID
import qualified Data.UUID as UUID
import Text.Printf
import Network.HTTP.Types.Status
import System.Directory
import System.Log.Logger
import System.Random
import Web.Scotty

import HabitOfFate.Data hiding (_habits)
import qualified HabitOfFate.Game as Game
import HabitOfFate.Habit
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

newtype HabitId = HabitId UUID

instance Parsable HabitId where
  parseParam = fmap HabitId ∘ maybe (Left "badly formed UUID") Right ∘ fromText ∘ L.toStrict

deleteAt ∷ Int → Seq α → Seq α
deleteAt i s = Seq.take i s ⊕ Seq.drop (i+1) s

insertAt ∷ Int → α → Seq α → Seq α
insertAt i x s = (Seq.take i s |> x) ⊕ Seq.drop i s

info, notice ∷ String → IO ()
info = infoM "HabitOfFate.Server"
notice = noticeM "HabitOfFate.Server"

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
    id

act' ∷ ServerAction α → ActionM ()
act' run = act (run >> return (return ()))

throwActionError code = throwError $ ActionError code Nothing
throwActionErrorWithMessage code message = throwError $ ActionError code (Just message)

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

decodeAction =
  either
    (throwActionErrorWithMessage badRequest400 ∘ S.pack ∘ ("Error decoding JSON: " ⊕))
    return
  ∘
  eitherDecode

decodeAndParseHabitAction =
  decodeAction
  >=>
  either
    (throwActionErrorWithMessage badRequest400 ∘ S.pack ∘ ("Error parsing the habit: " ⊕))
    return
  ∘
  parseEither parseHabitDoc

habitMain ∷ IO ()
habitMain = do
  let url_prefix = "http://localhost:8081/" ∷ S.Text
      port = 8081
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
  liftIO ∘ forkIO ∘ forever $
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
      lookupHabit ∷ UUID → ServerAction Habit
      lookupHabit habit_id = do
        d ← lift $ readTVar data_var
        case find (hasId uuid habit_id) (d ^. habits) of
          Nothing → throwActionErrorWithMessage badRequest400 ∘ S.pack ∘ show $ habit_id
          Just habit → return habit
      modifyAndWriteData f = lift $ do
        modifyTVar data_var f
        tryPutTMVar write_request ()
  scotty port $ do
    get "/habits" $ do
      liftIO (readTVarIO data_var)
      >>=
      json ∘ generateHabitsDoc (url_prefix ⊕ "habits") ∘ (^. habits)
    get "/habits/:id" $ do
      HabitId habit_id ← param "id"
      act $ json ∘ generateHabitDoc (url_prefix ⊕ "habits/" ⊕ UUID.toText habit_id) <$> lookupHabit habit_id
    put "/habits/:id" $ do
      habit_body ← body
      HabitId habit_id ← param "id"
      act' $ do
        habit ← decodeAndParseHabitAction habit_body
        unless (habit ^. uuid == habit_id || UUID.null (habit ^. uuid))
          ∘
          throwActionErrorWithMessage badRequest400
          ∘
          S.pack
          $
          printf
            "UUID of uploaded doc (%s) does not match the UUID in the URL (%s)"
            (toString $ habit ^. uuid)
            (toString $ habit_id)
        withIndexAndData uuid habit_id habits $ (.~ habit) ∘ ix
    delete "/habits/:id" $ do
      HabitId habit_id ← param "id"
      act $ do
        withIndexAndData uuid habit_id habits deleteAt
        return $ status noContent204
    get "/move/habit/:id/:index" $ do
      HabitId habit_id ← param "id"
      new_index ← param "index"
      act' $ do
        withIndexAndData uuid habit_id habits $ \index habits →
          insertAt new_index (Seq.index habits index) (deleteAt index habits)
    put "/habits" $ do
      habit_body ← body
      random_uuid ← liftIO randomIO
      act $ do
        habit ← decodeAndParseHabitAction habit_body
        unless (UUID.null $ habit ^. uuid) $
          throwActionErrorWithMessage badRequest400  "uploaded doc was given an id"
        modifyAndWriteData $ habits %~ (|> (uuid .~ random_uuid) habit)
        return $ status created201
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
            return ∘ B.toLazyText $ b
          else do
            return "No credits."
      ) >>= text
