{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Prelude hiding (id)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.State hiding (get, put)
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Data.Aeson hiding ((.=), json)
import Data.Bool
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lens as TextLens
import Data.UUID hiding (null)
import Text.Printf
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory
import System.Log.Logger
import System.Random
import Text.Parsec hiding (uncons)
-- import Text.Taggy hiding (run)
import Web.Scotty

import HabitOfFate.Data hiding (_habits)
import qualified HabitOfFate.Game as Game hiding (text)
import HabitOfFate.Habit
import HabitOfFate.JSON
import HabitOfFate.Unicode
import HabitOfFate.Utils

newtype HabitId = HabitId UUID

instance Parsable HabitId where
  parseParam = fmap HabitId ∘ maybe (Left "badly formed UUID") Right ∘ fromText ∘ (^. strict)

info, notice ∷ MonadIO m ⇒ String → m ()
info = liftIO ∘ infoM "HabitOfFate.Server"
notice = liftIO ∘ noticeM "HabitOfFate.Server"

data ActionError = ActionError Status (Maybe Text)
  deriving (Eq,Ord,Show)

throwActionError code = throwError $ ActionError code Nothing
throwActionErrorWithMessage code message = throwError $ ActionError code (Just message)

type ServerAction = ExceptT ActionError STM

type ServerStoryAction = WriterT Builder ServerAction

data Tag = Bold | Underline | Color deriving (Eq,Ord,Read,Show)

data FormatState = FormatState
  { _bold ∷ Bool
  , _underline ∷ Bool
  , _tags ∷ [Tag]
  }
makeLenses ''FormatState

writeChar ∷ MonadWriter Builder m ⇒ Char → m ()
writeChar = tell ∘ B.singleton

writeNewline ∷ MonadWriter Builder m ⇒ m ()
writeNewline = writeChar '\n'

writeText ∷ MonadWriter Builder m ⇒ Text → m ()
writeText = tell ∘ B.fromText

writeLine ∷ MonadWriter Builder m ⇒ Text → m ()
writeLine t = writeText t >> writeNewline

writeQuestSeparator ∷ MonadWriter Builder m ⇒ m ()
writeQuestSeparator = writeLine "<questSeparator/>"

writeEventSeparator ∷ MonadWriter Builder m ⇒ m ()
writeEventSeparator = writeLine "<eventSeparator/>"

writeFormattedText ∷ Text → ServerStoryAction ()
writeFormattedText =
  runParserT parser (FormatState False False []) ""
  >=>
  either (error ∘ show) return
  where
    parser ∷ ParsecT Text FormatState ServerStoryAction ()
    parser = do
      spaces
      lift $ writeLine "<p>"
      fmap mconcat ∘ many1 $ choice
        [do string "\n\n"
            getState
              >>=
              flip unless (fail "unclosed formats in paragraph")
              ∘
              null
              ∘
              (^. tags)
            spaces
            lift $ writeLine "</p><p>"
        ,parseFormatChar '*' "b" Bold bold "bold"
        ,parseFormatChar '_' "u" Underline underline "underline"
        ,do char '['
            hue ←
              choice
              ∘
              map (\(c,color) → char c >> return color)
              $
              [('r',"red")
              ,('r',"blue")
              ,('r',"green")
              ]
            modifyState $ tags %~ (Color:)
            lift ∘ writeText $ "<color hue=\"" ⊕ hue ⊕ "\">"
        ,do char ']'
            getState <&> (^. tags) >>= \case
              Color:rest_tags → do
                modifyState $ tags .~ rest_tags
                lift ∘ writeText $ "</color>"
              _ → fail "color closed when not the most recent format"
        ]
      lift $ writeLine "</p>"

    parseFormatChar ∷ Char → Text → Tag → Lens' FormatState Bool → String → ParsecT Text FormatState ServerStoryAction ()
    parseFormatChar format_char tag_name tag format_lens format_name = do
      char format_char
      active ← getState
      if active ^. format_lens
        then case active ^. tags of
          top_tag:rest_tags | top_tag == tag → do
            modifyState $ (format_lens .~ False) ∘ (tags .~ rest_tags)
            lift ∘ writeText $ "</\"" ⊕ tag_name ⊕ "\">"
          _ → fail $ printf "%s closed when not the most recent format" format_name
        else do
          modifyState $ (format_lens .~ True) ∘ (tags %~ (tag:))
          lift ∘ writeText $ "<\"" ⊕ tag_name ⊕ "\">"

act ∷ ServerAction (ActionM ()) → ActionM ()
act =
  liftIO ∘ atomically ∘ runExceptT
  >=>
  either
    (\(ActionError code maybe_message) → do
      status code
      maybe (return ()) (text ∘ (^. from strict)) maybe_message
    )
    identity

act' ∷ ServerAction α → ActionM ()
act' run = act (run >> return (return ()))

hasId ∷ Getter α UUID → UUID → α → Bool
hasId uuid_lens uuid = (== uuid) ∘ (^. uuid_lens)

unmakeJSONAction ∷ Value → UnmakeJSON α → ServerAction α
unmakeJSONAction value action =
  either
    (throwActionErrorWithMessage badRequest400 ∘ Text.pack)
    return
  $
  unmakeJSON value action

makeApp ∷ FilePath → IO Application
makeApp filepath = do
  let url_prefix = "http://localhost:8081/" ∷ Text
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
          Nothing → throwActionErrorWithMessage notFound404 ∘ toText $ habit_id
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
                Text.pack
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
    post "/run" ∘ act' ∘ fmap text $ do
        d ← lift $ readTVar data_var
        if stillHasCredits d
          then do
            let go ∷ Data → ServerStoryAction Data
                go d = do
                  let r = runData d
                  writeFormattedText $ r ^. story
                  if stillHasCredits (r ^. new_data)
                    then do
                      if r ^. quest_completed
                        then writeQuestSeparator
                        else writeEventSeparator
                      go (r ^. new_data)
                    else return (r ^. new_data)
            (new_d, b) ← runWriterT ∘ go $ d
            lift $ do
              writeTVar data_var new_d
              tryPutTMVar write_request ()
            return ∘ B.toLazyText $ b
          else do
            return "No credits."

habitMain ∷ IO ()
habitMain = getDataFilePath >>= makeApp >>= run 8081
