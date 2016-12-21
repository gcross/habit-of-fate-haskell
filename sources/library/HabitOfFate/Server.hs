{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Prelude hiding (id)

import Control.Lens hiding ((.=))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Bool
import Data.IORef
import Data.List hiding (delete)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.UUID
import Network.HTTP.Types.Status
import System.Directory
import System.Log.Logger
import System.IO
import System.Random
import Web.Scotty

import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data
import qualified HabitOfFate.Game as Game
import HabitOfFate.Unicode

deleteAt ∷ Int → Seq α → Seq α
deleteAt i s = Seq.take i s ⊕ Seq.drop (i+1) s

insertAt ∷ Int → α → Seq α → Seq α
insertAt i x s = (Seq.take i s |> x) ⊕ Seq.drop i s

info = infoM "HabitOfFate.Server"
notice = noticeM "HabitOfFate.Server"

jsonObject = json ∘ object

port = 8081

hasId uuid_lens uuid = (== uuid) ∘ toText ∘ (^. uuid_lens)

habitMain = do
  filepath ← getDataFilePath
  info $ "Data file is located at " ++ filepath
  data_ref ←
    doesFileExist filepath
    >>=
    bool (do info "Creating new data file"
             return newData
         )
         (do info "Reading existing data file"
             readData filepath
         )
    >>=
    newIORef
  notice $ "Starting server at " ++ show port
  scotty port $ do
    get "/habits" $ do
      user_habits ← (^. habits) <$> liftIO (readIORef data_ref)
      jsonObject
        [ "links" .= object ["self" .= String "http://localhost:8081/habits"]
        , "data" .= fmap habitToDoc user_habits
        ]
    get "/habits/:id" $ do
      habit_id ← param "id"
      liftIO (readIORef data_ref)
        <&>
        find (hasId uuid habit_id) ∘ (^. habits)
        >>=
        maybe
          (status notFound404)
          (\habit → jsonObject
            [ "links" .= object ["self" .= String ("http://localhost:8081/habit/" ⊕ habit_id)]
            , "data" .= habitToDoc habit
            ]
          )
    let writeDataToFile = liftIO $ readIORef data_ref >>= writeData filepath
        modifyAndWriteData f = liftIO $ do
          modifyIORef data_ref f
          writeDataToFile
        withIndexAndData uuid_lens collection f = do
          old_data ← liftIO $ readIORef data_ref
          id ← param "id"
          maybe
            (status notFound404)
            (modifyAndWriteData ∘ f)
            (Seq.findIndexL (hasId uuid_lens id) (old_data ^. collection))
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
    put "/habits/:id" $ do
      habit ← readHabit Nothing
      withIndexAndData uuid habits $ \index → habits . ix index .~ habit
    delete "/habits/:id" $ do
      withIndexAndData uuid habits $ \index → habits %~ deleteAt index
    get "/move/habit/:id/:index" $ do
      new_index ← param "index"
      withIndexAndData uuid habits $ \index → habits %~
        (\h → insertAt new_index (Seq.index h index) (deleteAt index h))
    put "/habits" $ do
      habit ← liftIO randomIO >>= readHabit ∘ Just
      modifyAndWriteData $ habits %~ (|> habit)
    post "/mark/successful/habits" $
      body <&> eitherDecode'
      >>=
      either
        (\error_message → do
          status badRequest400
          text ∘ pack $ "Error when parsing the document: " ⊕ error_message
          finish
        )
        (return ∘ parseEither parseJSON)
      >>=
      either
        (\error_message → do
          status badRequest400
          text ∘ pack $ "Error when parsing the list of ids: " ⊕ error_message
          finish
        )
        return
      >>=
      mapM lookupHabit
      >>=
      modifyAndWriteData
        ∘
        (game . Game.success_credits +~)
        ∘
        sum
        ∘
        map (^. success_credits)
