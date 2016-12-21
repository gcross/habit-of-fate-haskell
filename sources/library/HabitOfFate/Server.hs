{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.List
import qualified Data.Sequence as Seq
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.UUID
import Network.HTTP.Types.Status
import System.Directory
import System.Log.Logger
import System.IO
import Web.Scotty

import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data
import HabitOfFate.Unicode

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
    put "/habits/:id" $ do
      habit_id ← param "id"
      habit ←
        body <&> eitherDecode'
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the document: " ⊕ error_message
            finish
          )
          (return ∘ parseEither ((.: "data") >=> habitFromDoc))
        >>=
        either
          (\error_message → do
            status badRequest400
            text ∘ pack $ "Error when parsing the Habit: " ⊕ error_message
            finish
          )
          return
      old_data ← liftIO $ readIORef data_ref
      case Seq.findIndexL (hasId uuid habit_id) (old_data ^. habits) of
          Nothing → status notFound404
          Just index → liftIO $ do
            let new_data = old_data & habits . ix index .~ habit
            writeIORef data_ref new_data
            writeData filepath new_data
