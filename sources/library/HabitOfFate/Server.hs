{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Prelude hiding (id)

import Control.Lens hiding ((.=))
import qualified Control.Lens as Lens
import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Bool
import Data.List
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.UUID
import Network.HTTP.Types.Status
import System.Directory
import System.Log.Logger
import System.IO
import Web.Scotty

import HabitOfFate.Behaviors
import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data
import HabitOfFate.Unicode

info = infoM "HabitOfFate.Server"
notice = noticeM "HabitOfFate.Server"

jsonObject = json ∘ object

port = 8081

habitMain = do
  filepath ← getDataFilePath
  info $ "Data file is located at " ++ filepath
  mvar ←
    doesFileExist filepath
    >>=
    bool (do info "Creating new data file"
             return newData
         )
         (do info "Reading existing data file"
             readData filepath
         )
    >>=
    newMVar
  notice $ "Starting server at " ++ show port
  scotty port $ do
    get "/habits" $ do
      user_habits ← (^. behaviors . habits) <$> liftIO (readMVar mvar)
      jsonObject
        [ "links" .= object ["self" .= String "http://localhost:8081/habits"]
        , "data" .= map habitToDoc user_habits
        ]
    get "/habits/:id" $ do
      habit_id ← param "id"
      liftIO (readMVar mvar)
        <&>
        find ((== habit_id) ∘ toText ∘ (^. uuid)) ∘ (^. behaviors . habits)
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
      found ← liftIO ∘ modifyMVar mvar $ \d →
          case findIndex ((== habit_id) ∘ toText ∘ (^. uuid)) $ d ^. behaviors . habits of
            Nothing → return (d, False)
            Just index → return (d & (behaviors . habits . ix index) .~ habit, True)
      unless found $ do
        status notFound404
        finish
