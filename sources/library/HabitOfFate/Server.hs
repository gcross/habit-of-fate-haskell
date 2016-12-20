{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Prelude hiding (id)

import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Bool
import Data.List
import Data.String
import Data.Text (Text)
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
        , "data" .= map habitToJSON user_habits
        ]
    get "/habits/:id" $ do
      habit_id ← param "id"
      liftIO (readMVar mvar)
        <&>
        find ((== habit_id) ∘ toText ∘ (^. id)) ∘ (^. behaviors . habits)
        >>=
        maybe
          (status notFound404)
          (\habit → jsonObject
            [ "links" .= object ["self" .= String ("http://localhost:8081/habit/" ⊕ habit_id)]
            , "data" .= habitToJSON habit
            ]
          )
