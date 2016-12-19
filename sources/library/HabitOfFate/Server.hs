{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Control.Lens
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Bool
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Log.Logger
import System.IO

import HabitOfFate.Behaviors
import HabitOfFate.Behaviors.Habit
import HabitOfFate.Data
import HabitOfFate.Unicode

type API = "habits" :> Get '[JSON] [Habit]

api ∷ Proxy API
api = Proxy

server ∷ MVar Data → Server API
server mvar = (^. behaviors . habits) <$> liftIO (readMVar mvar)

app ∷ MVar Data → Application
app mvar = serve api (server mvar)

port = 8081

--------------------------------------------------------------------------------
------------------------------------- Main -------------------------------------
--------------------------------------------------------------------------------

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
  run port (app mvar)

--------------------------------------------------------------------------------
------------------------------------ Loggers -----------------------------------
--------------------------------------------------------------------------------

info = infoM "HabitOfFate.Server"
notice = noticeM "HabitOfFate.Server"
