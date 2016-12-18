{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server where

import Data.Text
import Network.Wai.Handler.Warp
import Servant

import HabitOfFate.Behaviors.Habit
import HabitOfFate.Unicode

type API = "habits" :> Get '[JSON] [Habit]

api ∷ Proxy API
api = Proxy

server ∷ Server API
server = return [Habit "Test" 1 0]

app ∷ Application
app = serve api server

port = 8081

habitMain = do
  putStrLn $ "Starting server at " ++ show port
  run port app
