{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Server
  ( Username(..)
  , makeApp
  , makeAppRunningInTestMode
  ) where

import HabitOfFate.Prelude

import Control.Concurrent
import Control.Concurrent.STM
import Data.Set (minView)
import Data.Time.Clock
import GHC.Conc.Sync (unsafeIOToSTM)
import Network.HTTP.Types.Status
import Network.Wai
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging
import HabitOfFate.Server.Common

import HabitOfFate.Server.Requests.DeleteHabit
import HabitOfFate.Server.Requests.EditHabit
import HabitOfFate.Server.Requests.GetAllHabits
import HabitOfFate.Server.Requests.GetCredits
import HabitOfFate.Server.Requests.GetFile
import HabitOfFate.Server.Requests.GetHabit
import HabitOfFate.Server.Requests.GetQuestStatus
import HabitOfFate.Server.Requests.LoginOrCreate
import HabitOfFate.Server.Requests.Logout
import HabitOfFate.Server.Requests.MarkHabitAndRun
import HabitOfFate.Server.Requests.MoveHabit
import HabitOfFate.Server.Requests.NewHabit
import HabitOfFate.Server.Requests.PutHabit

--------------------------------------------------------------------------------
------------------------------ Background Threads ------------------------------
--------------------------------------------------------------------------------

writeDataOnChange ∷ Environment → (Accounts → IO ()) → IO α
writeDataOnChange Environment{..} saveAccounts = forever $
  (atomically $ do
    takeTMVar write_request_var
    readTVar accounts_tvar >>= traverse readTVar
  )
  >>=
  saveAccounts

cleanCookies ∷ Environment → IO α
cleanCookies Environment{..} = forever $ do
  dropped ←
    (atomically $ do
      current_time ← unsafeIOToSTM getCurrentTime
      expirations ← readTVar expirations_tvar
      case minView expirations of
        Nothing → pure False
        Just (first@(first_time, first_cookie), rest) → do
          if first_time < current_time
            then do
              unsafeIOToSTM $ logIO [i|Dropping cookie #{first} at #{current_time}.|]
              modifyTVar cookies_tvar $ deleteMap first_cookie
              writeTVar expirations_tvar rest
              pure True
            else
              pure False
    )
  unless dropped $ threadDelay (60 * 1000 * 1000)

--------------------------------------------------------------------------------
------------------------------ Server Application ------------------------------
--------------------------------------------------------------------------------

makeAppWithTestMode ∷ Bool → Accounts → (Accounts → IO ()) → IO Application
makeAppWithTestMode test_mode initial_accounts saveAccounts = do
  liftIO $ hSetBuffering stderr LineBuffering

  logIO "Starting server..."

  accounts_tvar ← atomically $ traverse newTVar initial_accounts >>= newTVar
  write_request_var ← newEmptyTMVarIO
  cookies_tvar ← newTVarIO mempty
  expirations_tvar ← newTVarIO mempty

  let environment = Environment{..}

  _ ← forkIO $ writeDataOnChange environment saveAccounts
  _ ← forkIO $ cleanCookies environment

  Scotty.scottyApp $ do

    Scotty.defaultHandler $ \message → do
      logIO [i|ERROR: #{message}|]
      Scotty.status internalServerError500
      Scotty.text message

    handleGetFile

    mapM_ ($ environment)
      [ handleDeleteHabit
      , handleNewHabit -- MUST be before EditHabit or creating a new habit breaks
      , handleEditHabit
      , handleGetAllHabits
      , handleGetCredits
      , handleGetHabit
      , handleGetQuestStatus
      , handleLoginOrCreate
      , handleLogout
      , handleMarkHabitAndRun
      , handleMoveHabit
      , handlePutHabit
      ]

    Scotty.get "/" $ Scotty.redirect "/habits"

    Scotty.notFound $ do
      r ← Scotty.request
      logIO [i|URL not found! #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]
      Scotty.next

makeApp ∷
  Map Username Account →
  (Map Username Account → IO ()) →
  IO Application
makeApp = makeAppWithTestMode False

makeAppRunningInTestMode ∷
  Map Username Account →
  (Map Username Account → IO ()) →
  IO Application
makeAppRunningInTestMode = makeAppWithTestMode True
