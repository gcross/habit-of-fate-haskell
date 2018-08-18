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

import Data.Aeson hiding ((.=))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Random
import Control.Monad.Operational (Program, interpretWithMonad)
import qualified Control.Monad.Operational as Operational
import qualified Data.ByteString.Builder as Builder
import Data.List (isSuffixOf)
import Data.Set (minView)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock
import Data.UUID hiding (null)
import GHC.Conc.Sync (unsafeIOToSTM)
import Network.HTTP.Types.Status
import Network.Wai
import System.FilePath ((</>))
import System.IO (BufferMode(LineBuffering), hSetBuffering, stderr)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Cookie
import Web.Scotty
  ( ActionM
  , Parsable
  , addHeader
  , params
  , finish
  , rescue
  , scottyApp
  , status
  )
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Credits
import HabitOfFate.Data.Habit
import HabitOfFate.Logging
import HabitOfFate.Server.Actions.Queries
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common
import HabitOfFate.Server.Program.Common
import HabitOfFate.Server.Program.Reader
import HabitOfFate.Server.Program.Writer
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
import HabitOfFate.Story.Renderer.HTML
import HabitOfFate.Story.Renderer.XML

writeDataOnChange ∷ TVar (Map Username (TVar Account)) → TMVar () → (Accounts → IO ()) → IO α
writeDataOnChange accounts_tvar write_request_var saveAccounts = forever $
  (atomically $ do
    takeTMVar write_request_var
    readTVar accounts_tvar >>= traverse readTVar
  )
  >>=
  saveAccounts

cleanCookies ∷ TVar (Map Cookie (UTCTime, Username)) → TVar (Set (UTCTime, Cookie)) → IO α
cleanCookies cookies_tvar expirations_tvar = forever $ do
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

  logIO $ "Starting server..."

  accounts_tvar ← atomically $
    traverse newTVar initial_accounts >>= newTVar
  write_request_var ← newEmptyTMVarIO

  _ ← forkIO $ writeDataOnChange accounts_tvar write_request_var saveAccounts

  cookies_tvar ← newTVarIO mempty
  expirations_tvar ← newTVarIO mempty

  _ ← forkIO $ cleanCookies cookies_tvar expirations_tvar

  let createAndReturnCookie ∷ Username → ActionM ()
      createAndReturnCookie username = do
        Cookie token ← liftIO $ do
          current_time ← getCurrentTime
          cookie ← (pack >>> Cookie) <$> (replicateM 20 $ randomRIO ('A','z'))
          atomically $ do
            let expiration_time = addUTCTime (30*86400) current_time
            modifyTVar cookies_tvar $ insertMap cookie (expiration_time, username)
            modifyTVar expirations_tvar $ insertSet (expiration_time, cookie)
          pure cookie
        def
          { setCookieName="token"
          , setCookieValue=encodeUtf8 token
          , setCookieHttpOnly=True
          , setCookieSameSite=Just sameSiteStrict
          , setCookieSecure=not test_mode
          }
          |> renderSetCookie
          |> Builder.toLazyByteString
          |> decodeUtf8
          |> Scotty.setHeader "Set-Cookie"

  let environment = Environment{..}

  scottyApp $ do

    Scotty.defaultHandler $ \message → do
      logIO [i|ERROR: #{message}|]
      Scotty.status internalServerError500
      Scotty.text message

    handleGetFile

    mapM_ ($ environment)
      [ handleDeleteHabit
      , handleEditHabit
      , handleGetAllHabits
      , handleGetCredits
      , handleGetHabit
      , handleGetQuestStatus
      , handleLoginOrCreate
      , handleLogout
      , handleMarkHabitAndRun
      , handleMoveHabit
      , handleNewHabit
      , handlePutHabit
      ]

------------------------------------- Root -------------------------------------
    Scotty.get "/" $ Scotty.redirect "/habits"
---------------------------------- Not Found -----------------------------------
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
