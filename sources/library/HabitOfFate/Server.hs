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

import HabitOfFate.Credits
import HabitOfFate.Data.Account
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
import HabitOfFate.Server.Requests.GetHabit
import HabitOfFate.Server.Requests.LoginOrCreate
import HabitOfFate.Server.Requests.Logout
import HabitOfFate.Server.Requests.MoveHabit
import HabitOfFate.Server.Requests.NewHabit
import HabitOfFate.Server.Requests.PutHabit
import HabitOfFate.Story.Renderer.HTML
import HabitOfFate.Story.Renderer.XML

import Paths_habit_of_fate (getDataFileName)

runEvent = do
  account ← get
  let (event, new_account) = runState runAccount account
  put new_account
  pure event

--------------------------------------------------------------------------------
------------------------------ Server Application ------------------------------
--------------------------------------------------------------------------------

makeAppWithTestMode ∷ Bool → Accounts → (Accounts → IO ()) → IO Application
makeAppWithTestMode test_mode initial_accounts saveAccounts = do
  liftIO $ hSetBuffering stderr LineBuffering

  logIO $ "Starting server..."

  accounts_tvar ← atomically $
    traverse newTVar initial_accounts >>= newTVar

  cookies_tvar ← newTVarIO mempty
  expirations_tvar ← newTVarIO mempty

  forkIO <<< forever $
    let go =
          (
            atomically $ do
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
          >>=
          \case { False → threadDelay (60 * 1000 * 1000) >> go; _ → go }
    in go

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

  write_request_var ← newEmptyTMVarIO
  forever >>> forkIO $
    (atomically $ do
      takeTMVar write_request_var
      readTVar accounts_tvar >>= traverse readTVar
    )
    >>=
    saveAccounts

  let environment = Environment{..}

  scottyApp $ do

    Scotty.defaultHandler $ \message → do
      logIO [i|ERROR: #{message}|]
      Scotty.status internalServerError500
      Scotty.text message

    mapM_ ($ environment)
      [ handleDeleteHabit
      , handleEditHabit
      , handleGetAllHabits
      , handleGetHabit
      , handleLoginOrCreate
      , handleLogout
      , handleMoveHabit
      , handleNewHabit
      , handlePutHabit
      ]

--------------------------------- Get Credits ----------------------------------
    Scotty.get "/api/credits" <<< apiReader environment $ do
      log $ "Requested credits."
      view (stored_credits_) >>= returnJSON ok200
--------------------------------- Mark Habits ----------------------------------
    Scotty.post "/api/mark" <<< apiWriter environment $ do
      marks ← getBodyJSON
      let markHabits ∷
            Getter HabitsToMark [UUID] →
            Getter Habit Scale →
            Lens' Account Double →
            WriterProgram Double
          markHabits uuids_getter scale_getter value_lens = do
            old_value ← use value_lens
            increment ∷ Double ←
              marks
                |> (^. uuids_getter)
                |> mapM (lookupHabit >>> fmap ((^. scale_getter) >>> scaleFactor))
                |> fmap sum
            value_lens <.= old_value + increment
      log $ [i|Marking #{marks ^. succeeded} successes and #{marks ^. failed} failures.|]
      (Credits
          <$> (Successes <$> markHabits succeeded difficulty_ (stored_credits_ . successes_))
          <*> (Failures  <$> markHabits failed    importance_ (stored_credits_ . failures_ ))
       ) >>= returnJSON ok200
    let runGame = do
          event ← runEvent
          let rendered_event
                | (not <<< null) event = renderEventToHTML event
                | otherwise = H.p $ H.toHtml ("Nothing happened." ∷ Text)
          stored_credits ← use stored_credits_
          renderHTMLUsingTemplateAndReturn "Habit of Fate - Event" [] ok200 $ do
            rendered_event
            if stored_credits ^. successes_ /= 0 || stored_credits ^. failures_ /= 0
              then H.form ! A.method "post" $ H.input ! A.type_ "submit" ! A.value "Next"
              else H.a ! A.href "/habits" $ H.toHtml ("Done" ∷ Text)
        markHabit ∷
          String →
          Getter Habit Double →
          Lens' Credits Double →
          ActionM ()
        markHabit status habit_scale_getter_ credits_lens_ = webWriter environment $ do
          habits ← use habits_
          habit_id ← getParam "habit_id"
          log [i|Marking #{habit_id} as #{status}.|]
          case habits ^. at habit_id of
            Nothing →
              renderHTMLUsingTemplateAndReturn "Habit of Fate - Marking a Habit" [] notFound404 $ do
                H.h1 "Habit Not Found"
                H.p $ H.toHtml [i|"Habit #{habit_id} was not found.|]
            Just habit → do
              stored_credits_ . credits_lens_ += habit ^. habit_scale_getter_
              runGame
    Scotty.post "/mark/success/:habit_id" $ markHabit "succeeded" (difficulty_ . to scaleFactor) successes_
    Scotty.post "/mark/failure/:habit_id" $ markHabit "failed"(importance_ . to scaleFactor) failures_
    Scotty.post "/run" <<< webWriter environment $ runGame
--------------------------------- Quest Status ---------------------------------
    Scotty.get "/api/status" <<< apiReader environment $
      ask
      >>=
      (getAccountStatus >>> renderEventToXMLText >>> returnLazyText ok200)
    Scotty.get "/status" <<< webReader environment $
      (ask <&> getAccountStatus)
      >>=
      renderEventToHTMLAndReturn "Habit of Fate - Quest Status" [] ok200
----------------------------------- Run Game -----------------------------------
    Scotty.post "/api/run" <<< apiWriter environment $ do
      runEvent >>= (renderEventToXMLText >>> returnLazyText ok200)
------------------------------------- Root -------------------------------------
    Scotty.get "/" $ Scotty.redirect "/habits"
---------------------------------- Web Files -----------------------------------
    let fetch ∷ FilePath → String → Lazy.Text → Maybe Lazy.Text → ActionM ()
        fetch subdirectory extension content_type maybe_compression = do
          filepath ← param "filename"
          logIO [i|Requested file #{filepath} in #{subdirectory}|]
          when ('/' ∈ filepath) $ do
            logIO [i|Filepath #{filepath} has a slash.|]
            Scotty.next
          unless (('.':extension) `isSuffixOf` filepath) $ do
            logIO [i|Filename #{filepath} does not end with .#{extension}|]
            Scotty.next
          file_to_return ← (subdirectory </> filepath) |> getDataFileName |> liftIO
          logIO [i|Returning #{file_to_return}|]
          addHeader "Content-Type" content_type
          maybe (pure ()) (addHeader "Content-Encoding") maybe_compression
          Scotty.file file_to_return
    Scotty.get "/css/:filename" $ fetch "css" "css" "text/css" Nothing
    Scotty.get "/images/:filename" $ fetch "images" "svgz" "image/svg+xml" (Just "gzip")
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
