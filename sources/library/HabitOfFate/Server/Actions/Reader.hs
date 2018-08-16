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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Actions.Reader where

import HabitOfFate.Prelude

import Control.Monad.Operational (Program, interpretWithMonad)
import qualified Control.Monad.Operational as Operational
import Network.HTTP.Types.Status (Status)
import Web.Scotty (ActionM, params)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Logging
import HabitOfFate.Server.Actions.Common
import HabitOfFate.Server.Actions.Queries
import HabitOfFate.Server.Actions.Results
import HabitOfFate.Server.Common

data ReaderInstruction α where
  ReaderCommonInstruction ∷ CommonInstruction α → ReaderInstruction α
  ReaderViewInstruction ∷ ReaderInstruction Account

newtype ReaderProgram α = ReaderProgram
  { unwrapReaderProgram ∷ Program ReaderInstruction α }
  deriving (Applicative, Functor, Monad)

instance ActionMonad ReaderProgram where
  singletonCommon = ReaderCommonInstruction >>> Operational.singleton >>> ReaderProgram

instance MonadReader Account (ReaderProgram) where
  ask = ReaderProgram $ Operational.singleton ReaderViewInstruction
  local = error "if you see this, then ReaderProgram needs to have a local method"

readerWith ∷ (∀ α. String → ActionM α) → Environment → ReaderProgram ProgramResult → ActionM ()
readerWith actionWhenAuthFails environment (ReaderProgram program) = do
  logRequest
  (username, account_tvar) ← authorizeWith actionWhenAuthFails environment
  params_ ← params
  body_ ← Scotty.body
  account ← account_tvar |> readTVarMonadIO
  let interpret ∷ ReaderInstruction α → ExceptT Status (Writer (Seq String)) α
      interpret (ReaderCommonInstruction GetBodyInstruction) = pure body_
      interpret (ReaderCommonInstruction GetParamsInstruction) = pure params_
      interpret (ReaderCommonInstruction (RaiseStatusInstruction s)) = throwError s
      interpret (ReaderCommonInstruction (LogInstruction message)) =
        void ([i|[#{unwrapUsername username}]: #{message}|] |> singleton |> tell)
      interpret ReaderViewInstruction = pure account
      (error_or_result, logs) =
        program
          |> interpretWithMonad interpret
          |> runExceptT
          |> runWriter
  traverse_ logIO logs
  case error_or_result of
    Left status_ →
      setStatusAndLog status_
    Right (ProgramRedirectsTo href) → Scotty.redirect href
    Right (ProgramResult status_ content) → do
      setStatusAndLog status_
      setContent content
