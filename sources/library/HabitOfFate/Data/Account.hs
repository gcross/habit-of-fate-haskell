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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Account where

import HabitOfFate.Prelude

import Control.Exception
import Control.Monad.Random (StdGen, newStdGen, runRand, uniform)
import Crypto.PasswordStore
import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), Value(..))
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Web.Scotty (Parsable)

import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Quests
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = show >>> toJSON

instance FromJSON StdGen where
  parseJSON = parseJSON >>> fmap read

type Group = Text
type Groups = ItemsSequence Group

data Account = Account
  {   _password_ ∷ Text
  ,   _habits_ ∷ ItemsSequence Habit
  ,   _marks_ ∷ Tagged (Seq Scale)
  ,   _maybe_current_quest_state_ ∷ Maybe CurrentQuestState
  ,   _rng_ ∷ StdGen
  ,   _configuration_ ∷ Configuration
  ,   _last_seen_ ∷ UTCTime
  ,   _groups_ ∷ Groups
  } deriving (Read,Show)
deriveJSON ''Account
makeLenses ''Account

newAccount ∷ Text → IO Account
newAccount password =
  Account
    <$> (
          makePassword (encodeUtf8 password) 17
          >>=
          (decodeUtf8 >>> evaluate)
        )
    <*> pure def
    <*> pure (Tagged (Success def) (Failure def))
    <*> pure Nothing
    <*> newStdGen
    <*> pure def
    <*> getCurrentTime
    <*> pure []

passwordIsValid ∷ Text → Account → Bool
passwordIsValid password account =
  verifyPassword (encodeUtf8 password) (encodeUtf8 $ account ^. password_)

getAccountStatus ∷ Account → Lazy.Text
getAccountStatus account =
  maybe
    "You are between quests."
    (runCurrentQuest run)
    (account ^. maybe_current_quest_state_)
  where
   run ∷ ∀ s. Quest s → s → Lazy.Text
   run quest quest_state = questGetStatus quest quest_state

runAccount ∷ State Account Lazy.Text
runAccount = do
  maybe_current_quest_state ← use maybe_current_quest_state_
  rng ← use rng_
  case maybe_current_quest_state of
    Nothing → do
      let ((new_current_quest_state, event), new_rng) = flip runRand rng $ do
            WrappedQuest quest ← uniform quests
            InitializeQuestResult quest_state intro_event ← questInitialize quest
            pure
              ( questPrism quest # quest_state
              , intro_event
              )
      maybe_current_quest_state_ .= Just new_current_quest_state
      rng_ .= new_rng
      pure event
    Just current_quest_state → do
      let run ∷ ∀ s. Quest s → s → State Account Lazy.Text
          run quest quest_state = do
            marks ← use marks_
            maybe_runQuestTrial ←
              case uncons (marks ^. success_) of
                Just (scale, rest) → do
                  marks_ . success_ .= rest
                  pure $ Just $ questTrial >>> ($ SuccessResult) >>> ($ scale)
                Nothing →
                  case uncons (marks ^. failure_) of
                    Just (scale, rest) → do
                      marks_ . failure_ .= rest
                      pure $ Just $ questTrial >>> ($ FailureResult) >>> ($ scale)
                    Nothing →
                      pure Nothing
            case maybe_runQuestTrial of
              Just runQuestTrial → do
                let ((TryQuestResult quest_status event, new_quest_state), new_rng) =
                      quest
                        |> runQuestTrial
                        |> flip runStateT quest_state
                        |> flip runRand rng ∷ ((TryQuestResult, s), StdGen)
                rng_ .= new_rng
                maybe_current_quest_state_ .=
                  case quest_status of
                    QuestHasEnded → Nothing
                    QuestInProgress → Just $ new_quest_state ^. re (questPrism quest)
                pure event
              Nothing →
                pure $ questGetStatus quest quest_state
      runCurrentQuest run current_quest_state

newtype Username = Username { unwrapUsername ∷ Text } deriving
  ( Eq
  , FromJSONKey
  , Ord
  , Parsable
  , Read
  , Show
  , ToJSONKey
  )

instance FromJSON Username where
  parseJSON = parseJSON >>> fmap Username

instance ToJSON Username where
  toJSON = unwrapUsername >>> toJSON
  toEncoding = unwrapUsername >>> toEncoding
