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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Zones.All (TZLabel(America__New_York))
import Data.Typeable (Typeable)
import Web.Scotty (Parsable)

import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = show >>> toJSON

instance FromJSON StdGen where
  parseJSON = parseJSON >>> fmap read

instance ToJSON TZLabel where
  toJSON = show >>> toJSON

instance FromJSON TZLabel where
  parseJSON (String label) =
    case readEither (unpack label) of
      Left error_message → fail error_message
      Right timezone → pure timezone
  parseJSON _ = fail "Expected a string."

type Group = Text
type Groups = ItemsSequence Group

data Account = Account
  {   _password_ ∷ Text
  ,   _habits_ ∷ ItemsSequence Habit
  ,   _stored_credits_ ∷ Tagged Double
  ,   _maybe_current_quest_state_ ∷ Maybe CurrentQuestState
  ,   _rng_ ∷ StdGen
  ,   _timezone_ ∷ TZLabel
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
    <*> pure (Tagged (Success 0) (Failure 0))
    <*> pure Nothing
    <*> newStdGen
    <*> pure America__New_York
    <*> getCurrentTime
    <*> pure []

passwordIsValid ∷ Text → Account → Bool
passwordIsValid password account =
  verifyPassword (encodeUtf8 password) (encodeUtf8 $ account ^. password_)

getAccountStatus ∷ Account → Event
getAccountStatus account =
  maybe
    ["You are between quests."]
    (runCurrentQuest run)
    (account ^. maybe_current_quest_state_)
  where
   run ∷ ∀ s. Quest s → s → Event
   run quest quest_state = questGetStatus quest quest_state

runAccount ∷ State Account Event
runAccount = do
  maybe_current_quest_state ← use maybe_current_quest_state_
  rng ← use rng_
  case maybe_current_quest_state of
    Nothing → do
      let ((new_current_quest_state, event), new_rng) = flip runRand rng $ do
            WrappedQuest quest ← uniform quests
            InitializeQuestResult quest_state intro_event ← questInitialize quest
            pure
              ( quest_state ^. re (questPrism quest)
              , intro_event
              )
      maybe_current_quest_state_ .= Just new_current_quest_state
      rng_ .= new_rng
      pure event
    Just current_quest_state → do
      let run ∷ ∀ s. Quest s → s → State Account Event
          run quest quest_state = do
            stored_credits ← use stored_credits_
            let ((TryQuestResult quest_status new_stored_credits event, new_quest_state), new_rng) =
                  quest
                    |> questTrial
                    |> ($ stored_credits)
                    |> flip runStateT quest_state
                    |> flip runRand rng ∷ ((TryQuestResult, s), StdGen)
            rng_ .= new_rng
            stored_credits_ .= new_stored_credits
            maybe_current_quest_state_ .=
              case quest_status of
                QuestHasEnded → Nothing
                QuestInProgress → Just $ new_quest_state ^. re (questPrism quest)
            pure event
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
