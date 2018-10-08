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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Account where

import HabitOfFate.Prelude

import Control.Exception
import Control.Monad.Random
import Crypto.PasswordStore
import Data.Aeson hiding ((.=))
import Data.UUID (UUID)
import Web.Scotty (Parsable)

import HabitOfFate.Data.Credits
import HabitOfFate.Data.Habit
import HabitOfFate.Quest
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.Tag
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = show >>> toJSON

instance FromJSON StdGen where
  parseJSON = parseJSON >>> fmap read

data Account = Account
  {   _password_ ∷ Text
  ,   _habits_ ∷ Habits
  ,   _stored_credits_ ∷ Credits
  ,   _awaited_credits_ ∷ Credits
  ,   _maybe_current_quest_state_ ∷ Maybe CurrentQuestState
  ,   _rng_ :: StdGen
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
    <*> pure def
    <*> pure def
    <*> pure Nothing
    <*> newStdGen

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
      let ((new_current_quest_state, new_awaited_credits, event), new_rng) = flip runRand rng $ do
            WrappedQuest quest ← uniform quests
            initial_quest_result ← questNewState quest
            let quest_state = initialQuestState initial_quest_result
            intro_event ← questIntro quest quest_state
            pure
              ( quest_state ^. re (questPrism quest)
              , initialQuestCredits initial_quest_result
              , intro_event
              )
      awaited_credits_ .= new_awaited_credits
      maybe_current_quest_state_ .= Just new_current_quest_state
      rng_ .= new_rng
      pure $ event
    Just current_quest_state → do
      stored_credits ← use stored_credits_
      if (stored_credits ^. successes_ <= 0) && (stored_credits ^. failures_ <= 0)
        then pure []
        else do
          awaited_credits ← use awaited_credits_
          let spend ∷
                Quest s →
                (s → CurrentQuestState) →
                s →
                Lens' Credits Double →
                Getter (Quest s) (ProgressToMilestoneQuestRunner s) →
                Getter (Quest s) (AttainedMilestoneQuestRunner s) →
                State Account Event
              spend quest wrapQuestState quest_state credits_lens_ partial_lens_ complete_lens_
                | awaited_credits ^. credits_lens_ > stored_credits ^. credits_lens_ = do
                    awaited_credits_ . credits_lens_ -= stored_credits ^. credits_lens_
                    stored_credits_ . credits_lens_ .= 0
                    let (event, new_rng) =
                          quest
                            |> (^. partial_lens_)
                            |> ($ quest_state)
                            |> flip runRand rng
                    rng_ .= new_rng
                    pure event
                | otherwise = do
                    awaited_credits_ . credits_lens_ .= 0
                    stored_credits_ . credits_lens_ -= stored_credits ^. credits_lens_
                    let ((result, new_quest_state), new_rng) =
                          quest
                            |> (^. complete_lens_)
                            |> flip runStateT quest_state
                            |> flip runRand rng
                    rng_ .= new_rng
                    case questStatus result of
                      QuestInProgress new_awaited_credits→ do
                        awaited_credits_ . credits_lens_ .= new_awaited_credits
                        maybe_current_quest_state_ .=
                          (Just $ wrapQuestState new_quest_state)
                      QuestHasEnded →
                        maybe_current_quest_state_ .= Nothing
                    pure $ questEvent result

              run ∷ ∀ s. Quest s → s → State Account Event
              run quest quest_state
                | stored_credits ^. successes_ > 0 =
                    spend
                      quest
                      (^. re (questPrism quest))
                      quest_state
                      successes_
                      (to progressToMilestones . success_)
                      (to attainedMilestones . success_)
                | stored_credits ^. failures_ > 0 =
                    spend
                      quest
                      (^. re (questPrism quest))
                      quest_state
                      failures_
                      (to progressToMilestones . failure_)
                      (to attainedMilestones . failure_)
                | otherwise = pure []

          runCurrentQuest run current_quest_state

data HabitsToMark = HabitsToMark
  { _succeeded_ ∷ [UUID]
  , _failed_ ∷ [UUID]
  } deriving (Eq, Ord, Read, Show)
deriveJSON ''HabitsToMark
makeLenses ''HabitsToMark

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
