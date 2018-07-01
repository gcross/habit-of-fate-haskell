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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Account where

import HabitOfFate.Prelude

import Control.Exception
import Control.Monad.Random
import Crypto.PasswordStore
import Data.Aeson hiding ((.=))
import Data.UUID (UUID)

import HabitOfFate.Credits
import HabitOfFate.Habit
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

runAccount ∷ State Account Event
runAccount = do
  maybe_current_quest_state ← use maybe_current_quest_state_
  rng ← use rng_
  case maybe_current_quest_state of
    Nothing → do
      let (wrapped_quest, new_rng_1of2) = runRand (uniform quests) rng
          ((new_current_quest_state, new_awaited_credits, event), new_rng_2of2) =
            flip runRand new_rng_1of2 $ do
              case wrapped_quest of
                WrappedQuest (quest@(Quest quest_prism _ _ _)) → do
                  initial_quest_result ← quest ^. start_quest_
                  pure
                    ( initial_quest_result ^. initial_quest_state_ . re quest_prism
                    , initial_quest_result ^. initial_quest_credits_
                    , initial_quest_result ^. initial_quest_event_
                    )
      awaited_credits_ .= new_awaited_credits
      maybe_current_quest_state_ .= Just new_current_quest_state
      rng_ .= new_rng_2of2
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
                Lens' (Quest s) (ProgressToMilestoneQuestRunner s) →
                Lens' (Quest s) (AttainedMilestoneQuestRunner s) →
                State Account Event
              spend quest wrapQuestState quest_state credits_lens_ partial_lens_ complete_lens_
                | awaited_credits ^. credits_lens_ > stored_credits ^. credits_lens_ = do
                    awaited_credits_ . credits_lens_ -= stored_credits ^. credits_lens_
                    stored_credits_ . credits_lens_ .= 0
                    let (event, new_rng) =
                          quest
                            |> (^. partial_lens_)
                            |> flip runReaderT quest_state
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
                    case result ^. quest_status_ of
                      QuestInProgress new_awaited_credits→ do
                        awaited_credits_ . credits_lens_ .= new_awaited_credits
                        maybe_current_quest_state_ .=
                          (Just $ wrapQuestState new_quest_state)
                      QuestHasEnded →
                        maybe_current_quest_state_ .= Nothing
                    pure $ result ^. quest_event_

              run ∷ ∀ s. Quest s → s → State Account Event
              run quest@(Quest quest_prism _ _ _) quest_state
                | stored_credits ^. successes_ > 0 =
                    spend
                      quest
                      (^. re quest_prism)
                      quest_state
                      successes_
                      (progress_to_milestones_ . success_)
                      (attained_milestones_ . success_)
                | stored_credits ^. failures_ > 0 =
                    spend
                      quest
                      (^. re quest_prism)
                      quest_state
                      failures_
                      (progress_to_milestones_ . failure_)
                      (attained_milestones_ . failure_)
                | otherwise = pure []

          runCurrentQuest current_quest_state run

data HabitsToMark = HabitsToMark
  { _succeeded ∷ [UUID]
  , _failed ∷ [UUID]
  } deriving (Eq, Ord, Read, Show)
deriveJSON ''HabitsToMark
makeLenses ''HabitsToMark
