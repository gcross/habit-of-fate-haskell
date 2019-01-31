{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.StateMachine where

import HabitOfFate.Prelude hiding (State)

import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Control.Monad.Random (MonadRandom,uniform)
import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Story
import HabitOfFate.Substitution
import HabitOfFate.TH
import HabitOfFate.Trial

data State α = State
  { substitutions ∷ HashMap Text Gendered
  , current ∷ α
  } deriving (Eq,Ord,Read,Show)
makeLenses ''State
deriveJSON ''State

data Transition α = Transition
  { outcomes ∷ StoryOutcomes
  , between_stories ∷ [Story]
  , status_story ∷ Story
  , next ∷ [α]
  , extra_subs ∷ Tagged ([(Text, Gendered)])
  } deriving (Functor)

type Transitions α = [(α, Transition α)]

initialize ∷ Substitutions → α → Story → InitializeQuestRunner (State α)
initialize common_substitutions first intro_story = do
  InitializeQuestResult
    (State common_substitutions first)
    <$> substitute common_substitutions intro_story

data NoSuchTransitionException = ∀ α. Show α ⇒ NoSuchTransitionException α
instance Show NoSuchTransitionException where
  show (NoSuchTransitionException x) = [i|NoSuchTransitionException (#{show x})|]
instance Exception NoSuchTransitionException where
  displayException (NoSuchTransitionException state) = [i|No such state #{state}|]

getStatus ∷ (Show α, Eq α) ⇒ Transitions α → GetStatusQuestRunner (State α)
getStatus states State{..} =
  case lookup current states of
    Nothing → throwM $ NoSuchTransitionException current
    Just Transition{..} → substitute substitutions status_story

data MissingStory = ∀ α. Show α ⇒ MissingStory α Text
instance Show MissingStory where
  show (MissingStory label category) = [i|MissingStory #{label} "#{category}"|]
instance Exception MissingStory where
  displayException (MissingStory label category) =
    [i|Missing any story of category "#{category}" for state labeled #{label}.|]

uniformOrDie ∷ (MonadRandom m, MonadThrow m, Show α) ⇒ α → Text → [x] → m x
uniformOrDie label name list
  | onull list = throwM $ MissingStory label name
  | otherwise = uniform list

trial ∷ (Eq α, Show α) ⇒ Transitions α → TrialQuestRunner (State α)
trial transitions result scale = do
  old_state@State{..} ← get
  Transition{..} ← maybe (throwM $ NoSuchTransitionException current) pure $ lookup current transitions
  let subSuccess, subFailure ∷ Story → MonadThrow m ⇒ m Lazy.Text
      subSuccess = substitute (substitutions ⊕ mapFromList (extra_subs ^. success_))
      subFailure = substitute (substitutions ⊕ mapFromList (extra_subs ^. failure_))
  tryBinomial (1/3) scale >>= bool
    (TryQuestResult QuestInProgress <$> (uniformOrDie current "between" between_stories >>= substitute substitutions))
    (if onull next
      then case result of
        SuccessResult → TryQuestResult QuestHasEnded <$> (subSuccess $ storyForSuccess outcomes)
        FailureResult → tryBinomial (1/2) scale >>= bool
            (TryQuestResult QuestHasEnded   <$> (subFailure $ storyForFailure outcomes))
            (TryQuestResult QuestInProgress <$> (subFailure $ storyForAverted outcomes))
      else do
        next_current ← uniform next
        put $ old_state { current = next_current }
        case result of
          SuccessResult → TryQuestResult QuestInProgress <$> (subSuccess $ storyForSuccess outcomes)
          FailureResult → tryBinomial (1/2) scale >>= bool
            (TryQuestResult QuestHasEnded <$> (subFailure $ storyForFailure outcomes))
            (TryQuestResult QuestHasEnded <$> (subFailure $ storyForAverted outcomes))
    )
