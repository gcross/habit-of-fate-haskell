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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.StateMachine where

import HabitOfFate.Prelude hiding (State)

import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Control.Monad.Random (MonadRandom,uniform)

import HabitOfFate.Data.Deed
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Story
import HabitOfFate.Substitution
import HabitOfFate.TH
import HabitOfFate.Trial

data State label s = State
  { substitutions ∷ HashMap Text Gendered
  , current ∷ label
  , internal ∷ s
  } deriving (Eq,Ord,Read,Show)
makeLenses ''State
deriveJSON ''State

data Transition label = Transition
  { outcomes ∷ StoryOutcomes
  , between_stories ∷ [Story]
  , status_story ∷ Story
  , next ∷ [label]
  , extra_subs ∷ Tagged ([(Text, Gendered)])
  } deriving (Eq,Functor,Ord,Read,Show)

type Transitions label = [(label, Transition label)]

initialize ∷ Substitutions → label → s → Story → InitializeQuestRunner (State label s)
initialize common_substitutions first_label internal intro_story = do
  InitializeQuestResult
    (State common_substitutions first_label internal)
    <$> substitute common_substitutions intro_story

data NoSuchTransitionException = ∀ label. Show label ⇒ NoSuchTransitionException label
instance Show NoSuchTransitionException where
  show (NoSuchTransitionException label) = [i|NoSuchTransitionException (#{show label})|]
instance Exception NoSuchTransitionException where
  displayException (NoSuchTransitionException label) = [i|No such state label #{label}|]

getStatus ∷ (Show label, Eq label) ⇒ Transitions label → GetStatusQuestRunner (State label s)
getStatus states State{..} =
  case lookup current states of
    Nothing → throwM $ NoSuchTransitionException current
    Just Transition{..} → substitute substitutions status_story

data MissingStory = ∀ label. Show label ⇒ MissingStory label Text
instance Show MissingStory where
  show (MissingStory label category) = [i|MissingStory #{label} "#{category}"|]
instance Exception MissingStory where
  displayException (MissingStory label category) =
    [i|Missing any story of category "#{category}" for state labeled #{label}.|]

uniformOrDie ∷ (MonadRandom m, MonadThrow m, Show label) ⇒ label → Text → [x] → m x
uniformOrDie label name list
  | onull list = throwM $ MissingStory label name
  | otherwise = uniform list

trial ∷ (Eq label, Show label) ⇒ Transitions label → TrialQuestRunner (State label s)
trial transitions result scale = do
  old_state@State{..} ← get
  Transition{..} ← maybe (throwM $ NoSuchTransitionException current) pure $ lookup current transitions
  let sub ∷ MonadThrow m ⇒ SuccessOrFailureResult → Story → m Markdown
      sub result = substitute (substitutions ⊕ mapFromList (extra_subs ^. taggedLensForResult result))

      continueQuest ∷ MonadThrow m ⇒ SuccessOrFailureResult → (StoryOutcomes → Story) → m TryQuestResult
      continueQuest result storyFor = TryQuestResult QuestInProgress <$> (sub result $ storyFor outcomes)

      endQuest ∷ (MonadRandom m, MonadThrow m) ⇒ (StoryOutcomes → Story) → SuccessOrFailureResult → m TryQuestResult
      endQuest storyFor result = do
        let (lens_name, deed_lens_)  = case result of
              SuccessResult → ("fame", story_fame_)
              FailureResult → ("shame", story_shame_)
        text ← uniformOrDie current lens_name (outcomes ^. deed_lens_) >>= sub result
        TryQuestResult (QuestHasEnded result text) <$> (sub result $ storyFor outcomes)

  tryBinomial (1/3) scale >>= bool
    (TryQuestResult QuestInProgress <$> (uniformOrDie current "between" between_stories >>= substitute substitutions))
    (if onull next
      then case result of
        SuccessResult → endQuest storyForSuccess SuccessResult
        FailureResult → uniformAction
          [ endQuest storyForFailure FailureResult
          , endQuest storyForAverted SuccessResult
          ]
      else do
        next_current ← uniform next
        put $ old_state { current = next_current }
        case result of
          SuccessResult → continueQuest SuccessResult storyForSuccess
          FailureResult → uniformAction
            [ endQuest storyForFailure FailureResult
            , continueQuest FailureResult storyForAverted
            ]
    )
