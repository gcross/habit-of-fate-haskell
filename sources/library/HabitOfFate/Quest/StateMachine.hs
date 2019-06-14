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

import Control.Monad.Random.Class (uniform)

import HabitOfFate.Data.Outcomes
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Story
import HabitOfFate.Substitution
import HabitOfFate.TH

data State label s = State
  { substitutions ∷ HashMap Text Gendered
  , current ∷ label
  , internal ∷ s
  } deriving (Eq,Ord,Read,Show)
makeLenses ''State
deriveJSON ''State

data Transition label = Transition
  { outcomes ∷ Outcomes Story
  , between_stories ∷ [Story]
  , status_story ∷ Story
  , next ∷ Tagged [label]
  , extra_subs ∷ Tagged ([(Text, Gendered)])
  , story_fames ∷ [Story]
  } deriving (Eq,Functor,Ord,Read,Show)

type Transitions label = [(label, Transition label)]

initialize ∷ Substitutions → label → s → Story → InitializeQuestRunner (State label s)
initialize common_substitutions first_label internal intro_story = pure $
  InitializeQuestResult
    (State common_substitutions first_label internal)
    (substitute common_substitutions intro_story)

getStatus ∷ (Show label, Eq label) ⇒ Transitions label → GetStatusQuestRunner (State label s)
getStatus states =
  ask >>= \State{..} → pure $
  case lookup current states of
    Nothing → error [i|No such transition exception: #{current}|]
    Just Transition{..} → substitute substitutions status_story

trial ∷ (Eq label, Show label) ⇒ Transitions label → TrialQuestRunner (State label s)
trial transitions result = do
  old_state@State{..} ← get
  let Transition{..} = fromMaybe (error [i|No such transition exception: #{current}|]) $ lookup current transitions

      sub result = substitute (substitutions ⊕ mapFromList (extra_subs ^. taggedLensForResult result))

      continueQuest result storyFor = pure $ TryQuestResult QuestInProgress (sub result $ storyFor outcomes)

      endQuest storyFor result = do
        let deeds = case result of
              SuccessResult → story_fames
              FailureResult → outcomes & outcomes_shames
        text ← chooseFrom deeds <&> sub result
        pure $ TryQuestResult (QuestHasEnded result text) (sub result $ storyFor outcomes)

  tryProgress
    (TryQuestResult QuestInProgress <$> chooseFromAndSubstitute between_stories substitutions)
    (let next_for_result = next ^. taggedLensForResult result
     in if onull next_for_result
      then case result of
        SuccessResult → endQuest storyForSuccess SuccessResult
        FailureResult → tryAverted $ Tagged
          (Success $ endQuest storyForAverted SuccessResult)
          (Failure $ endQuest storyForFailure FailureResult)
      else do
        next_current ← chooseFrom next_for_result
        put $ old_state { current = next_current }
        case result of
          SuccessResult → continueQuest SuccessResult storyForSuccess
          FailureResult → tryAverted $ Tagged
            (Success $ continueQuest FailureResult storyForAverted)
            (Failure $ endQuest storyForFailure FailureResult)
    )
