{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.RunGame (runGame) where

import HabitOfFate.Prelude

import Control.Monad.Random (uniform, weighted)

import HabitOfFate.Data.Account
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Data.QuestState
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quests
import HabitOfFate.Server.Transaction
import HabitOfFate.Story
import HabitOfFate.Trial

data NextIs = NextIsStuck | NextIsNarrative | NextIsEvent deriving (Eq, Show)

type QuestStateT = StateT (QuestState Markdown) Transaction

runGame ∷ Transaction (Markdown, Bool)
runGame = do
  let resetQuestState ∷ QuestStateT ()
      resetQuestState = do
        quest_state ← randomQuestState
        interlude ← uniform interludes
        put (quest_state & quest_state_remaining_content_ %~ (NarrativeContent interlude:))

      registerDeedAndResetQuestState ∷ SuccessOrFailureResult → [Markdown] → QuestStateT ()
      registerDeedAndResetQuestState kind contents = do
        lift $ do
          deed ← Deed kind <$> uniform contents <*> getCurrentTimeAsLocalTime
          deeds_ %= (deed:)
        resetQuestState

      walkFirst ∷ QuestStateT (Markdown, NextIs)
      walkFirst =
        use quest_state_remaining_content_ >>= \case
        [] → do
          use quest_state_fames_ >>= registerDeedAndResetQuestState SuccessResult
          walkFirst
        (RandomStoriesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_random_stories_ .= c
          walkFirst
        (StatusContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_status_ .= c
          walkFirst
        (FamesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_fames_ .= c
          walkFirst
        (NarrativeContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          (c, ) <$> walkSecond
        (EventContent outcomes:rest_content) → do
          let randomStory ∷ QuestStateT (Markdown, NextIs)
              randomStory = (use quest_state_random_stories_ >>= uniform) <&> (, NextIsStuck)

              randomOutcome ∷ SuccessOrFailureResult → Scale → QuestStateT (Markdown, NextIs)
              randomOutcome result scale = tryBinomial (1/3) scale >>= bool
                randomStory
                (case result of
                  SuccessResult → advance $ storyForSuccess outcomes
                  FailureResult →
                    let failure = do
                          registerDeedAndResetQuestState FailureResult (outcomes & outcomes_shames)
                          pure (storyForFailure outcomes, NextIsNarrative)
                        tryAverted = weighted [(SuccessResult, 1), (FailureResult, 2)] >>= \case
                          SuccessResult → advance $ storyForAverted outcomes
                          FailureResult → failure
                    in case outcomes of
                      SuccessFailure{..} → failure
                      SuccessAvertedFailure{..} → tryAverted
                      SuccessDangerAvertedFailure{..} → tryAverted
                )
               where
                advance story = do
                  quest_state_remaining_content_ .= rest_content
                  (story, ) <$> walkSecond

          marks ← lift $ use marks_
          case (uncons (marks ^. success_), uncons (marks ^. failure_)) of
            (Just (scale, rest), _) → do
              lift $ marks_ . success_ .= rest
              randomOutcome SuccessResult scale
            (_, Just (scale, rest)) → do
              lift $ marks_ . failure_ .= rest
              randomOutcome FailureResult scale
            _ → randomStory

      walkSecond ∷ QuestStateT NextIs
      walkSecond =
        use quest_state_remaining_content_ >>= \case
        [] → do
          use quest_state_fames_ >>= registerDeedAndResetQuestState SuccessResult
          pure NextIsNarrative
        (RandomStoriesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_random_stories_ .= c
          walkSecond
        (StatusContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_status_ .= c
          walkSecond
        (FamesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_fames_ .= c
          walkSecond
        (NarrativeContent _:_) → pure NextIsNarrative
        (EventContent _:_) → pure NextIsEvent

  ((content, next_is), new_quest_state) ← use quest_state_ >>= runStateT walkFirst
  quest_state_ .= new_quest_state

  display_next_button ←
    case next_is of
      NextIsNarrative → pure True
      _ → marksArePresent

  pure (content, display_next_button)
