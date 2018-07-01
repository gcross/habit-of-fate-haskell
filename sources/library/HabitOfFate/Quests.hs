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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests where

import HabitOfFate.Prelude

import Control.Monad.Random

import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Quest
import HabitOfFate.Story
import HabitOfFate.Tag
import HabitOfFate.TH

data CurrentQuestState =
    Forest Forest.State
  deriving (Eq,Ord,Read,Show)
deriveJSON ''CurrentQuestState
makePrisms ''CurrentQuestState

data Quest s = Quest
  { _quest_prism_ ∷ Prism' CurrentQuestState s
  , _start_quest_ ∷ InitialQuestRunner s
  , _progress_to_milestones_ ∷ Tagged (ProgressToMilestoneQuestRunner s)
  , _attained_milestones_ ∷ Tagged (AttainedMilestoneQuestRunner s)
  }

start_quest_ ∷ Lens' (Quest s) (InitialQuestRunner s)
start_quest_ = lens _start_quest_ (\old new → old { _start_quest_ = new })

progress_to_milestones_ ∷ Lens' (Quest s) (Tagged (ProgressToMilestoneQuestRunner s))
progress_to_milestones_ = lens _progress_to_milestones_ (\old new → old { _progress_to_milestones_ = new })

attained_milestones_ ∷ Lens' (Quest s) (Tagged (AttainedMilestoneQuestRunner s))
attained_milestones_ = lens _attained_milestones_ (\old new → old { _attained_milestones_ = new })

data WrappedQuest = ∀ s. WrappedQuest (Quest s)

quests ∷ [WrappedQuest]
quests =
  [WrappedQuest $
     Quest
       _Forest
       Forest.start
       (Tagged
         (Success (Forest.runProgressToSuccessMilestone))
         (Failure Forest.runProgressToFailureMilestone)
       )
       (Tagged
         (Success Forest.runAttainedSuccessMilestone)
         (Failure Forest.runAttainedFailureMilestone)
       )
  ]

type RunCurrentQuestResult = RunQuestResult CurrentQuestState

runCurrentQuest ∷ CurrentQuestState → (∀ s. Quest s → s → α) → α
runCurrentQuest current_quest_state f =
  foldr
    (\(WrappedQuest (quest@(Quest quest_prism _ _ _))) rest →
      case current_quest_state ^? quest_prism of
        Nothing → rest
        Just quest_state → f quest quest_state
    )
    (error "Unrecognized quest type")
    quests
