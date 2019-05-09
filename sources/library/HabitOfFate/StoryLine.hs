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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.StoryLine where

import HabitOfFate.Prelude

import HabitOfFate.Data.Tagged
import HabitOfFate.Story
import HabitOfFate.Substitution

data StoryLineOutcome content = StoryLineOutcome
  { outcome_text ∷ content
  , outcome_title ∷ Text
  , outcome_substitutions ∷ Substitutions
  }

data StoryQuestion content = StoryQuestion
  { question_story ∷ content
  , question_outcomes ∷ Tagged (StoryLineOutcome content)
  }

data StoryEntry content =
    StoryEvent
      { event_name ∷ Text
      , event_title ∷ Text
      , event_outcomes ∷ StoryOutcomes content
      , event_initial_question ∷ StoryQuestion content
      , event_failure_question ∷ StoryQuestion content
      , event_random_stories ∷ [content]
      }
  | StoryNarrative
      { narrative_name ∷ Text
      , narrative_title ∷ Text
      , narrative_content ∷ content
      }
  | StoryLine
      { story_line_name ∷ Text
      , story_line_content ∷ [StoryEntry content]
      }
  | StoryShuffle
      { story_lines_to_shuffle ∷ [StoryEntry content]
      }
  | StorySplit
      { split_name ∷ Text
      , split_title ∷ Text
      , split_story ∷ content
      , split_question ∷ Text
      , split_branches ∷ [StoryBranch content]
      }

data StoryBranch content = StoryBranch
  { branch_text ∷ Text
  , branch_story_entry ∷ StoryEntry content
  }


data QuestStory content = QuestStory
  { quest_name ∷ Text
  , quest_entry ∷ StoryEntry content
  }
