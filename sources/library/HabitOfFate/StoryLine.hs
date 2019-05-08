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
  { _outcome_text_ ∷ content
  , _outcome_title_ ∷ Text
  , _outcome_substitutions_ ∷ [(Text, Gendered)]
  }

data StoryQuestion content = StoryQuestion
  { _question_story_ ∷ content
  , _question_outcomes_ ∷ Tagged (StoryLineOutcome content)
  }

data StoryEntry content =
    StoryEvent
      { _event_name_ ∷ Text
      , _event_title_ ∷ Text
      , _event_outcomes_ ∷ StoryOutcomes content
      , _event_initial_question ∷ StoryQuestion content
      , _event_failure_question ∷ StoryQuestion content
      , _event_random_stories ∷ [content]
      }
  | StoryNarrative
      { _narrative_name ∷ Text
      , _narrative_title ∷ Text
      , _narrative_content ∷ content
      }
  | StoryLine
      { _story_line_name ∷ Text
      , _story_line_content ∷ [StoryEntry content]
      }
  | StoryShuffle
      { _story_lines_to_shuffle_ ∷ [StoryEntry content]
      }
  | StorySplit
      { _split_name_ ∷ Text
      , _split_title_ ∷ Text
      , _split_story ∷ content
      , _split_question ∷ Text
      , _split_branches_ ∷ [StoryBranch content]
      }

data StoryBranch content = StoryBranch
  { _branch_text_ ∷ Text
  , _branch_story_entry_ ∷ StoryEntry content
  }

data RandomSubstitution = AnyPerson | AnyMan | AnyWoman | AnyFrom [Gendered]

data QuestStory content = QuestStory
  { _quest_name_ ∷ Text
  , _quest_random_substitutions_ ∷ [(Text, RandomSubstitution)]
  , _quest_entry_ ∷ StoryEntry content
  }
