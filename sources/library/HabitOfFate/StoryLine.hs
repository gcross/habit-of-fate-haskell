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
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.StoryLine where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow)
import Data.Traversable (Traversable(traverse))

import HabitOfFate.Data.Markdown
import HabitOfFate.Story
import HabitOfFate.Substitution

data ShuffleMode = Shuffle | NoShuffle

data StoryEntry content =
    StoryEvent
      { event_name ∷ Text
      , event_outcomes ∷ Outcomes content
      , event_random_stories ∷ [content]
      }
  | StoryNarrative
      { narrative_name ∷ Text
      , narrative_story ∷ Narrative content
      }
  | StoryLine
      { story_line_shuffle_mode ∷ ShuffleMode
      , story_line_name ∷ Text
      , story_line_contents ∷ [StoryEntry content]
      }
  | StorySplit
      { split_name ∷ Text
      , split_story ∷ Narrative content
      , split_question ∷ Text
      , split_branches ∷ [StoryBranch content]
      }
  | StoryFames
      { story_fames ∷ [content]
      }
 deriving (Foldable,Functor,Traversable)

data StoryBranch content = StoryBranch
  { branch_text ∷ Text
  , branch_story_entry ∷ StoryEntry content
  } deriving (Foldable,Functor,Traversable)

data QuestStory content = QuestStory
  { quest_name ∷ Text
  , quest_entry ∷ StoryEntry content
  } deriving (Foldable,Functor,Traversable)

substituteQuestStory ∷ MonadThrow m ⇒ Substitutions → QuestStory Story → m (QuestStory Markdown)
substituteQuestStory subs = traverse (substitute subs)
