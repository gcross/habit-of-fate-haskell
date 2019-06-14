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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.StoryLine where

import HabitOfFate.Prelude

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Story
import HabitOfFate.Substitution

data ShuffleMode = Shuffle | NoShuffle deriving (Enum,Eq,Ord,Read,Show)

data Entry content =
    EventEntry
      { event_name ∷ Text
      , event_outcomes ∷ Outcomes content
      , event_random_stories ∷ [content]
      }
  | NarrativeEntry
      { narrative_name ∷ Text
      , narrative_content ∷ Narrative content
      }
  | LineEntry
      { line_shuffle_mode ∷ ShuffleMode
      , line_name ∷ Text
      , line_contents ∷ [Entry content]
      }
  | SplitEntry
      { split_name ∷ Text
      , split_story ∷ Narrative content
      , split_question ∷ Markdown
      , split_branches ∷ [Branch content]
      }
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

nameOf ∷ Entry content → Text
nameOf EventEntry{..} = event_name
nameOf NarrativeEntry{..} = narrative_name
nameOf LineEntry{..} = line_name
nameOf SplitEntry{..} = split_name

nextPathOf ∷ Entry content → Text
nextPathOf EventEntry{..} = event_name ⊕ "/common"
nextPathOf NarrativeEntry{..} = narrative_name
nextPathOf LineEntry{..} = case line_contents of
  [] → error "Empty line has no next name."
  entry:_ → line_name ⊕ "/" ⊕ nextPathOf entry
nextPathOf SplitEntry{..} = split_name ⊕ "/common"

data Branch content = Branch
  { branch_choice ∷ Markdown
  , branch_fames ∷ [content]
  , branch_entry ∷ Entry content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data Quest content = Quest
  { quest_name ∷ Text
  , quest_choice ∷ Markdown
  , quest_fames ∷ [content]
  , quest_standard_substitutions ∷ Substitutions
  , quest_entry ∷ Entry content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

substituteQuest ∷ Substitutions → Quest Story → Quest Markdown
substituteQuest subs = fmap (substitute subs)

substituteQuestWithStandardSubstitutions ∷ Quest Story → Quest Markdown
substituteQuestWithStandardSubstitutions quest@Quest{..} = substituteQuest quest_standard_substitutions quest

initialQuestPath ∷ Quest content → Text
initialQuestPath Quest{..} = quest_name ⊕ "/" ⊕ nextPathOf quest_entry
