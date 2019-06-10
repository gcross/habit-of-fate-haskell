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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Paths where

import HabitOfFate.Prelude

import Control.Monad.Random.Class (MonadRandom, uniform)
import System.Random.Shuffle (shuffleM)

import HabitOfFate.Data.Outcomes
import HabitOfFate.StoryLine
import HabitOfFate.Story

data Step content =
    EventStep
      { step_outcomes ∷ Outcomes content
      , step_random_stories ∷ [content]
      }
  | NarrativeStep
      { step_content ∷ content
      }
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data QuestPath content = QuestPath
  { quest_path_name ∷ Text
  , quest_path_steps ∷ [Step content]
  , quest_path_fames ∷ [content]
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

generatePath ∷ ∀ m content. Monad m ⇒ (∀ α. [α] → m α) → (∀ α. [α] → m [α]) → Quest content → m (Text, QuestPath content)
generatePath select shuffle Quest{..} = do
  (name, steps, fames) ← foldlM folder (quest_name, mempty, mempty) [quest_entry]
  pure (name, QuestPath quest_name (toList steps) fames)
 where
  folder ∷ Monad m ⇒ (Text, Seq (Step content), [content]) → Entry content → m (Text, Seq (Step content), [content])
  folder (parent_name, steps, fames) entry = case entry of
    EventEntry{..} → pure
      ( parent_name ⊕ "/" ⊕ event_name
      , steps `snoc` EventStep event_outcomes event_random_stories
      , fames
      )
    NarrativeEntry{..} → pure
      ( parent_name ⊕ "/" ⊕ narrative_name
      , steps `snoc` NarrativeStep (narrative_content & narrative_story)
      , fames
      )
    LineEntry{..} →
      (
        case line_shuffle_mode of
          NoShuffle → pure
          Shuffle → shuffle
        $
        (line_contents ∷ [Entry content])
        ∷ m [Entry content]
      )
      >>=
      (foldlM folder (parent_name ⊕ "/" ⊕ line_name, steps, fames) ∷ [Entry content] → m (Text, Seq (Step content), [content]))
    SplitEntry{..} → do
      Branch{..} ← select split_branches
      folder (parent_name, steps, fames ⊕ branch_fames) branch_entry

allPaths ∷ Quest content → [(Text, QuestPath content)]
allPaths = generatePath identity pure

randomQuestPath ∷ MonadRandom m ⇒ Quest content → m (QuestPath content)
randomQuestPath quest = generatePath uniform shuffleM quest <&> snd
