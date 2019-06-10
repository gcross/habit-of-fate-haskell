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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.QuestState where

import HabitOfFate.Prelude

import Control.Monad.Random.Class (MonadRandom, uniform)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , withObject
  )
import System.Random.Shuffle (shuffleM)

import HabitOfFate.Data.Outcomes
import HabitOfFate.JSON
import HabitOfFate.StoryLine
import HabitOfFate.Story

data Content content =
    EventContent
      { event_content_outcomes ∷ Outcomes content
      , event_content_random_stories ∷ [content]
      }
  | NarrativeContent
      { narrative_content ∷ content
      }
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

instance ToJSON content ⇒ ToJSON (Content content) where
  toJSON content = runJSONBuilder $ do
    case content of
      EventContent{..} → do
        writeField "kind" ("event" ∷ Text)
        writeField "outcomes" event_content_outcomes
        writeField "random stories" event_content_random_stories
      NarrativeContent{..} → do
        writeField "kind" ("narrative" ∷ Text)
        writeField "content" narrative_content

instance FromJSON content ⇒ FromJSON (Content content) where
  parseJSON = withObject "account must be object-shaped" $ \o → do
    kind ∷ Text ← o .: "kind"
    case kind of
      "event" → EventContent <$> (o .: "outcomes") <*> (o .: "random stories")
      "narrative" → NarrativeContent <$> (o .: "content")
      _ → fail [i|Content kind must be event or narrative, not #{kind}|]

data QuestState content = QuestState
  { quest_state_name ∷ Text
  , quest_state_remaining_content ∷ [Content content]
  , quest_state_fames ∷ [content]
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

generateQuestState ∷ ∀ m content. Monad m ⇒ (∀ α. [α] → m α) → (∀ α. [α] → m [α]) → Quest content → m (Text, QuestState content)
generateQuestState select shuffle Quest{..} = do
  (name, remaining_content, fames) ← foldlM folder (quest_name, mempty, mempty) [quest_entry]
  pure (name, QuestState quest_name (toList remaining_content) fames)
 where
  folder ∷ Monad m ⇒ (Text, Seq (Content content), [content]) → Entry content → m (Text, Seq (Content content), [content])
  folder (parent_name, remaining_content, fames) entry = case entry of
    EventEntry{..} → pure
      ( parent_name ⊕ "/" ⊕ event_name
      , remaining_content `snoc` EventContent event_outcomes event_random_stories
      , fames
      )
    NarrativeEntry{..} → pure
      ( parent_name ⊕ "/" ⊕ narrative_name
      , remaining_content `snoc` NarrativeContent (narrative_content & narrative_story)
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
      (foldlM folder (parent_name ⊕ "/" ⊕ line_name, remaining_content, fames)
        ∷ [Entry content] → m (Text, Seq (Content content), [content]))
    SplitEntry{..} → do
      Branch{..} ← select split_branches
      folder (parent_name, remaining_content, fames ⊕ branch_fames) branch_entry

allQuestStates ∷ Quest content → [(Text, QuestState content)]
allQuestStates = generateQuestState identity pure

randomQuestState ∷ MonadRandom m ⇒ Quest content → m (QuestState content)
randomQuestState quest = generateQuestState uniform shuffleM quest <&> snd
