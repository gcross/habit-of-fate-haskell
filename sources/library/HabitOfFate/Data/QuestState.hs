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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.QuestState where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.Logic (LogicT, observeAllT)
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

data FolderState content = FolderState
  { _name_ ∷ Text
  , _remaining_content_ ∷ Seq (Content content)
  , _fames_ ∷ [content]
  }
makeLenses ''FolderState

generateQuestState ∷ ∀ m content. MonadThrow m ⇒ (∀ α. [α] → m α) → (∀ α. [α] → m [α]) → Quest content → m (Text, QuestState content)
generateQuestState select shuffle Quest{..} = do
  let initial_folder_state = FolderState
        { _name_ = quest_name
        , _remaining_content_ = mempty
        , _fames_ = quest_fames
        }
  FolderState{..} ← foldlM folder initial_folder_state [quest_entry]
  pure (_name_, QuestState quest_name (toList _remaining_content_) _fames_)
 where
  folder folder_state entry = case entry of
    EventEntry{..} → pure ( folder_state
      & name_ ⊕~ ("/" ⊕ event_name)
      & remaining_content_ %~ (`snoc` EventContent event_outcomes event_random_stories))
    NarrativeEntry{..} → pure ( folder_state
      & name_ ⊕~ ("/" ⊕ narrative_name)
      & remaining_content_ %~ (`snoc` NarrativeContent (narrative_content & narrative_story)))
    LineEntry{..} →
      (
        case line_shuffle_mode of
          NoShuffle → pure
          Shuffle → shuffle
        $
        line_contents
      )
      >>=
      foldlM folder (folder_state & name_ ⊕~ ("/" ⊕ line_name))
    SplitEntry{..} → do
      Branch{..} ← select split_branches
      folder (folder_state & fames_ ⊕~ branch_fames) branch_entry

instance MonadThrow m ⇒ MonadThrow (LogicT m) where
  throwM = throwM >>> lift

allQuestStates ∷ MonadThrow m ⇒ Quest content → m [(Text, QuestState content)]
allQuestStates = generateQuestState (map pure >>> asum) pure >>> observeAllT

randomQuestState ∷ (MonadRandom m, MonadThrow m) ⇒ Quest content → m (QuestState content)
randomQuestState quest = generateQuestState uniform shuffleM quest <&> snd
