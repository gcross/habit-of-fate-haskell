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

import Control.Monad.Catch (MonadThrow(throwM), Exception)
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
      }
  | NarrativeContent
      { narrative_content ∷ content
      }
  | RandomStoriesContent
      { random_stories_content ∷ [content]
      }
  | StatusContent
      { status_content ∷ content
      }
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

instance ToJSON content ⇒ ToJSON (Content content) where
  toJSON content = runJSONBuilder $ do
    case content of
      EventContent{..} → do
        writeField "kind" ("event" ∷ Text)
        writeField "outcomes" event_content_outcomes
      NarrativeContent{..} → do
        writeField "kind" ("narrative" ∷ Text)
        writeField "content" narrative_content
      RandomStoriesContent{..} → do
        writeField "kind" ("random stories" ∷ Text)
        writeField "content" random_stories_content
      StatusContent{..} → do
        writeField "kind" ("status" ∷ Text)
        writeField "content" status_content

instance FromJSON content ⇒ FromJSON (Content content) where
  parseJSON = withObject "account must be object-shaped" $ \o → do
    kind ∷ Text ← o .: "kind"
    case kind of
      "event" → EventContent <$> (o .: "outcomes")
      "narrative" → NarrativeContent <$> (o .: "content")
      "random stories" → RandomStoriesContent <$> (o .: "content")
      "status" → NarrativeContent <$> (o .: "content")
      _ → fail [i|Content kind must be event, narrative, or ransoms, not #{kind}|]

data QuestState content = QuestState
  { quest_state_name ∷ Text
  , quest_state_remaining_content ∷ [Content content]
  , quest_state_fames ∷ [content]
  , quest_state_initial_random_stories ∷ [content]
  , quest_state_status ∷ content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data FolderState content = FolderState
  { _name_ ∷ Text
  , _remaining_content_ ∷ Seq (Content content)
  , _maybe_fames_ ∷ Maybe [content]
  }
makeLenses ''FolderState

data FamesError = DuplicateFames Text | NoFames deriving (Eq,Show)
instance Exception FamesError where

generateQuestState ∷ ∀ m content. MonadThrow m ⇒ (∀ α. [α] → m α) → (∀ α. [α] → m [α]) → Quest content → m (Text, QuestState content)
generateQuestState select shuffle Quest{..} = do
  let initial_folder_state = FolderState
        { _name_ = quest_name
        , _remaining_content_ = mempty
        , _maybe_fames_ = Nothing
        }
  FolderState{..} ← foldlM folder initial_folder_state [quest_entry]
  maybe
    (throwM NoFames)
    (\fames → pure (_name_, QuestState quest_name (toList _remaining_content_) fames quest_initial_random_stories quest_initial_status))
    _maybe_fames_
 where
  folder folder_state entry = case entry of
    EventEntry{..} → pure ( folder_state
      & name_ ⊕~ ("/" ⊕ event_name)
      & remaining_content_ %~ (`snoc` EventContent event_outcomes))
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
    SplitEntry{..} → select split_branches >>= \Branch{..} → folder folder_state branch_entry
    FamesEntry{..} → case folder_state ^. maybe_fames_ of
      Nothing → pure (folder_state & maybe_fames_ .~ Just fames_content)
      Just _ → throwM $ DuplicateFames (folder_state ^. name_)
    RandomStoriesEntry{..} → pure ( folder_state
      & remaining_content_ %~ (`snoc` RandomStoriesContent random_stories_content))
    StatusEntry{..} → pure ( folder_state
      & remaining_content_ %~ (`snoc` StatusContent status_content))

instance MonadThrow m ⇒ MonadThrow (LogicT m) where
  throwM = throwM >>> lift

allQuestStates ∷ MonadThrow m ⇒ Quest content → m [(Text, QuestState content)]
allQuestStates = generateQuestState (map pure >>> asum) pure >>> observeAllT

randomQuestState ∷ (MonadRandom m, MonadThrow m) ⇒ Quest content → m (QuestState content)
randomQuestState quest = generateQuestState uniform shuffleM quest <&> snd
