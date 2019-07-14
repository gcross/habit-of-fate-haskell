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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.QuestState where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM), Exception)
import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.Random.Class (MonadRandom(getRandomR), uniform)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , withObject
  )
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random.Shuffle (shuffleM)

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.JSON
import HabitOfFate.Names
import HabitOfFate.Quest
import HabitOfFate.Story
import HabitOfFate.Substitution

data Content content =
    EventContent (Outcomes content)
  | NarrativeContent content
  | RandomStoriesContent [content]
  | StatusContent content
  | FamesContent [content]
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

instance ToJSON content ⇒ ToJSON (Content content) where
  toJSON content = runJSONBuilder $ case content of
    EventContent c → writeContent "event" c
    NarrativeContent c → writeContent "narrative" c
    RandomStoriesContent c → writeContent "random stories" c
    StatusContent c → writeContent "status" c
    FamesContent c → writeContent "fames" c
   where
    writeContent ∷ ToJSON c ⇒ Text → c → JSONBuilder ()
    writeContent name c = do
      writeField "kind" name
      writeField "content" c

instance FromJSON content ⇒ FromJSON (Content content) where
  parseJSON = withObject "account must be object-shaped" $ \o → do
    kind ∷ Text ← o .: "kind"
    case kind of
      "event" → EventContent <$> (o .: "content")
      "narrative" → NarrativeContent <$> (o .: "content")
      "random stories" → RandomStoriesContent <$> (o .: "content")
      "status" → StatusContent <$> (o .: "content")
      "fames" → FamesContent <$> (o .: "content")
      _ → fail [i|Content kind must be event, narrative, ransoms, or fames, not #{kind}|]

data QuestState content = QuestState
  { _quest_state_name_ ∷ Text
  , _quest_state_fames_ ∷ [content]
  , _quest_state_remaining_content_ ∷ [Content content]
  , _quest_state_random_stories_ ∷ [content]
  , _quest_state_status_ ∷ content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)
makeLenses ''QuestState

instance ToJSON content ⇒ ToJSON (QuestState content) where
  toJSON QuestState{..} = runJSONBuilder $ do
    writeField "name" _quest_state_name_
    writeField "fames" _quest_state_fames_
    writeField "content" _quest_state_remaining_content_
    writeField "stories" _quest_state_random_stories_
    writeField "status" _quest_state_status_

instance FromJSON content ⇒ FromJSON (QuestState content) where
  parseJSON = withObject "quest state must be object-shaped" $ \o →
    QuestState
      <$> (o .: "name")
      <*> (o .: "fames")
      <*> (o .: "content")
      <*> (o .: "stories")
      <*> (o .: "status")

data FolderState content = FolderState
  { _name_ ∷ Text
  , _remaining_content_ ∷ Seq (Content content)
  , _maybe_fames_ ∷ Maybe [content]
  }
makeLenses ''FolderState

data FamesError = DuplicateFames Text | NoFames deriving (Eq,Show)
instance Exception FamesError where

generateQuestState ∷
  ∀ m. MonadThrow m ⇒
  (∀ α. [α] → m α) →
  (∀ α. Vector α → m α) →
  (∀ α. [α] → m [α]) →
  Quest →
  m (Text, QuestState Markdown)
generateQuestState select selectName shuffle Quest{..} = do
  let initial_folder_state = FolderState
        { _name_ = quest_name
        , _remaining_content_ = mempty
        , _maybe_fames_ = Nothing
        }
  FolderState{..} ← foldlM folder initial_folder_state [quest_entry]
  fames ← maybe (throwM NoFames) pure _maybe_fames_
  let pre_substitution_quest_state = QuestState
        quest_name
        fames
        (toList _remaining_content_)
        quest_initial_random_stories
        "You are between quests."
  substitutions ←
    traverse
      (\QS{..} → (quest_substitution_name,) <$> case quest_substitution_list of
        MaleList → flip Gendered Male <$> selectName male_names
        FemaleList → flip Gendered Female <$> selectName female_names
        NeuterList list → flip Gendered Neuter <$> selectName list
      )
      quest_substitutions
    <&>
    mapFromList
  quest_state ← traverse (substitute substitutions) pre_substitution_quest_state
  pure (_name_, quest_state)
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

allQuestStates ∷ MonadThrow m ⇒ Quest → m [(Text, QuestState Markdown)]
allQuestStates = generateQuestState (map pure >>> asum) (V.head >>> pure) pure >>> observeAllT

randomQuestStateFor ∷ (MonadRandom m, MonadThrow m) ⇒ Quest → m (QuestState Markdown)
randomQuestStateFor quest =
  generateQuestState
    uniform
    (\v → getRandomR (0, olength v-1) <&> (v !))
    shuffleM
    quest
  <&>
  snd
