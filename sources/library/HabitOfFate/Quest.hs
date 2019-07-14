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

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM),Exception)
import Data.Traversable (Traversable(traverse))
import Data.Vector (Vector)

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Story
import HabitOfFate.Substitution

data ShuffleMode = Shuffle | NoShuffle deriving (Enum,Eq,Ord,Read,Show)

data Entry content =
    EventEntry
      { event_name ∷ Text
      , event_outcomes ∷ Outcomes content
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
  | FamesEntry
      { fames_content ∷ [content]
      }
  | RandomStoriesEntry
      { random_stories_content ∷ [content]
      }
  | StatusEntry
      { status_content ∷ content
      }
 deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data DeadEnd = DeadEnd Text deriving (Eq,Show)
instance Exception DeadEnd where

nextPathOf ∷ MonadThrow m ⇒ Entry content → m Text
nextPathOf EventEntry{..} = pure $ event_name ⊕ "/common"
nextPathOf NarrativeEntry{..} = pure $ narrative_name
nextPathOf LineEntry{..} = go line_contents
 where
  go (FamesEntry{..}:rest) = go rest
  go (RandomStoriesEntry{..}:rest) = go rest
  go (StatusEntry{..}:rest) = go rest
  go [] = throwM $ DeadEnd "empty"
  go (entry:_) = ((line_name ⊕ "/") ⊕) <$> nextPathOf entry
nextPathOf SplitEntry{..} = pure $ split_name ⊕ "/common"
nextPathOf FamesEntry{..} = throwM $ DeadEnd "fames"
nextPathOf RandomStoriesEntry{..} = throwM $ DeadEnd "random stories"
nextPathOf StatusEntry{..} = throwM $ DeadEnd "status"

data Branch content = Branch
  { branch_choice ∷ Markdown
  , branch_entry ∷ Entry content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data NameList = MaleList | FemaleList | NeuterList (Vector Text) deriving (Eq,Ord,Read,Show)

data QuestSubstitution = QS
  { quest_substitution_list ∷ NameList
  , quest_substitution_name ∷ Text
  , quest_substitution_default ∷ Text
  } deriving (Eq,Ord,Read,Show)

data Quest = Quest
  { quest_name ∷ Text
  , quest_choice ∷ Markdown
  , quest_substitutions ∷ [QuestSubstitution]
  , quest_entry ∷ Entry Story
  } deriving (Eq,Ord,Read,Show)

defaultQuestSubstitutions ∷ Quest → Substitutions
defaultQuestSubstitutions Quest{..} =
  [ ( quest_substitution_name
    , Gendered quest_substitution_default $
        case quest_substitution_list of
          MaleList → Male
          FemaleList → Female
          NeuterList _ → Neuter
    )
  | QS{..} ← quest_substitutions
  ] |> mapFromList

questEntryWithDefaultSubstitutions ∷ MonadThrow m ⇒ Quest → m (Entry Markdown)
questEntryWithDefaultSubstitutions quest@Quest{..} =
  traverse (substitute (defaultQuestSubstitutions quest)) quest_entry

initialQuestPath ∷ MonadThrow m ⇒ Quest → m Text
initialQuestPath Quest{..} = ((quest_name ⊕ "/") ⊕) <$> nextPathOf quest_entry
