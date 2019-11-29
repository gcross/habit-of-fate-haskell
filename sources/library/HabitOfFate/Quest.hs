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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM),Exception)
import Data.List (head)
import Data.Traversable (Traversable(traverse))

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

event_outcomes_ ∷ Lens (Entry c1) (Entry c2) (Outcomes c1) (Outcomes c2)
event_outcomes_ = lens event_outcomes (\x y → x{event_outcomes = y})

narrative_content_ ∷ Lens (Entry c1) (Entry c2) (Narrative c1) (Narrative c2)
narrative_content_ = lens narrative_content (\x y → x{narrative_content = y})

line_contents_ ∷ Lens (Entry c1) (Entry c2) [Entry c1] [Entry c2]
line_contents_ = lens line_contents (\x y → x{line_contents = y})

branch_entry_ ∷ Lens (Branch c1) (Branch c2) (Entry c1) (Entry c2)
branch_entry_ = lens branch_entry (\x y → x{branch_entry = y})

fames_content_ ∷ Lens (Entry c1) (Entry c2) [c1] [c2]
fames_content_ = lens fames_content (\x y → x{fames_content = y})

random_stories_content_ ∷ Lens (Entry c1) (Entry c2) [c1] [c2]
random_stories_content_ = lens random_stories_content (\x y → x{random_stories_content = y})

status_content_ ∷ Lens (Entry c1) (Entry c2) c1 c2
status_content_ = lens status_content (\x y → x{status_content = y})

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
  , branch_substitutions ∷ [Substitution]
  , branch_entry ∷ Entry content
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data Substitution =
    SP { substitution_placeholder ∷ Text
       , substitution_names_and_genders ∷ [(Text,Gender)]
       }
  | S  { substitution_placeholder ∷ Text
       , substitution_names_and_genders ∷ [(Text,Gender)]
       }
  deriving (Eq,Ord,Read,Show)

isPrimarySubstitution ∷ Substitution → Bool
isPrimarySubstitution (SP{}) = True
isPrimarySubstitution (S{}) = False

defaultSubstitutionsFor ∷ [Substitution] → Substitutions
defaultSubstitutionsFor =
  concatMap (\s →
    let (name,gender) = s & substitution_names_and_genders & head
    in (s & substitution_placeholder,Gendered name gender)
       :
       if isPrimarySubstitution s
       then [("", Gendered name gender)]
       else []
  )
  >>>
  mapFromList

randomSubstitutionsFor ∷
  Applicative m ⇒
  ([(Text,Gender)] → m (Text,Gender)) →
  [Substitution] →
  m Substitutions
randomSubstitutionsFor select =
  traverse (\s →
    select (s & substitution_names_and_genders)
    <&>
    \(name,gender) →
      (s & substitution_placeholder, Gendered name gender)
      :
      if isPrimarySubstitution s
      then [("", Gendered name gender)]
      else []
  )
  >>>
  fmap (concat >>> mapFromList)

data Quest = Quest
  { quest_name ∷ Text
  , quest_choice ∷ Markdown
  , quest_substitutions ∷ [Substitution]
  , quest_entry ∷ Entry Story
  } deriving (Eq,Ord,Read,Show)

substituteEntry ∷ MonadThrow m ⇒ Substitutions → Entry Story → m (Entry Markdown)
substituteEntry subs = \case
  x@EventEntry{} →
    traverseOf (event_outcomes_ . traversed) subst x
  x@NarrativeEntry{} →
    traverseOf (narrative_content_ . traversed) subst x
  x@LineEntry{} →
    traverseOf (line_contents_ . traversed) (substituteEntry subs) x
  x@SplitEntry{..} → do
    new_story ← traverse subst split_story
    new_branches ←
      traverse
        (\y@Branch{..} →
          traverseOf branch_entry_ (substituteEntry $ subs ⊕ defaultSubstitutionsFor branch_substitutions) y
        )
        split_branches
    pure x{split_story=new_story,split_branches=new_branches}
  x@FamesEntry{} →
    traverseOf (fames_content_ . traversed) subst x
  x@RandomStoriesEntry{} →
    traverseOf (random_stories_content_ . traversed) subst x
  x@StatusEntry{} →
    traverseOf status_content_ subst x
  where
  subst = substitute subs

initialQuestPath ∷ MonadThrow m ⇒ Quest → m Text
initialQuestPath Quest{..} = ((quest_name ⊕ "/") ⊕) <$> nextPathOf quest_entry
