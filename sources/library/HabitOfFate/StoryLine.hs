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

import Control.Monad.Catch (MonadThrow)
import Data.Traversable (Traversable(traverse))

import HabitOfFate.Data.Markdown
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
nextPathOf entry@LineEntry{..} = case line_contents of
  [] → error "Empty line has no next name."
  entry:_ → line_name ⊕ "/" ⊕ nextPathOf entry
nextPathOf entry@SplitEntry{..} = split_name ⊕ "/common"

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

substituteQuest ∷ MonadThrow m ⇒ Substitutions → Quest Story → m (Quest Markdown)
substituteQuest subs = traverse (substitute subs)

substituteQuestWithStandardSubstitutions ∷ MonadThrow m ⇒ Quest Story → m (Quest Markdown)
substituteQuestWithStandardSubstitutions quest@Quest{..} = substituteQuest quest_standard_substitutions quest

initialQuestPath ∷ Quest content → Text
initialQuestPath Quest{..} = quest_name ⊕ "/" ⊕ nextPathOf quest_entry

data Page = Page
  { page_path ∷ Text
  , page_title ∷ Markdown
  , page_content ∷ Markdown
  , page_choices ∷ PageChoices Markdown
  } deriving (Eq,Ord,Read,Show)

nextPageChoice ∷ Maybe Text → PageChoices content
nextPageChoice Nothing = DeadEnd
nextPageChoice (Just next) = NoChoice next

buildPagesFromQuest ∷ Quest Markdown → [Page]
buildPagesFromQuest Quest{..} = process Nothing quest_name quest_entry
 where
  process ∷ Maybe Text → Text → Entry Markdown → [Page]
  process maybe_next_choice parent node = case node of
    EventEntry{..} →
      let base = parent ⊕ "/" ⊕ event_name ⊕ "/"
      in case event_outcomes of
        SuccessFailure{..} →
          [Page
            { page_path = base ⊕ "common"
            , page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice, base ⊕ "success")
                ,(outcomes_failure_choice, base ⊕ "failure")
                ]
            }
          ,Page
            { page_path = base ⊕ "success"
            , page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            }
          ,Page
            { page_path = base ⊕ "failure"
            , page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            }
          ]
        SuccessAvertedFailure{..} →
          [Page
            { page_path = base ⊕ "common"
            , page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice, base ⊕ "success")
                ,(outcomes_averted_choice, base ⊕ "averted")
                ,(outcomes_failure_choice, base ⊕ "failure")
                ]
            }
          ,Page
            { page_path = base ⊕ "success"
            , page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            }
          ,Page
            { page_path = base ⊕ "averted"
            , page_title = outcomes_averted_title
            , page_content = outcomes_averted_story
            , page_choices = nextPageChoice maybe_next_choice
            }
          ,Page
            { page_path = base ⊕ "failure"
            , page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            }
          ]
        SuccessDangerAvertedFailure{..} →
          [Page
            { page_path = base ⊕ "common"
            , page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice, base ⊕ "success")
                ,(outcomes_danger_choice, base ⊕ "danger")
                ]
            }
          ,Page
            { page_path = base ⊕ "success"
            , page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            }
          ,Page
            { page_path = base ⊕ "danger"
            , page_title = outcomes_danger_title
            , page_content = outcomes_danger_story
            , page_choices = Choices outcomes_danger_question
                [(outcomes_averted_choice,base ⊕ "averted")
                ,(outcomes_failure_choice,base ⊕ "failure")
                ]
            }
          ,Page
            { page_path = base ⊕ "averted"
            , page_title = outcomes_averted_title
            , page_content = outcomes_averted_story
            , page_choices = nextPageChoice maybe_next_choice
            }
          ,Page
            { page_path = base ⊕ "failure"
            , page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            }
          ]
    NarrativeEntry{..} →
      [let Narrative{..} = narrative_content
       in Page
        { page_path = parent ⊕ "/" ⊕ narrative_name
        , page_title = narrative_title
        , page_content = narrative_story
        , page_choices = nextPageChoice maybe_next_choice
        }
      ]
    LineEntry{..} →
      let base = parent ⊕ "/" ⊕ line_name
          go ∷ Entry Markdown → [Entry Markdown] → [[Page]]
          go x [] = process maybe_next_choice base x:[]
          go x (y:rest) = process (Just $ nameOf y) base x:go y rest
      in concat $ case line_contents of
        [] → []
        x:rest → go x rest
    SplitEntry{..} →
      let name = parent ⊕ "/" ⊕ split_name
          base = name ⊕ "/"
          Narrative{..} = split_story
      in
        [Page
          { page_path = base ⊕ "common"
          , page_title = narrative_title
          , page_content = narrative_story
          , page_choices = Choices split_question $
              map
                (\Branch{..} → (branch_choice, base ⊕ nextPathOf branch_entry))
                split_branches
          }
        ]
        ⊕
        concatMap ((& branch_entry) >>> process maybe_next_choice name) split_branches
