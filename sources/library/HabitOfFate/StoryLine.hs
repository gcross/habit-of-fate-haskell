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

data ShuffleMode = Shuffle | NoShuffle

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
      { story_line_shuffle_mode ∷ ShuffleMode
      , story_line_name ∷ Text
      , story_line_contents ∷ [Entry content]
      }
  | SplitEntry
      { split_name ∷ Text
      , split_story ∷ Narrative content
      , split_question ∷ Markdown
      , split_branches ∷ [Branch content]
      }
  | FamesEntry
      { story_fames ∷ [content]
      }
 deriving (Foldable,Functor,Traversable)

nameOf ∷ Entry content → Maybe Text
nameOf EventEntry{..} = Just event_name
nameOf NarrativeEntry{..} = Just narrative_name
nameOf LineEntry{..} = Just story_line_name
nameOf SplitEntry{..} = Just split_name
nameOf FamesEntry{..} = Nothing

data Branch content = Branch
  { branch_choice ∷ Markdown
  , branch_entry ∷ Entry content
  } deriving (Foldable,Functor,Traversable)

data Quest content = Quest
  { quest_name ∷ Text
  , quest_entry ∷ Entry content
  } deriving (Foldable,Functor,Traversable)

substituteQuest ∷ MonadThrow m ⇒ Substitutions → Quest Story → m (Quest Markdown)
substituteQuest subs = traverse (substitute subs)

data Page = Page
  { page_title ∷ Markdown
  , page_content ∷ Markdown
  , page_choices ∷ PageChoices Markdown
  }

nextPageChoice ∷ Maybe Text → PageChoices content
nextPageChoice Nothing = DeadEnd
nextPageChoice (Just next) = NoChoice next

buildPagesFromQuest ∷ Quest Markdown → [(Text, Page)]
buildPagesFromQuest Quest{..} = process Nothing quest_name quest_entry
 where
  process ∷ Maybe Text → Text → Entry Markdown → [(Text, Page)]
  process maybe_next_choice parent node = case node of
    EventEntry{..} →
      let base = parent ⊕ "/" ⊕ event_name ⊕ "/"
      in map (first (base ⊕)) $ case event_outcomes of
        SuccessFailure{..} →
          [("common", Page
            { page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("success", Page
            { page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", Page
            { page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            })
          ]
        SuccessAvertedFailure{..} →
          [("common", Page
            { page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_averted_choice,"averted")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("success", Page
            { page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            })
          ,("averted", Page
            { page_title = outcomes_averted_title
            , page_content = outcomes_averted_story
            , page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", Page
            { page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            })
          ]
        SuccessDangerAvertedFailure{..} →
          [("common", Page
            { page_title = outcomes_common_title
            , page_content = outcomes_common_story
            , page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_danger_choice,"danger")
                ]
            })
          ,("success", Page
            { page_title = outcomes_success_title
            , page_content = outcomes_success_story
            , page_choices = nextPageChoice maybe_next_choice
            })
          ,("danger", Page
            { page_title = outcomes_danger_title
            , page_content = outcomes_danger_story
            , page_choices = Choices outcomes_danger_question
                [(outcomes_averted_choice,"averted")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("averted", Page
            { page_title = outcomes_averted_title
            , page_content = outcomes_averted_story
            , page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", Page
            { page_title = outcomes_failure_title
            , page_content = outcomes_failure_story
            , page_choices = DeadEnd
            })
          ]
    NarrativeEntry{..} →
      [(parent ⊕ "/" ⊕ narrative_name
       ,let Narrative{..} = narrative_content
        in Page
          { page_title = narrative_title
          , page_content = narrative_story
          , page_choices = nextPageChoice maybe_next_choice
          })
      ]
    LineEntry{..} →
      let base = parent ⊕ "/" ⊕ story_line_name
          go ∷ [Entry Markdown] → [[(Text, Page)]]
          go [] = []
          go ((FamesEntry _):rest) = go rest
          go (x:[]) = process maybe_next_choice base x:[]
          go (x:rest@(y:_)) = process (Just next_choice) base x:go rest
           where
            next_choice = case nameOf x of
              Nothing → error "Internal error: FamesEntry should already have been covered"
              Just next_choice → next_choice
      in go story_line_contents |> concat
    SplitEntry{..} →
      let base = parent ⊕ "/" ⊕ split_name
          Narrative{..} = split_story
      in
        [("common", Page
          { page_title = narrative_title
          , page_content = narrative_story
          , page_choices = Choices split_question $
              mapMaybe
                (\Branch{..} → (branch_choice,) <$> nameOf branch_entry)
                split_branches
          })
        ]
        ⊕
        concatMap ((& branch_entry) >>> process maybe_next_choice base) split_branches
    FamesEntry{..} → []
