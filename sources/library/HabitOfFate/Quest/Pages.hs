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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Pages where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow)

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Quest
import HabitOfFate.Story

data PageChoices content =
    NoChoice Text
  | Choices content [(content,Text)]
  deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

data Page = Page
  { page_path ∷ Text
  , page_title ∷ Markdown
  , page_content ∷ Markdown
  , page_choices ∷ PageChoices Markdown
  } deriving (Eq,Ord,Read,Show)

buildPagesFromQuest ∷ ∀ m. MonadThrow m ⇒ Quest Markdown → m [Page]
buildPagesFromQuest quest@Quest{..} = do
  initial_quest_path ← initialQuestPath quest
  let quest_choices =
        [(" Return to the beginning of this quest.", initial_quest_path)
        ,(" Return to the choice of quests.", "index")
        ]
      quest_choices_without_brackets =
        [(embolden "Return to the beginning of this quest.", initial_quest_path)
        ,(embolden "Return to the choice of quests.", "index")
        ]

      proceedChoices ∷ Maybe Text → PageChoices Markdown
      proceedChoices Nothing =
        Choices
          "You have guided your chosen protagonist to success.  What next?"
          quest_choices_without_brackets
      proceedChoices (Just next_path) = NoChoice next_path

      process ∷ Maybe Text → Text → Entry Markdown → m [Page]
      process maybe_next_choice parent node = case node of
        EventEntry{..} → pure $
          let base = parent ⊕ "/" ⊕ event_name ⊕ "/"
              quest_choices_with_undo = ((" Return to the beginning of this event.", base ⊕ "common"):quest_choices)
              failure_dead_end =
                Choices
                  "You have guided your chosen protagonist to failure.  What next?"
                  ((embolden "Return to the beginning of this event.", base ⊕ "common"):quest_choices_without_brackets)
          in case event_outcomes of
            SuccessFailure{..} →
              [Page
                { page_path = base ⊕ "common"
                , page_title = outcomes_common_title
                , page_content = outcomes_common_story
                , page_choices = Choices outcomes_common_question $
                    [(embolden outcomes_success_choice, base ⊕ "success")
                    ,(embolden outcomes_failure_choice, base ⊕ "failure")
                    ]
                    ⊕
                    quest_choices
                }
              ,Page
                { page_path = base ⊕ "success"
                , page_title = outcomes_success_title
                , page_content = outcomes_success_story
                , page_choices = proceedChoices maybe_next_choice
                }
              ,Page
                { page_path = base ⊕ "failure"
                , page_title = outcomes_failure_title
                , page_content = outcomes_failure_story
                , page_choices = failure_dead_end
                }
              ]
            SuccessAvertedFailure{..} →
              [Page
                { page_path = base ⊕ "common"
                , page_title = outcomes_common_title
                , page_content = outcomes_common_story
                , page_choices = Choices outcomes_common_question $
                    [(embolden outcomes_success_choice, base ⊕ "success")
                    ,(embolden outcomes_averted_choice, base ⊕ "averted")
                    ,(embolden outcomes_failure_choice, base ⊕ "failure")
                    ]
                    ⊕
                    quest_choices
                }
              ,Page
                { page_path = base ⊕ "success"
                , page_title = outcomes_success_title
                , page_content = outcomes_success_story
                , page_choices = proceedChoices maybe_next_choice
                }
              ,Page
                { page_path = base ⊕ "averted"
                , page_title = outcomes_averted_title
                , page_content = outcomes_averted_story
                , page_choices = proceedChoices maybe_next_choice
                }
              ,Page
                { page_path = base ⊕ "failure"
                , page_title = outcomes_failure_title
                , page_content = outcomes_failure_story
                , page_choices = failure_dead_end
                }
              ]
            SuccessDangerAvertedFailure{..} →
              [Page
                { page_path = base ⊕ "common"
                , page_title = outcomes_common_title
                , page_content = outcomes_common_story
                , page_choices = Choices outcomes_common_question $
                    [(embolden outcomes_success_choice, base ⊕ "success")
                    ,(embolden outcomes_danger_choice, base ⊕ "danger")
                    ]
                    ⊕
                    quest_choices
                }
              ,Page
                { page_path = base ⊕ "success"
                , page_title = outcomes_success_title
                , page_content = outcomes_success_story
                , page_choices = proceedChoices maybe_next_choice
                }
              ,Page
                { page_path = base ⊕ "danger"
                , page_title = outcomes_danger_title
                , page_content = outcomes_danger_story
                , page_choices = Choices outcomes_danger_question $
                    [(embolden outcomes_averted_choice,base ⊕ "averted")
                    ,(embolden outcomes_failure_choice,base ⊕ "failure")
                    ]
                    ⊕
                    quest_choices_with_undo
                }
              ,Page
                { page_path = base ⊕ "averted"
                , page_title = outcomes_averted_title
                , page_content = outcomes_averted_story
                , page_choices = proceedChoices maybe_next_choice
                }
              ,Page
                { page_path = base ⊕ "failure"
                , page_title = outcomes_failure_title
                , page_content = outcomes_failure_story
                , page_choices = failure_dead_end
                }
              ]
        NarrativeEntry{..} → pure $
          [let Narrative{..} = narrative_content
          in Page
            { page_path = parent ⊕ "/" ⊕ narrative_name
            , page_title = narrative_title
            , page_content = narrative_story
            , page_choices = proceedChoices maybe_next_choice
            }
          ]
        LineEntry{..} →
          let base = parent ⊕ "/" ⊕ line_name
              go ∷ Entry Markdown → [Entry Markdown] → m [[Page]]
              go x [] = process maybe_next_choice base x <&> (:[])
              go x (FamesEntry{..}:rest) = go x rest
              go x (RandomStoriesEntry{..}:rest) = go x rest
              go x (StatusEntry{..}:rest) = go x rest
              go x (y:rest) = do
                next_path ← nextPathOf y
                (:) <$> process (Just $ base ⊕ "/" ⊕ next_path) base x <*> go y rest
          in case line_contents of
              [] → pure []
              x:rest → go x rest
             <&>
             concat
        SplitEntry{..} → do
          let name = parent ⊕ "/" ⊕ split_name
              base = name ⊕ "/"
              Narrative{..} = split_story
          page_choices ←
            traverse
              (\Branch{..} → (embolden branch_choice,) <$> ((base ⊕) <$> nextPathOf branch_entry))
              split_branches
          (Page
            { page_path = base ⊕ "common"
            , page_title = narrative_title
            , page_content = narrative_story
            , page_choices = Choices split_question page_choices
            }
            :) <$> (traverse ((& branch_entry) >>> process maybe_next_choice name) split_branches <&> concat)
        FamesEntry{..} → pure []
        RandomStoriesEntry{..} → pure []
        StatusEntry{..} → pure []

  process Nothing quest_name quest_entry
