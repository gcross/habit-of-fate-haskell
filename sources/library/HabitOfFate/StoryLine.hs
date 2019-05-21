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

data StoryEntry content =
    StoryEvent
      { event_name ∷ Text
      , event_outcomes ∷ Outcomes content
      , event_random_stories ∷ [content]
      }
  | StoryNarrative
      { narrative_name ∷ Text
      , narrative_content ∷ Narrative content
      }
  | StoryLine
      { story_line_shuffle_mode ∷ ShuffleMode
      , story_line_name ∷ Text
      , story_line_contents ∷ [StoryEntry content]
      }
  | StorySplit
      { split_name ∷ Text
      , split_story ∷ Narrative content
      , split_question ∷ Markdown
      , split_branches ∷ [StoryBranch content]
      }
  | StoryFames
      { story_fames ∷ [content]
      }
 deriving (Foldable,Functor,Traversable)

nameOf ∷ StoryEntry content → Maybe Text
nameOf StoryEvent{..} = Just event_name
nameOf StoryNarrative{..} = Just narrative_name
nameOf StoryLine{..} = Just story_line_name
nameOf StorySplit{..} = Just split_name
nameOf StoryFames{..} = Nothing

data StoryBranch content = StoryBranch
  { branch_choice ∷ Markdown
  , branch_entry ∷ StoryEntry content
  } deriving (Foldable,Functor,Traversable)

data QuestStory content = QuestStory
  { quest_name ∷ Text
  , quest_entry ∷ StoryEntry content
  } deriving (Foldable,Functor,Traversable)

substituteQuestStory ∷ MonadThrow m ⇒ Substitutions → QuestStory Story → m (QuestStory Markdown)
substituteQuestStory subs = traverse (substitute subs)

data StoryPage = StoryPage
  { story_page_title ∷ Markdown
  , story_page_content ∷ Markdown
  , story_page_choices ∷ PageChoices Markdown
  }

nextPageChoice ∷ Maybe Text → PageChoices content
nextPageChoice Nothing = DeadEnd
nextPageChoice (Just next) = NoChoice next

buildPagesFromQuestStory ∷ QuestStory Markdown → [(Text, StoryPage)]
buildPagesFromQuestStory QuestStory{..} = process Nothing quest_name quest_entry
 where
  process ∷ Maybe Text → Text → StoryEntry Markdown → [(Text, StoryPage)]
  process maybe_next_choice parent node = case node of
    StoryEvent{..} →
      let base = parent ⊕ "/" ⊕ event_name ⊕ "/"
      in map (first (base ⊕)) $ case event_outcomes of
        SuccessFailure{..} →
          [("common", StoryPage
            { story_page_title = outcomes_common_title
            , story_page_content = outcomes_common_story
            , story_page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("success", StoryPage
            { story_page_title = outcomes_success_title
            , story_page_content = outcomes_success_story
            , story_page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", StoryPage
            { story_page_title = outcomes_failure_title
            , story_page_content = outcomes_failure_story
            , story_page_choices = DeadEnd
            })
          ]
        SuccessAvertedFailure{..} →
          [("common", StoryPage
            { story_page_title = outcomes_common_title
            , story_page_content = outcomes_common_story
            , story_page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_averted_choice,"averted")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("success", StoryPage
            { story_page_title = outcomes_success_title
            , story_page_content = outcomes_success_story
            , story_page_choices = nextPageChoice maybe_next_choice
            })
          ,("averted", StoryPage
            { story_page_title = outcomes_averted_title
            , story_page_content = outcomes_averted_story
            , story_page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", StoryPage
            { story_page_title = outcomes_failure_title
            , story_page_content = outcomes_failure_story
            , story_page_choices = DeadEnd
            })
          ]
        SuccessDangerAvertedFailure{..} →
          [("common", StoryPage
            { story_page_title = outcomes_common_title
            , story_page_content = outcomes_common_story
            , story_page_choices = Choices outcomes_common_question
                [(outcomes_success_choice,"success")
                ,(outcomes_danger_choice,"danger")
                ]
            })
          ,("success", StoryPage
            { story_page_title = outcomes_success_title
            , story_page_content = outcomes_success_story
            , story_page_choices = nextPageChoice maybe_next_choice
            })
          ,("danger", StoryPage
            { story_page_title = outcomes_danger_title
            , story_page_content = outcomes_danger_story
            , story_page_choices = Choices outcomes_danger_question
                [(outcomes_averted_choice,"averted")
                ,(outcomes_failure_choice,"failure")
                ]
            })
          ,("averted", StoryPage
            { story_page_title = outcomes_averted_title
            , story_page_content = outcomes_averted_story
            , story_page_choices = nextPageChoice maybe_next_choice
            })
          ,("failure", StoryPage
            { story_page_title = outcomes_failure_title
            , story_page_content = outcomes_failure_story
            , story_page_choices = DeadEnd
            })
          ]
    StoryNarrative{..} →
      [(parent ⊕ "/" ⊕ narrative_name
       ,let Narrative{..} = narrative_content
        in StoryPage
          { story_page_title = narrative_title
          , story_page_content = narrative_story
          , story_page_choices = nextPageChoice maybe_next_choice
          })
      ]
    StoryLine{..} →
      let base = parent ⊕ "/" ⊕ story_line_name
          go ∷ [StoryEntry Markdown] → [[(Text, StoryPage)]]
          go [] = []
          go ((StoryFames _):rest) = go rest
          go (x:[]) = process maybe_next_choice base x:[]
          go (x:rest@(y:_)) = process (Just next_choice) base x:go rest
           where
            next_choice = case nameOf x of
              Nothing → error "Internal error: StoryFames should already have been covered"
              Just next_choice → next_choice
      in go story_line_contents |> concat
    StorySplit{..} →
      let base = parent ⊕ "/" ⊕ split_name
          Narrative{..} = split_story
      in
        [("common", StoryPage
          { story_page_title = narrative_title
          , story_page_content = narrative_story
          , story_page_choices = Choices split_question $
              mapMaybe
                (\StoryBranch{..} → (branch_choice,) <$> nameOf branch_entry)
                split_branches
          })
        ]
        ⊕
        concatMap ((& branch_entry) >>> process maybe_next_choice base) split_branches
    StoryFames{..} → []
