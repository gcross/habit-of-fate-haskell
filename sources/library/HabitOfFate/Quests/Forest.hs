{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.Forest where

--------------------------------------------------------------------------------
----------------------------------- Imports ------------------------------------
--------------------------------------------------------------------------------

import HabitOfFate.Prelude hiding (State)

import Control.Monad.Random
import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import HabitOfFate.Quests.Forest.Parser
import HabitOfFate.Substitution
import HabitOfFate.TH
import HabitOfFate.Trial

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

data State = State
  { _substitutions_ ∷ HashMap Text Gendered
  , _herb_found_ ∷ Bool
  , _remaining_credits_ ∷ Tagged Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''State
makeLenses ''State

--------------------------------------------------------------------------------
------------------------------------ Stories -----------------------------------
--------------------------------------------------------------------------------

intro_stories ∷ [Story]
intro_stories = $(loadStories "forest" "intro")

wander_stories ∷ [Story]
wander_stories = $(loadStories "forest" "wander")

found_stories ∷ [Story]
found_stories = $(loadStories "forest" "found")

won_stories ∷ [Story]
won_stories = $(loadStories "forest" "won")

looking_for_herb_story ∷ Story
looking_for_herb_story = [stories_fixed|
|Susie continues to search in the dark for an |Illsbane plant.
|]

returning_home_story ∷ Story
returning_home_story = [stories_fixed|
An |Illsbane] plant in hand, |Susie continues home.
|]

--------------------------------------------------------------------------------
-------------------------------- Failure Stories -------------------------------
--------------------------------------------------------------------------------

failure_stories ∷ [FailureStory]
failure_stories = $(loadFailureStories)

failure_averted_stories ∷ [Story]
failure_averted_stories =
  [ failure_story ^. common_story_ ⊕ failure_story ^. averted_story_
  | failure_story ← failure_stories
  ]

failure_happened_stories ∷ [Story]
failure_happened_stories =
  [ failure_story ^. common_story_ ⊕ failure_story ^. happened_story_
  | failure_story ← failure_stories
  ]

--------------------------------------------------------------------------------
------------------------------------ Logic -------------------------------------
--------------------------------------------------------------------------------

test_substitutions ∷ Substitutions
test_substitutions =
  mapFromList
    [ ( "", Gendered "Bobby" Male )
    , ( "Susie", Gendered "Bobby" Male )
    , ( "Tommy", Gendered "Mary" Female )
    , ( "Illsbane", Gendered "Tigerlamp" Neuter )
    ]

initialize ∷ InitializeQuestRunner State
initialize = do
  InitializeQuestResult
    <$> (State test_substitutions False
         <$>
         (Tagged
           <$> (Success <$> numberUntilEvent 5)
           <*> (Failure <$> numberUntilEvent 1)
         )
        )
    <*> (substitute test_substitutions <$> uniform intro_stories)

pickStoryUsingState ∷ (MonadRandom m, MonadState State m) ⇒ [Story] → m Lazy.Text
pickStoryUsingState stories = substitute <$> use substitutions_ <*> uniform stories

pickStory ∷ MonadRandom m ⇒ State → [Story] → m Lazy.Text
pickStory s stories = substitute (s ^. substitutions_) <$> uniform stories

getStatus ∷ GetStatusQuestRunner State
getStatus s = substitute (s ^. substitutions_) story
 where
  story
   | s ^. herb_found_ = returning_home_story
   | otherwise = looking_for_herb_story

trial ∷ TrialQuestRunner State
trial (Tagged (Success available_success_credits) (Failure available_failure_credits)) = do
  Tagged
    (Success remaining_success_credits)
    (Failure remaining_failure_credits) ← use remaining_credits_
  if available_success_credits >= remaining_success_credits
    then do
      herb_already_found ← herb_found_ <<.= True
      TryQuestResult
        (if herb_already_found then QuestHasEnded else QuestInProgress)
        (Tagged
          (Success $ available_success_credits - remaining_success_credits)
          (Failure available_failure_credits)
        )
        <$> (if herb_already_found
              then pickStoryUsingState won_stories
              else pickStoryUsingState found_stories
            )
    else
      if available_failure_credits >= remaining_failure_credits
        then
          TryQuestResult
            QuestHasEnded
            (Tagged
              (Success 0)
              (Failure $ available_failure_credits - remaining_failure_credits)
            )
            <$> pickStoryUsingState failure_happened_stories
        else do
          remaining_credits_ .=
            Tagged
              (Success $ remaining_success_credits - available_success_credits)
              (Failure $ remaining_failure_credits - available_failure_credits)
          let stories
                | available_failure_credits > 0 = failure_averted_stories
                | otherwise = wander_stories
          TryQuestResult
            QuestInProgress
            (Tagged
              (Success 0)
              (Failure 0)
            )
            <$> pickStoryUsingState stories

--------------------------------------------------------------------------------
--------------------------------- Proofreading ---------------------------------
--------------------------------------------------------------------------------

stories ∷ [(Text, [Lazy.Text])]
stories =
  map (second (map (substitute test_substitutions)))
  [ ("Introduction", intro_stories)
  , ("Wandering", wander_stories)
  , ("Found", found_stories)
  , ("Won", won_stories)
  , ("Looking for Herb Status", [looking_for_herb_story])
  , ("Returning", [returning_home_story])
  ]
