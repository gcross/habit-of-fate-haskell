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
import HabitOfFate.Story
import HabitOfFate.TH
import HabitOfFate.Trial

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

data State = State
  { _substitutions_ ∷ HashMap Text Gendered
  , _herb_found_ ∷ Bool
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
    <$> (pure $ State test_substitutions False)
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

trialSuccess ∷ TrialQuestRunner State
trialSuccess =
  tryBinomial (1/3)
  >=>
  bool
    (TryQuestResult QuestInProgress <$> pickStoryUsingState wander_stories)
    ((herb_found_ <<.= True)
     >>=
     bool
       (TryQuestResult QuestInProgress <$> pickStoryUsingState found_stories)
       (TryQuestResult QuestHasEnded <$> pickStoryUsingState won_stories)
    )

trialFailure ∷ TrialQuestRunner State
trialFailure =
  tryBinomial (1/2)
  >=>
  bool
    (TryQuestResult QuestInProgress <$> pickStoryUsingState failure_averted_stories)
    (TryQuestResult QuestHasEnded <$> pickStoryUsingState failure_happened_stories)

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
