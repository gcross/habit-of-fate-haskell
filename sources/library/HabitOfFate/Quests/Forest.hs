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
wander_stories = [stories|
Nothing happens as |Susie wanders through the forest.
================================================================================
As |Susie continues to search he/she| hears the howling of wolves, which makes
him/her| shiver. Thank goodness they seem to be in the distance!
================================================================================
|Susie is paying so much attention to looking for |Illsbane that he/she|
almost misses the ominous circle of mushrooms. He/she| says a prayer of thanks
that he/she| noticed it before stepping inside.
================================================================================
|Susie starts to notice that it is growing brighter. He/she| looks around and
sees an unnaturally glowing clearing in the distance. He/she| feels drawn to it,
but he/she| knows that no good could possibly come to him/her| there, so he/she|
summons his/her| will and turns away from it.
================================================================================
|Susie's candle goes out; the resulting darkness is oppressive. Fortunately,
he/she| prepared for this. He/she| reached into his/her| pack and drew out
flintstone, which he/she| uses to re-light the candle.
================================================================================
|Susie can feel that something is amiss, but he/she| can't figure out what. He/she|
pauses for a moment and looks around. After a moment, he/she| realizes that
his/her| shadow didn't stop walking when he/she| did. He/she| backs away slowly as
his/her| shadow gets further and further away from him/her|. He/she| decide to
start searching in a different direction.
================================================================================
|Susie looks up at the Moon. For reasons that nobody understands, the Moon is
always full in the Wicked Forest. The good news is that this makes it easier to
search for |Illsbane, but the bad news is that he/she| has to worry about
werewolves...
================================================================================
“Hello, human. It isn't often that I get to see one of your kind here.”

|Susie jumped and looked around for the source of the voice. He/she| heard it
laugh. “You are just as stupid as the rest of your kind. Look, I am over here.”

|Susie finally realized that the voice was coming from the tree right next to
her.

“Why are you here?” it asked. |Susie replied, “I am looking for an |Illsbane
plant. I down't suppose you have seen one?” It laughed. “Do I look like I have
legs?” |Susie replied, “Umm, no, I guess not; I guess I'll just be going on my
way then...”

|Susie resumed searching, the laugher of the tree receding as he/she| left it
behind.
|]

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

trial ∷ TrialQuestRunner State
trial SuccessResult =
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
trial FailureResult =
  tryBinomial (1/2)
  >=>
  bool
    (TryQuestResult QuestInProgress <$> pickStoryUsingState failure_averted_stories)
    (TryQuestResult QuestHasEnded <$> pickStoryUsingState failure_happened_stories)

--------------------------------------------------------------------------------
--------------------------------- Proofreading ---------------------------------
--------------------------------------------------------------------------------

proofread_stories ∷ [(Text, [Lazy.Text])]
proofread_stories =
  map (second (map (substitute test_substitutions)))
  [ ("Introduction", intro_stories)
  , ("Wandering", wander_stories)
  , ("Found", found_stories)
  , ("Won", won_stories)
  , ("Looking for Herb Status", [looking_for_herb_story])
  , ("Returning", [returning_home_story])
  ]
