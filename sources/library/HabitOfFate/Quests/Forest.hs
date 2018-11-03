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
------------------------------------- Lost -------------------------------------
--------------------------------------------------------------------------------

data FailureResult = FailureAverted | FailureHappened

data FailureEvent = FailureEvent
  { _failure_common_paragraphs_ ∷ Story
  , _failure_averted_paragraphs_ ∷ Story
  , _failure_happened_paragraphs_ ∷ Story
  }
makeLenses ''FailureEvent

makeFailureEvent ∷ (Story, Story, Story) → FailureEvent
makeFailureEvent (common,averted,happened) = FailureEvent common averted happened

failure_stories ∷ [FailureEvent]
failure_stories = fmap makeFailureEvent
------------------------------ Gingerbread House -------------------------------
  [[s_fixed|
|Susie sees a house made out of... gingerbread?

He/she| feels a strange compulsion to approach it.
================================================================================
He/she| fights the compulsion, and continues on his/her| search.
================================================================================
As he/she| gets closer, the door opens and an old woman beckons her in.
“You've arrived just in time!” she says. “Dinner has just finished cooking. Come
on in!”

Not knowing why he/she| was doing this, |Susie entered the... cottage? The woman
leads her to an oven. “Here, look inside.”

|Susie looks inside the oven and sees... little |Tommy? he/she| screams, and
faints.

Daylight awakens her. He/she|Susie looks around, but the gingerbread house is
nowhere to be found.

He/she| sobs -- there is no way that he/she| will be able to make it home
in time now.
     |]
  ]

failure_averted_stories ∷ [Story]
failure_averted_stories =
  [ event ^. failure_common_paragraphs_ ⊕ event ^. failure_averted_paragraphs_
  | event ← failure_stories
  ]

failure_happened_stories ∷ [Story]
failure_happened_stories =
  [ event ^. failure_common_paragraphs_ ⊕ event ^. failure_happened_paragraphs_
  | event ← failure_stories
  ]

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
looking_for_herb_story = [s_fixed|
|Susie continues to search in the dark for an |Illsbane plant.
|]

returning_home_story ∷ Story
returning_home_story = [s_fixed|
An |Illsbane] plant in hand, |Susie continues home.
|]

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
