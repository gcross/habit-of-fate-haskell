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
------------------------------------ Intro -------------------------------------
--------------------------------------------------------------------------------

intro_stories ∷ [Story]
intro_stories = [s|
The last thing in the world that <strong>|Susie</strong> wanted to do was
to wander alone in the Wicked Forest at night, but his/her|Susie
son/daughter|Tommy, little <strong>|Tommy</strong>, was sick and would not
live through the night unless |Susie could find <strong>an
|Illsbane</strong> plant. It is a hopeless task, but he/she| has no
other choice.
|]

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
------------------------------------ Wander ------------------------------------
--------------------------------------------------------------------------------

wander_stories ∷ [Story]
wander_stories = [s|
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

--------------------------------------------------------------------------------
------------------------------------ Found -------------------------------------
--------------------------------------------------------------------------------

found_stories ∷ [Story]
found_stories = [s|
After searching for what feels like hours, |Susie nearly steps on an |Illsbane
plant. his/her| heart leaps and he/she| gives a short prayer of thanks. He/she|
reaches down carefully to pick it up, and then starts heading back to his/her|
home.
================================================================================
|Susie starts to hear a sound and he/she| can't tell whether it is a buzzing or
the most beautiful music he/she| has ever heard. As it gets louder he/she|
notices the area around him/her| starting to get brighter. He/she| looks around
and sees a fairy, glowing brightly in the dark. The fairy beckons to him/her|,
and then flies away. |Susie hesitates briefly, and then runs after the fairy.

|Susie chases the fairy for about an hour, starting to doubt whether this is
such a good idea, when the fairy stops. He/she| catches up to it and sees {an
Illsbane} plant under it. Carefully, he/she| reaches down and picks it. When
he/she| looks up, the fairy is gone.

He/she| falls to his/her| knees and thanks you for guiding her to the plan.
He/she| then gets up and starts heading back to his/her| home.
|]

--------------------------------------------------------------------------------
------------------------------------- Won --------------------------------------
--------------------------------------------------------------------------------

won_story ∷ Story
won_story = [s_fixed|
|Susie is starting to feel like he/she| will never make it back when he/she|
notices that things are starting to get brighter -- he/she| must be getting close
to the vilage! he/she| gives you thanks for guiding her home.

A little bit further, and he/she| is back to his/her| house. Without a moment to
spare, he/she| immediately starts brewing medicine for |Tommy! He/she| brings
the medicine to |Tommy, and wakes him/her|Tommy up long enough to ladel it down
his throat. He/she|Tommy immediately falls back asleep. As |Susie is filled with
relief, exhaustion catches up to him/her| and he/she| falls asleep on the floor.

He/she| sleeps peacefully, with a smile on his/her| face. The next day, he/she|
builds an alter to you out of gratitude.
|]

--------------------------------------------------------------------------------
------------------------------------ Status ------------------------------------
--------------------------------------------------------------------------------

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
              then use substitutions_ <&> flip substitute won_story
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
  , ("Won", [won_story])
  , ("Looking for Herb Status", [looking_for_herb_story])
  , ("Returning", [returning_home_story])
  ]
