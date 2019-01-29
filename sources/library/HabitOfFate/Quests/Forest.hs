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

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), (.:), withObject, withText, object)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random (MonadRandom, getRandom, uniform)
import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.JSON
import HabitOfFate.Quest
import HabitOfFate.Substitution
import HabitOfFate.Story
import HabitOfFate.Pages
import HabitOfFate.Trial

import HabitOfFate.Quests.Forest.Stories

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

data Searcher = Parent | Healer
  deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON Searcher where
  toJSON Parent = String "parent"
  toJSON Healer = String "healer"

instance FromJSON Searcher where
  parseJSON = withText "searcher field must be a string" $ \case
    "parent" → pure Parent
    "healer" → pure Healer
    other → fail [i|searcher must be parent or healer, not #{other}|]

data NextEvent = GingerbreadHouseEvent | FoundEvent | FairyCircleEvent | VictoryEvent
  deriving (Enum,Eq,Ord,Read,Show)

instance ToJSON NextEvent where
  toJSON GingerbreadHouseEvent = String "gingerbread house"
  toJSON FoundEvent = String "found"
  toJSON FairyCircleEvent = String "fairy circle"
  toJSON VictoryEvent = String "victory"

instance FromJSON NextEvent where
  parseJSON = withText "next event field must be a string" $ \case
    "gingerbread house" → pure GingerbreadHouseEvent
    "found" → pure FoundEvent
    "fairy circle" → pure FairyCircleEvent
    "victory" → pure VictoryEvent
    other → fail [i|searcher must be parent or healer, not #{other}|]

data State = State
  { _substitutions_ ∷ HashMap Text Gendered
  , _searcher_ ∷ Searcher
  , _next_event_ ∷ NextEvent
  } deriving (Eq,Ord,Read,Show)
makeLenses ''State

instance ToJSON State where
  toJSON State{..} = object
    [ "substitutions" .== toJSON _substitutions_
    , "searcher" .== toJSON _searcher_
    , "next event" .== _next_event_
    ]

instance FromJSON State where
  parseJSON = withObject "forest state must have object shape" $ \o →
    State
      <$> (o .: "substitutions" >>= parseJSON)
      <*> (o .: "searcher" >>= parseJSON)
      <*> (o .: "next event" >>= parseJSON)

static_substitutions ∷ Substitutions
static_substitutions =
  mapFromList
    [ ( "", Gendered "Bobby" Male )
    , ( "color", Gendered "blue" Neuter )
    , ( "Susie", Gendered "Bobby" Male )
    , ( "Tommy", Gendered "Mary" Female )
    , ( "Illsbane", Gendered "Tigerlamp" Neuter )
    ]


--------------------------------------------------------------------------------
------------------------------------ Logic -------------------------------------
--------------------------------------------------------------------------------

initialize ∷ InitializeQuestRunner State
initialize = do
  (searcher, intro_story) ← uniform [(Parent, intro_parent_story), (Healer, intro_healer_story)]
  InitializeQuestResult
    (State static_substitutions searcher GingerbreadHouseEvent)
    <$> substitute static_substitutions intro_story

pickStoryUsingState ∷ (MonadRandom m, MonadState State m, MonadThrow m) ⇒ [Story] → m Lazy.Text
pickStoryUsingState stories = join $ substitute <$> (use substitutions_) <*> (uniform stories)

getStatus ∷ GetStatusQuestRunner State
getStatus s = substitute (s ^. substitutions_) story
 where
  story
   | s ^. next_event_ <= FoundEvent = looking_for_herb_story
   | otherwise = returning_home_story

trial ∷ TrialQuestRunner State
trial result =
  tryBinomial (1/3)
  >=>
  bool
    (TryQuestResult QuestInProgress <$> pickStoryUsingState wander_stories)
    (do
      substitutions ← use substitutions_
      (new_quest_status, storyFor, cat_color) ←
        case result of
          SuccessResult → pure (QuestInProgress, storyForSuccess, "blue")
          FailureResult →
            getRandom
            <&>
            bool
              (QuestInProgress, storyForAverted, "green")
              (QuestHasEnded, storyForFailure, "green")
      let sub = storyFor >>> substitute substitutions
      use next_event_ >>= \case
        GingerbreadHouseEvent → do
          next_event_ .= FoundEvent
          TryQuestResult new_quest_status <$> sub gingerbread_house_event
        FoundEvent → do
          next_event_ .= FairyCircleEvent
          TryQuestResult new_quest_status
            <$> uniformAction
                  [ sub found_by_fairy_event
                  , substitute
                      (insertMap "color" (Gendered cat_color Neuter) substitutions)
                      (storyFor found_by_cat_event)
                  ]
        FairyCircleEvent → do
          next_event_ .= VictoryEvent
          TryQuestResult new_quest_status <$> sub fairy_circle_event
        VictoryEvent →
          TryQuestResult QuestHasEnded
            <$> ((use searcher_ <&> \case
                    Parent → conclusion_parent_event
                    Healer → conclusion_healer_event
                  ) >>= sub)
    )

--------------------------------------------------------------------------------
--------------------------------- Proofreading ---------------------------------
--------------------------------------------------------------------------------

pages ∷ MonadThrow m ⇒ m Pages
pages = buildPages
  (mapFromList
    [ ( "", Gendered "Andrea" Female )
    , ( "Susie", Gendered "Andrea" Female )
    , ( "Tommy", Gendered "Elly" Female )
    , ( "Illsbane", Gendered "Tigerlamp" Neuter )
    ]
  )
  ( PageGroup "forest"
    [ PageItem
        ""
        "Searching For An Essential Ingredient In The Wicked Forest"
        intro_healer_story
        (NoChoice "gingerbread")
    , PageGroup "gingerbread"
      [ PageItem
          ""
          "The Gingerbread House"
          (gingerbread_house_event ^. story_common_)
          (Choices "Where do you guide Andrea?"
            [("Towards the gingerbread house.",  "towards")
            ,("Away from the gingerbread house.", "away")
            ]
          )
      , PageItem
          "away"
          "Even Gingerbread Cannot Slow Her Search"
          (gingerbread_house_event ^. story_success_)
          (NoChoice "-forest-found")
      , PageItem
          "towards"
          "The Gingerbread Compulsion is Too Great"
          (gingerbread_house_event ^. story_averted_or_failure_)
          (Choices "How do you have Andrea react?"
            [("She enters the house.", "enter")
            ,("She runs away!", "run")
            ]
          )
      , PageItem
          "enter"
          "Entering The Gingerbread House"
          (gingerbread_house_event ^. story_failure_)
          DeadEnd
      , PageItem
          "run"
          "Escaping The Gingerbread House"
          (gingerbread_house_event ^. story_averted_)
          (NoChoice "-forest-found")
      ]
    , PageItem
        "found"
        "The Ingredient Is Finally Found... Or Is It?"
        "Test found page."
        DeadEnd
    ]
  )
