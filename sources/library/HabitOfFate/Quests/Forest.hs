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

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random (uniform)

import HabitOfFate.Data.Tagged
import HabitOfFate.Quest
import qualified HabitOfFate.Quest.StateMachine as StateMachine
import HabitOfFate.Quest.StateMachine (Transition(..))
import HabitOfFate.Substitution
import HabitOfFate.Story
import HabitOfFate.Pages
import HabitOfFate.TH

import HabitOfFate.Quests.Forest.Stories

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

data Searcher = Parent | Healer deriving (Eq,Ord,Read,Show)
deriveJSON ''Searcher

data Label = GingerbreadHouse | FoundByCat | FoundByFairy | FairyCircle | Home
  deriving (Bounded,Enum,Eq,Ord,Read,Show)
deriveJSON ''Label

type State = StateMachine.State Label Searcher

static_substitutions ∷ Substitutions
static_substitutions =
  mapFromList
    [ ( "", Gendered "Bobby" Male )
    , ( "Susie", Gendered "Bobby" Male )
    , ( "Tommy", Gendered "Mary" Female )
    , ( "Illsbane", Gendered "Tigerlamp" Neuter )
    , ( "catcolor", Gendered "yellow" Neuter )
    ]

--------------------------------------------------------------------------------
------------------------------------ Logic -------------------------------------
--------------------------------------------------------------------------------

transitionsFor ∷ Searcher → [(Label, Transition Label)]
transitionsFor searcher =
  [ (GingerbreadHouse, Transition gingerbread_house wander_stories looking_for_herb_story [FoundByCat, FoundByFairy] def)
  , (FoundByCat, Transition found_by_cat wander_stories looking_for_herb_story [FairyCircle]
      (Tagged (Success [("catcolor", Gendered "green" Neuter)])
              (Failure [("catcolor", Gendered "blue" Neuter)])
      )
    )
  , (FoundByFairy, Transition found_by_fairy wander_stories returning_home_story [FairyCircle] def)
  , (FairyCircle, Transition fairy_circle wander_stories returning_home_story [Home] def)
  , (Home, Transition conclusion wander_stories returning_home_story [] def)
  ]
 where
  conclusion = case searcher of
    Parent → conclusion_parent
    Healer → conclusion_healer

initialize ∷ InitializeQuestRunner State
initialize = do
  searcher ← uniform [Parent, Healer]
  let introduction = case searcher of { Parent → intro_parent_story; Healer → intro_healer_story; }
  StateMachine.initialize static_substitutions GingerbreadHouse searcher introduction

getStatus ∷ GetStatusQuestRunner State
getStatus s = StateMachine.getStatus (s |> StateMachine.internal |> transitionsFor) s

trial ∷ TrialQuestRunner State
trial result scale = do
  transitions ← get <&> (StateMachine.internal >>> transitionsFor)
  StateMachine.trial transitions result scale

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
          (gingerbread_house ^. story_common_)
          (Choices "Where do you guide Andrea?"
            [("Towards the gingerbread house.",  "towards")
            ,("Away from the gingerbread house.", "away")
            ]
          )
      , PageItem
          "away"
          "Even Gingerbread Cannot Slow Her Search"
          (gingerbread_house ^. story_success_)
          (NoChoice "-forest-found")
      , PageItem
          "towards"
          "The Gingerbread Compulsion is Too Great"
          (gingerbread_house ^. story_averted_or_failure_)
          (Choices "How do you have Andrea react?"
            [("She enters the house.", "enter")
            ,("She runs away!", "run")
            ]
          )
      , PageItem
          "enter"
          "Entering The Gingerbread House"
          (gingerbread_house ^. story_failure_)
          DeadEnd
      , PageItem
          "run"
          "Escaping The Gingerbread House"
          (gingerbread_house ^. story_averted_)
          (NoChoice "-forest-found")
      ]
    , PageItem
        "found"
        "The Ingredient Is Finally Found... Or Is It?"
        "Test found page."
        DeadEnd
    ]
  )
