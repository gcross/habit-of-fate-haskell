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

module HabitOfFate.Quests.Forest.Pages where

--------------------------------------------------------------------------------
----------------------------------- Imports ------------------------------------
--------------------------------------------------------------------------------

import HabitOfFate.Prelude hiding (State)

import Control.Monad.Catch (MonadThrow)

import HabitOfFate.Substitution
import HabitOfFate.Story
import HabitOfFate.Pages

import HabitOfFate.Quests.Forest.Stories

pages ∷ MonadThrow m ⇒ m Pages
pages = buildPages
  (mapFromList
    [ ( "", Gendered "Andrea" Female )
    , ( "Searcher", Gendered "Andrea" Female )
    , ( "Child", Gendered "Elly" Female )
    , ( "Plant", Gendered "Tigerlamp" Neuter )
    ]
  )
  ( PageGroup "forest" (Just "The Wicked Forest")
    [ PageItem ""
        "Searching For An Essential Ingredient In The Wicked Forest"
        intro_healer_story
        (NoChoice "gingerbread")
    , PageGroup "gingerbread" (Just "The Gingerbread House")
      [ PageItem ""
          "The Gingerbread House"
          (gingerbread_house ^. story_common_)
          (Choices "Where do you guide Andrea?"
            [("Towards the gingerbread house.",  "towards")
            ,("Away from the gingerbread house.", "away")
            ]
          )
      , PageItem "away"
          "Even Gingerbread Cannot Slow Her Search"
          (gingerbread_house ^. story_success_)
          (NoChoice "-forest-found")
      , PageItem "towards"
          "The Gingerbread Compulsion is Too Great"
          (gingerbread_house ^. story_averted_or_failure_)
          (Choices "How do you have Andrea react?"
            [("She enters the house.", "enter")
            ,("She runs away!", "run")
            ]
          )
      , PageItem "enter"
          "Entering The Gingerbread House"
          (gingerbread_house ^. story_failure_)
          DeadEnd
      , PageItem "run"
          "Escaping The Gingerbread House"
          (gingerbread_house ^. story_averted_)
          (NoChoice "-forest-found")
      ]
    , PageGroup "found" (Just "The Forest Creature")
      [ PageItem ""
          "Finding the Herb"
          ""
            (Choices "The healer runs into a creature of the forest.  Whom is this?"
              [("A cat.",  "cat")
              ,("A fairy.", "fairy")
              ]
            )
      , PageGroup "cat" Nothing
        [ PageItem ""
            "Chance Encounter with a Cat"
            (found_by_fairy ^. story_common_)
            (Choices "What color is the cat?"
              [("Blue", "blue")
              ,("Green", "green")
              ]
            )
        , PageItem "blue"
            "Following the Blue Cat"
            (found_by_fairy ^. story_averted_or_failure_)
            (Choices "Is she hurt?"
              [("Yes", "hurt")
              ,("No", "fine")
              ]
            )
        , PageItem "hurt"
            "Can't Get Up"
            (found_by_fairy ^. story_failure_)
            DeadEnd
        , PageItem "fine"
            "Missed Getting Hurt"
            (found_by_fairy ^. story_averted_)
            (NoChoice "-forest-fairy-circle")
        , PageItem "green"
            "Following the Green Cat"
            (found_by_fairy ^. story_success_)
            (NoChoice "-forest-fairy-circle")
        ]
      , PageGroup "fairy" Nothing
        [ PageItem ""
            "Running After a Fairy"
            (found_by_fairy ^. story_common_)
            (Choices "How fast does the fairy make Andrea chase her?"
              [("Running speed", "running")
              ,("Ludicious speed", "ludicrous")
              ]
            )
        , PageItem "running"
            "A Successful Chase"
            (found_by_fairy ^. story_success_)
            (NoChoice "-forest-fairy-circle")
        , PageItem "ludicrous"
            "Can't Stop Chasing"
            (found_by_fairy ^. story_averted_or_failure_)
            (Choices "What stops Andrea's chase?"
              [("A tree", "tree")
              ,("Time", "time")
              ]
            )
        , PageItem "tree"
            "Slammed Into a Tree"
            (found_by_fairy ^. story_averted_)
            (NoChoice "-forest-fairy-circle")
        , PageItem "time"
            "Passage of Time"
            (found_by_fairy ^. story_failure_)
            DeadEnd
        ]
      ]
    , PageGroup "fairy-circle" (Just "The Circle of Mushrooms")
      [ PageItem ""
          "Does She See The Fairy Circle?"
          "A mushroom circle lies just along Adrea's path, but she is too busy looking for a Tigerlamp plant."
          (Choices "Does she see it?"
            [("Yes", "away")
            ,("No",  "towards")
            ]
          )
      , PageItem "away"
          "Mushroom Circle Averted"
          (fairy_circle ^. story_success_)
          (NoChoice "-forest-home")
      , PageItem "towards"
          "Andrea Steps Inside"
          (fairy_circle ^. story_averted_or_failure_)
          (Choices "Does she make it out in time?"
            [("Yes", "escaped")
            ,("No", "trapped")
            ]
          )
      , PageItem "escaped"
          "Escaping The Fairy Ring"
          (fairy_circle ^. story_averted_)
          (NoChoice "-forest-home")
      , PageItem "trapped"
          "Traped in the Fairy Circle"
          (fairy_circle ^. story_failure_)
          DeadEnd
      ]
    , PageGroup "home" (Just "Almost done")
      [ PageItem ""
          "The Right Herb?"
          (conclusion_parent ^. story_common_)
          (Choices "What plant Did Andrea bring the right herb to the healer?"
            [("A Tigerlamp Plant", "victory")
            ,("An Illsbane Plant", "second-chance")
            ]
          )
      , PageItem "victory"
          "The Long Quest is Over!"
          (conclusion_parent ^. story_success_)
          (Choices "Where do you want to go from here?"
            [("Back to the start of the Wicked Forest", "-forest")
            ,("Back to the list of adventures", "-index")
            ]
          )
      , PageItem "second-chance"
          "Andrea Steps Inside"
          (conclusion_parent ^. story_averted_or_failure_)
          (Choices "Does the wrong herb work well enough?"
            [("Yes", "close-call")
            ,("No", "death")
            ]
          )
      , PageItem "close-call"
          "A Close Call"
          (conclusion_parent ^. story_averted_)
          (NoChoice "-forest-home")
      , PageItem "death"
          "All Is For Naught"
          (conclusion_parent ^. story_failure_)
          DeadEnd
      ]
    ]
  )
