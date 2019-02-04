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
