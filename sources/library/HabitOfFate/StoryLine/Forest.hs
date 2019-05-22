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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.StoryLine.Forest where

import HabitOfFate.Prelude

import HabitOfFate.Data.Gender
import HabitOfFate.Data.Tagged
import HabitOfFate.Quests.Forest.Stories
import HabitOfFate.Story
import HabitOfFate.StoryLine
import HabitOfFate.Substitution

plants ∷ [Gendered]
plants = map (\name → Gendered name Neuter) $
  ["Illsbane"
  ,"Tigerlamp"
  ]

quest ∷ Quest Story
quest = Quest
  "forest"
  (SplitEntry
    "who"
    intro
    "Who shall we follow into the Wicked Forest?"
    [ Branch
        "The village healer."
        (LineEntry NoShuffle "healer" $
          [ FamesEntry fames_healer
          , NarrativeEntry
              "intro"
              intro_healer
          , LineEntry Shuffle "events" shared_story_entries
          , EventEntry
              "conclusion"
              conclusion_healer
              wander_stories
          ])
    , Branch
        "The parent of the sick child."
        (LineEntry NoShuffle "parent" $
          [ FamesEntry fames_parent
          , NarrativeEntry
              "intro"
              intro_parent
          , LineEntry Shuffle "events" shared_story_entries
          , EventEntry
              "conclusion"
              conclusion_parent
              wander_stories
          ])
    ]
  )

shared_story_entries ∷ [Entry Story]
shared_story_entries =
  [ EventEntry
      "gingerbread"
      gingerbread_house
      wander_stories
  , SplitEntry
      "found"
      found
      "Who is this?"
      [ Branch
          "A cat."
          (EventEntry
            "cat"
            found_by_cat
            wander_stories)
      , Branch
          "A fairy."
          (EventEntry
            "fairy"
            found_by_fairy
            wander_stories)
      ]
  , EventEntry
      "fairy-circle"
      fairy_circle
      wander_stories
  ]
