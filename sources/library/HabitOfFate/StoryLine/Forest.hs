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

quest_story ∷ QuestStory Story
quest_story = QuestStory
  "forest"
  (StorySplit
    "who"
    intro
    "Who shall we follow into the Wicked Forest?"
    [ StoryBranch
        "The village healer."
        (StoryLine NoShuffle "healer" $
          [ StoryFames fames_healer
          , StoryNarrative
              "intro"
              intro_healer
          , StoryLine Shuffle "events" shared_story_entries
          , StoryEvent
              "conclusion"
              conclusion_healer
              wander_stories
          ])
    , StoryBranch
        "The parent of the sick child."
        (StoryLine NoShuffle "parent" $
          [ StoryFames fames_parent
          , StoryNarrative
              "intro"
              intro_parent
          , StoryLine Shuffle "events" shared_story_entries
          , StoryEvent
              "conclusion"
              conclusion_parent
              wander_stories
          ])
    ]
  )

shared_story_entries ∷ [StoryEntry Story]
shared_story_entries =
  [ StoryEvent
      "gingerbread"
      gingerbread_house
      wander_stories
  , StorySplit
      "found"
      found
      "Who is this?"
      [ StoryBranch
          "A cat."
          (StoryEvent
            "cat"
            found_by_cat
            wander_stories)
      , StoryBranch
          "A fairy."
          (StoryEvent
            "fairy"
            found_by_fairy
            wander_stories)
      ]
  , StoryEvent
      "fairy-circle"
      fairy_circle
      wander_stories
  ]
