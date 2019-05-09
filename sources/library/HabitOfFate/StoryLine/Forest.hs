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
    "Entering the Wicked Forest"
    intro
    "Who shall we follow into the Wicked Forest?"
    [ StoryBranch
        "The village healer."
        (StoryLine "healer" $
          [ StoryNarrative
              "intro"
              "A Healer Prepares to Enter the Forest"
              intro_healer_story
          , StoryShuffle shared_story_entries
          , StoryEvent
              "conclusion"
              "Almost home!"
              conclusion_healer
              (StoryQuestion
                [story|Is there anything... odd, about the huts?|]
                (Tagged
                  (Success $ StoryLineOutcome
                    [story|Nope, everything is fine.|]
                    "The Child is Saved!"
                    [])
                  (Failure $ StoryLineOutcome
                    [story|Actually...|]
                    "Something is Strange Here"
                    [])))
              (StoryQuestion
                [story|"So what are you going to have her| do about it?"|]
                (Tagged
                  (Success $ StoryLineOutcome
                    [story|"Go back into the forest; it's the safest thing to do."|]
                    "Safety Wins"
                    [])
                  (Failure $ StoryLineOutcome
                    [story|"Assume it's a figment of her| imagination and proceed; after all, he/she|'s in a hurry!"|]
                    "Why Hurrying is a Bad Idea"
                    [])))
              wander_stories
          ])
    , StoryBranch
        "The parent of the sick child."
        (StoryLine "parent" $
          [ StoryNarrative
              "intro"
              "A Parent Prepares to Enter the Forest"
              intro_parent_story
          , StoryShuffle shared_story_entries
          , StoryEvent
              "conclusion"
              "The Right Herb?"
              conclusion_parent
              (StoryQuestion
                [story|"What plant did he/she| brought to the healer?"|]
                (Tagged
                  (Success $ StoryLineOutcome
                    [story|A |Plant plant.|]
                    "The Long Quest is Over!"
                    [])
                  (Failure $ StoryLineOutcome
                     [story|A different plant.|]
                     "Oh No, It's the Wrong Plant!"
                     [])))
              (StoryQuestion
                [story|Does the wrong herb work well enough?|]
                (Tagged
                  (Success $ StoryLineOutcome
                    [story|Yes.|]
                    "A Close Call"
                    [])
                  (Failure $ StoryLineOutcome
                     [story|No.|]
                     "All Is For Naught"
                     [])))
              wander_stories
          ])
    ]
  )

shared_story_entries ∷ [StoryEntry Story]
shared_story_entries =
  [ StoryEvent
      "gingerbread"
      "The Gingerbread House"
      gingerbread_house
      (StoryQuestion
        [story|"Where do you guide |Searcher?|]
        (Tagged
          (Success $ StoryLineOutcome
            [story|"Away from the gingerbread house."|]
            "Even Gingerbread Cannot Slow The Search"
            [])
          (Failure $ StoryLineOutcome
              [story|"Towards the gingerbread house."|]
              "The Gingerbread Compulsion is Too Great"
              [])))
      (StoryQuestion
        [story|How do you have |Searcher react?|]
        (Tagged
          (Success $ StoryLineOutcome
            [story|He/She| runs away!|]
            "Escaping The Gingerbread House"
            [])
          (Failure $ StoryLineOutcome
            [story|He/She| enters the house.|]
            "Entering The Gingerbread House"
            [])))
      wander_stories
  , StorySplit
      "found"
      "A Creature of the Forest"
      [story||Searcher runs into a creature of the forest.|]
      "Who is this?"
      [ StoryBranch
          "A cat."
          (StoryEvent
            "cat"
            "A Chance Encounter with a Cat"
            found_by_cat
            (StoryQuestion
              [story|"What color is the cat?"|]
              (Tagged
                (Success $ StoryLineOutcome
                  [story|Blue.|]
                  "Following the Blue Cat"
                  [("catcolor", Gendered "blue" Neuter)])
                (Failure $ StoryLineOutcome
                  [story|Green.|]
                  "Following the Green Cat"
                  [("catcolor", Gendered "green" Neuter)])))
            (StoryQuestion
              [story|Is he/she| hurt?|]
              (Tagged
                (Success $ StoryLineOutcome
                  [story|Yes.|]
                  "Can't Get Up"
                  [])
                (Failure $ StoryLineOutcome
                  [story|No.|]
                  "Missed Getting Hurt"
                  [])))
            wander_stories)
      , StoryBranch
          "A fairy."
          (StoryEvent
            "fairy"
            "Running After a Fairy"
            found_by_fairy
            (StoryQuestion
              [story|How fast does the fairy make |Searcher chase her?|]
              (Tagged
                (Success $ StoryLineOutcome
                  [story|Running speed.|]
                  "A Successful Chase"
                  [])
                (Failure $ StoryLineOutcome
                  [story|Ludicrous speed.|]
                  "Can't stop chasing"
                  [])))
            (StoryQuestion
              [story|What stops |Searcher's chase?|]
              (Tagged
                (Success $ StoryLineOutcome
                  [story|A tree.|]
                  "Slammed Into a Tree"
                  [])
                (Failure $ StoryLineOutcome
                    [story|Time.|]
                    "Passage of Time"
                    [])))
            wander_stories)
      ]
  , StoryEvent
      "fairy-circle"
      "Does She See The Fairy Circle?"
      fairy_circle
      (StoryQuestion
        [story|Does he/she| see it before stepping inside?|]
        (Tagged
          (Success $ StoryLineOutcome
            [story|"Yes."|]
            "Mushroom Circle Averted"
            [])
          (Failure $ StoryLineOutcome
            [story|"No."|]
            "The Searcher Steps Inside"
            [])))
      (StoryQuestion
        [story|Does he/she| make it out in time?|]
        (Tagged
          (Success $ StoryLineOutcome
            [story|Yes.|]
            "Escaping the Fairy Ring"
            [])
          (Failure $ StoryLineOutcome
            [story|No.|]
            "Trapped in the Fairy Circle"
            [])))
      wander_stories
  ]
