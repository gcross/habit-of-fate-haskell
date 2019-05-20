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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Data.CallStack (HasCallStack)

import HabitOfFate.Data.Tagged
import qualified HabitOfFate.Quests.Forest as Forest
import qualified HabitOfFate.Quests.Forest.Stories as Forest
import HabitOfFate.Quest.StateMachine
import HabitOfFate.Story
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

testStory ∷ String → Substitutions → Story → TestTree
testStory name substitutions story = testCase name $ (extractPlaceholders story `difference` keysSet substitutions) @?= []

testStories ∷ String → Substitutions → [Story] → TestTree
testStories name substitutions stories =
  testGroup name
    [ testStory (show i) substitutions story
    | i ← [1 ∷ Int ..]
    | story ← stories
    ]

testTransitionsAreValid ∷ (Bounded α, Enum α, Eq α, Ord α, Show α) ⇒ String → [(α, Transition α)] → TestTree
testTransitionsAreValid name transitions = testGroup name
  [ testCase "All destination nodes are listed in the transitions" $
      let missing_destinations =
            transitions
              |> map (second $ \Transition{..} → filter (flip notMember transitions) (next ^. success_ ⊕ next ^. failure_))
              |> filter (snd >>> onull >>> not)
      in unless (onull missing_destinations) $
          assertFailure $ "Missing destinations in transitions: " ⊕ show missing_destinations
  , testCase "All transitions have at least between story" $
      let transitions_with_no_between_stories =
            [ label
            | (label, Transition{..}) ← transitions
            , onull between_stories
            ]
      in unless (onull transitions_with_no_between_stories) $
          assertFailure $ "Missing between stories in transitions: " ⊕ show transitions_with_no_between_stories
  , testCase "All transition labels are present in transitions" $
      (transitions |> map fst |> sort) @?= [minBound .. maxBound]
  ]

main ∷ HasCallStack ⇒ IO ()
main = doMain
  ----------------------------------------------------------------------------
  [ testGroup "Forest" $ let subs = Forest.static_substitutions in
  ----------------------------------------------------------------------------
    [ testStory "intro_parent" subs (Forest.intro_parent & narrative_story)
    , testStory "intro_healer" subs (Forest.intro_healer & narrative_story)
    , testStory "looking_for_herb_story" subs Forest.looking_for_herb_story
    , testStory "returning_home_story" subs Forest.returning_home_story
    , testStories "wander" subs Forest.wander_stories
    ]
  , testTransitionsAreValid
      "Forest (parent)"
      (Forest.transitionsFor $ Forest.Internal
        Forest.Parent
        (Gendered "Billy" Male)
        [Forest.GingerbreadHouseEvent, Forest.FoundEvent, Forest.FairyCircleEvent]
      )
  , testTransitionsAreValid
      "Forest (healer)"
      (Forest.transitionsFor $ Forest.Internal
        Forest.Healer
        (Gendered "Elly" Female)
        [Forest.FoundEvent, Forest.FairyCircleEvent, Forest.GingerbreadHouseEvent]
      )
  ]
