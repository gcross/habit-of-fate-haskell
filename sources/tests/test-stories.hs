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
import qualified Data.Text as T

import HabitOfFate.Data.QuestState
import HabitOfFate.Quest
import HabitOfFate.Quest.Pages
import HabitOfFate.Quests
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

testSubstititutions ∷ Quest → TestTree
testSubstititutions quest@Quest{..} =
  testCase (unpack quest_name) $
    (extractAllPlaceholders quest_entry ⊕ extractAllPlaceholders quest_initial_random_stories) @?= keysSet (defaultQuestSubstitutions quest)

testFames ∷ Quest → TestTree
testFames quest@Quest{..} =
  testCase (unpack quest_name) $ do
    all_states ← allQuestStates quest
    all_states
      |> mapMaybe (\(name, qs) → if onull (qs ^. quest_state_fames_) then Just name else Nothing)
      |> (\paths_without_fames →
          assertBool
            ("No fames in: " ⊕ (unpack $ T.intercalate ", " paths_without_fames))
            (onull paths_without_fames)
         )
    all_states
      |> mapMaybe (
          \(name, qs) → qs
            |> (^. quest_state_remaining_content_)
            |> find
                (\case
                  EventContent _ → True
                  _ → False
                )
            |> isJust
            |> bool (Just name) Nothing
         )
      |> (\paths_without_events →
          assertBool
            ("No events in: " ⊕ (unpack $ T.intercalate ", " paths_without_events))
            (onull paths_without_events)
         )
    all_states
      |> mapMaybe (
          \(name, qs) → qs
            |> (^. quest_state_remaining_content_)
            |> (^? _last)
            |> (\case
                  Just (EventContent _) → Nothing
                  Just (NarrativeContent _) → Nothing
                  _ → Just name
               )
         )
      |> (\paths_with_bad_ending →
          assertBool
            ("Paths have bad ending: " ⊕ (unpack $ T.intercalate ", " paths_with_bad_ending))
            (onull paths_with_bad_ending)
         )
    let all_names = map fst all_states
    nub all_names @?= all_names

main ∷ HasCallStack ⇒ IO ()
main = doMain $
  [ testGroup "Substitutions" $ map testSubstititutions quests
  , testCase "Pages" $ do
      pages ← traverse buildPagesFromQuest quests <&> (concat >>> (Page "index" "" "" (Choices "" []):))
      let paths = map page_path pages
          path_counts ∷ HashMap Text Int
          path_counts =
            foldl'
              (flip $ alterMap (maybe 1 (+1) >>> Just))
              mempty
              paths
          duplicate_paths = path_counts |> mapToList |> filter (snd >>> (> 1)) |> sort
      duplicate_paths @?= []
      let links =
            pages
            |> concatMap
                (\Page{..} → case page_choices of
                  Choices _ branches → [ (page_path, target) | target ← map snd branches ]
                  _ → []
                )
          missing_links = filter (snd >>> (∉ paths)) links
      missing_links @?= []
      forM_ quests $ \quest@Quest{..} → do
        initial_path ← initialQuestPath quest
        assertBool
          [i|"initial quest {quest_name} path {initial_path} does not exist"|]
          (initial_path ∈ paths)
  , testGroup "Fames" $ map testFames quests
  ]
