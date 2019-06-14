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

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.CallStack (HasCallStack)
import qualified Data.Text as T

import HabitOfFate.Data.QuestState
import HabitOfFate.Quest.Pages
import HabitOfFate.StoryLine
import HabitOfFate.StoryLine.Quests
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

testSubstititutions ∷ Quest Story → TestTree
testSubstititutions quest@Quest{..} =
  testCase (unpack quest_name) $
    extractAllPlaceholders quest @?= keysSet quest_standard_substitutions

testFames ∷ Quest Story → TestTree
testFames quest@Quest{..} =
  testCase (unpack quest_name) $ do
    let all_states = allQuestStates quest
    all_states
      |> mapMaybe (\(name, QuestState{..}) → if onull quest_state_fames then Just name else Nothing)
      |> (\paths_without_fames →
          assertBool
            ("No fames in: " ⊕ (unpack $ T.intercalate ", " paths_without_fames))
            (onull paths_without_fames)
         )
    let all_names = map fst all_states
    nub all_names @?= all_names

main ∷ HasCallStack ⇒ IO ()
main = doMain $
  [ testGroup "Substitutions" $ map testSubstititutions quests
  , testCase "Pages" $ do
      let pages =
            map substituteQuestWithStandardSubstitutions quests
            |> concatMap buildPagesFromQuest
            |> (Page "index" "" "" (Choices "" []):)
      evaluate $ rnf pages
      let paths = map page_path pages
          path_counts ∷ HashMap Text Int
          path_counts =
            foldl'
              (flip $ alterMap (maybe 1 (+1) >>> Just))
              mempty
              paths
          duplicate_paths = path_counts |> mapToList |> filter (\(_,count) → count > 1) |> sort
      duplicate_paths @?= []
      let links =
            pages
            |> concatMap
                (\Page{..} → case page_choices of
                  Choices _ branches → [(page_path, target) | (_, target) ← branches]
                  _ → []
                )
          missing_links = filter (\(source, target) → target `notElem` paths) links
      missing_links @?= []
      forM_ quests $ \quest@Quest{..} →
        assertBool
          [i|"initial quest {quest_name} path {initialQuestPath quest} does not exist"|]
          (initialQuestPath quest ∈ paths)
  , testGroup "Fames" $ map testFames quests
  ]
