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

import HabitOfFate.StoryLine
import HabitOfFate.StoryLine.Pages
import HabitOfFate.StoryLine.Quests
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

testSubstititutions ∷ Quest Story → TestTree
testSubstititutions quest@Quest{..} =
  testCase (unpack quest_name) $
    extractAllPlaceholders quest @?= keysSet quest_standard_substitutions

main ∷ HasCallStack ⇒ IO ()
main = doMain $
  [ testGroup "Substitutions" $ map testSubstititutions quests
  , testCase "Pages" $ do
      pages ←
        mapM substituteQuestWithStandardSubstitutions quests
        <&>
        (concatMap buildPagesFromQuest >>> (Page "index" "" "" (Choices "" []):))
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
  ]
