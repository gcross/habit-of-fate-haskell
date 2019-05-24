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
import HabitOfFate.StoryLine
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

testQuest ∷ String → Quest Story → TestTree
testQuest name quest@Quest{..} = testGroup name
  [ testCase "Substitutions" $ extractAllPlaceholders quest @?= keysSet quest_standard_substitutions
  , testCase "Pages" $ do
      pages ← substituteQuestWithStandardSubstitutions quest <&> buildPagesFromQuest
      let names = map fst pages
          name_counts ∷ HashMap Text Int
          name_counts =
            foldl'
              (flip $ alterMap (maybe 1 (+1) >>> Just))
              mempty
              names
          duplicate_names = name_counts |> mapToList |> filter (\(_,count) → count > 1) |> sort
      duplicate_names @?= []
      let links =
            pages
            |> concatMap
                (\(source, Page{..}) → case page_choices of
                  Choices _ branches → [(source, target) | (_, target) ← branches]
                  _ → []
                )
          missing_links = filter (\(source, target) → target `notElem` names) links
      missing_links @?= []
  ]

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ testQuest "Forest" Forest.quest
  ]
