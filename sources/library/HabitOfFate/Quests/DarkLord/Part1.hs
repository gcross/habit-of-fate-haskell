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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module HabitOfFate.Quests.DarkLord.Part1 (branch) where

import HabitOfFate.Prelude

import HabitOfFate.Quest
import qualified HabitOfFate.Quests.DarkLord.Part1.Paladin as Paladin
import qualified HabitOfFate.Quests.DarkLord.Part1.Mercenary as Mercenary
import HabitOfFate.Story

classes = Narrative
  { narrative_title = "Class"
  , narrative_story = [story|
There are several people whe might take on the Dark Lord and end his terrible
rule.
|]}

branch ∷ Branch Story
branch = Branch
  "Part 1: Fall of the Dark Lord (or is it?)"
  mempty
  ( SplitEntry "1" classes "Whom will you have make the attempt?"
    [ Paladin.branch
    , Mercenary.branch
    ]
  )
