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

module HabitOfFate.Quests.DarkLord.Part2 (branch) where

import HabitOfFate.Prelude

import HabitOfFate.Quest
import qualified HabitOfFate.Quests.DarkLord.Part2.Mage as Mage
import qualified HabitOfFate.Quests.DarkLord.Part2.Paladin as Paladin
import qualified HabitOfFate.Quests.DarkLord.Part2.Rogue as Rogue
import HabitOfFate.Story

classes ∷ Narrative Story
classes = [narrative|
= Title =
Class
= Story =
There are several people whe might take on the Dark Lord and end his terrible
rule.
|]

branch ∷ Branch Story
branch = Branch
  "Part 2: Fall of the Dark Lord Once and For All"
  mempty
  ( SplitEntry "2" classes "Whom will You have make the attempt?"
    [ Mage.branch
    , Paladin.branch
    , Rogue.branch
    ]
  )
