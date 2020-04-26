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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.DarkLord (quest) where

import HabitOfFate.Prelude

import HabitOfFate.Quest
import qualified HabitOfFate.Quests.DarkLord.Part1 as Part1
import qualified HabitOfFate.Quests.DarkLord.Part2 as Part2
import HabitOfFate.Story

part_selection = Narrative
  { narrative_title = "Choose What to Follow"
  , narrative_story = "This quest has two parts."
  }

quest ∷ Quest
quest = Quest
  "darklord"
  "An entreatment from someone seeking to defeat the Dark Lord."
  mempty
  ( SplitEntry "part" part_selection "Which do you choose to follow first?"
    [ Part1.branch
    , Part2.branch
    ]
  )
