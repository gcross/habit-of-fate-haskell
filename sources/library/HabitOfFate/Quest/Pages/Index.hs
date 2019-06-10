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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest.Pages.Index where

import HabitOfFate.Prelude

import HabitOfFate.Data.Markdown
import HabitOfFate.Quest.Pages
import HabitOfFate.StoryLine
import HabitOfFate.StoryLine.Quests

index_page ∷ Page
index_page = Page
  "index"
  "The Adventure Begins"
  (["Men, women, searchers, wanderers, people who just want to get home,"
    ,"all of them send their prayers to you in the hope you will hear"
    ,"them and grant them aid."
    ,""
    ,"But will you?  You are a God, after all, and these mortals can make"
    ,"such fun playthings."
    ]
    |> (unlines ∷ [Text] → Text)
    |> Markdown)
  (Choices "The choice is yours.  Where would you like to start?" $
    [ (embolden quest_choice, initialQuestPath quest) | quest@Quest{..} ← quests])
