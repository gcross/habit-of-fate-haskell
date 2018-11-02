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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Data.Text.Lazy.IO (writeFile)
import qualified Text.Blaze.Html5 as H

import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Server.Common (renderTopOnlyPage)
import HabitOfFate.Story.Renderer.HTML (renderEventToHTML)
import HabitOfFate.Substitution (Substitutions)

main = do
  writeFile "forest.html" $
    renderTopOnlyPage "Forest Stories" [] [] Nothing $
      foldMap
        (\(category, category_stories) →
          H.h1 (H.toHtml category) ⊕ mconcat (intersperse H.hr (map renderEventToHTML category_stories))
        )
        Forest.stories
