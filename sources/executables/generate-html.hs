{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

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

import System.IO (putStrLn)

import HabitOfFate.Pages
import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Story

addSuffix ∷ Text → Text
addSuffix x = x ⊕ case unsnoc x of
  Nothing → "index.html"
  Just (_, '/') → "index.html"
  _ → ".html"

processPageIds ∷ Text → [(Text, Page)] → [(Text, Page)]
processPageIds prefix = modifyAllPageIds (\x → prefix_with_slash ⊕ addSuffix x)
 where
  prefix_with_slash = prefix ⊕ "/"

main ∷ IO ()
main = do
  pagemap ←
    [ singleton
      ("index.html", Page
        "The Adventure Begins"
        ""
        (Choices "Which quest shall we begin?" $
          map (second (⊕ "/index.html"))
          [ ("The search in the Wicked Forest.", "forest")
          ]
        )
      )
    , processPageIds "forest" Forest.pages
    ]
    |> concat
    |> constructAndValidatePageMap
  forM_ (walkPages "index.html" pagemap) $ show >>> putStrLn
