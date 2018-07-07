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

module HabitOfFate.Story.Renderer.HTML (renderEventToHTMLNodes) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.Void
import Text.XML

import HabitOfFate.Story

renderParagraphToNodes ∷ Paragraph → [Node]
renderParagraphToNodes paragraph =
  case recurse paragraph of
    [] → []
    nodes → [NodeElement $ Element "p" mempty nodes]
  where
    recurse ∷ Paragraph → [Node]
    recurse (StyleP style p)
      | null nested = []
      | otherwise =
          let cls = case style of
                Bold → "bold-text"
                Underline → "underlined-text"
                Color Red → "red-text"
                Color Blue → "blue-text"
                Color Green → "green-text"
                Introduce → "introduce-text"
          in Element "span" (mapFromList [("class", cls)]) nested |> NodeElement |> singleton
      where
        nested = recurse p
    recurse (MergedP children) = concatMap recurse children
    recurse (TextP t) = [NodeContent t]
    recurse (SubstitutionP sub) = absurd sub

renderEventToHTMLNodes ∷ Event → [Node]
renderEventToHTMLNodes = concatMap renderParagraphToNodes
