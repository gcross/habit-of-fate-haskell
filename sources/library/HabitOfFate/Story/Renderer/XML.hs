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

module HabitOfFate.Story.Renderer.XML (renderStoryToXMLText) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Text.XML

import HabitOfFate.Story

renderParagraphToNodes ∷ Paragraph → [Node]
renderParagraphToNodes paragraph =
  case recurse paragraph of
    [] → []
    nodes → [NodeElement $ Element "p" mempty nodes]
  where
    recurse ∷ Paragraph → [Node]
    recurse (Style style p)
      | null nested = []
      | otherwise =
          let tag = case style of
                Bold → "b"
                Underline → "u"
                Color Red → "red"
                Color Blue → "blue"
                Color Green → "green"
                Introduce → "introduce"
          in Element tag mempty nested |> NodeElement |> singleton
      where
        nested = recurse p
    recurse (Merged children) = concatMap recurse children
    recurse (Text_ t) = [NodeContent t]

renderEventToNode ∷ Event → Node
renderEventToNode =
  unwrapGenEvent
  >>>
  concatMap renderParagraphToNodes
  >>>
  Element "event" mempty
  >>>
  NodeElement

renderQuestToNode ∷ Quest → Node
renderQuestToNode =
  foldr (renderEventToNode >>> (:)) []
  >>>
  Element "quest" mempty
  >>>
  NodeElement

renderStoryToDocument ∷ Story → Document
renderStoryToDocument =
  foldr (renderQuestToNode >>> (:)) []
  >>>
  Element "story" mempty
  >>>
  (\n → Document (Prologue [] Nothing []) n [])

renderStoryToXMLText ∷ Story → Lazy.Text
renderStoryToXMLText = renderStoryToDocument >>> renderText def
