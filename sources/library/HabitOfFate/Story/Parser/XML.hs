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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story.Parser.XML
  ( parseStoryFromDocument
  , parseStoryFromText
  ) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Text.XML

import HabitOfFate.Story

parseContainer ∷ Text → ([Node] → Either String α) → Node → Either String α
parseContainer expected_tag parseChildren node =
  case node of
    NodeInstruction _ → fail "unexpected XML instruction"
    NodeComment _ → parseChildren []
    NodeContent t
      | allSpaces t → parseChildren []
      | otherwise → fail $ "unexpected non-whitespace text outside of <p>"
    NodeElement (Element (Name tag _ _) attrs childs)
      | tag /= expected_tag →
          fail [i|expected <#{expected_tag}> but got <#{tag}>|]
      | attrs |> (not <<< null) →
          fail [i|expected no attributes in <#{tag}>"|]
      | otherwise → parseChildren childs

parseStoryFromNodes ∷ [Node] → Either String Story
parseStoryFromNodes =
  mapM (parseContainer "quest" parseQuestFromNodes)
  >>>
  fmap GenStory

parseQuestFromNodes ∷ [Node] → Either String Quest
parseQuestFromNodes =
  mapM (parseContainer "event" parseEventFromNodes)
  >>>
  fmap GenQuest

parseEventFromNodes ∷ [Node] → Either String Event
parseEventFromNodes =
  mapM (parseContainer "p" parseParagraphFromNodes)
  >>>
  fmap (GenEvent <<< filter (not <<< nullOf folded))

parseParagraphFromNodes ∷ [Node] → Either String Paragraph
parseParagraphFromNodes = mapM parseParagraphChild >>> fmap mconcat
  where
    parseParagraphChild ∷ Node → Either String Paragraph
    parseParagraphChild (NodeInstruction _) = fail "unexpected XML instruction"
    parseParagraphChild (NodeComment _) = return mempty
    parseParagraphChild (NodeContent t) = return $ Text_ t
    parseParagraphChild (NodeElement (Element (Name tag _ _) attrs childs)) =
      case lookup tag tags of
        Nothing → fail [i|unexpected tag <#{tag}>|]
        Just style
          | not <<< null $ attrs → fail [i|<#{tag}> had unexpected attributes|]
          | otherwise → Style style <$> parseParagraphFromNodes childs
      where
        tags ∷ Map Text Style
        tags = mapFromList
          [ ("b", Bold)
          , ("u", Underline)
          , ("red", Color Red)
          , ("blue", Color Blue)
          , ("green", Color Green)
          , ("introduce", Introduce)
          ]

parseStoryFromDocument ∷ Document → Either String Story
parseStoryFromDocument =
  documentRoot
  >>>
  NodeElement
  >>>
  parseContainer "story" parseStoryFromNodes

parseStoryFromText ∷ Lazy.Text → Either String Story
parseStoryFromText = (parseText def >>> _Left %~ show) >=> parseStoryFromDocument
