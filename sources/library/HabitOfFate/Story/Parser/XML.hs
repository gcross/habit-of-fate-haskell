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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story.Parser.XML
  ( parseEventFromDocument
  , parseEventFromText
  , parseStoryFromDocument
  , parseStoryFromText
  ) where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Exception (Exception)
import qualified Data.Text.Lazy as Lazy
import Data.Typeable (Typeable)
import Text.XML

import HabitOfFate.Story

data StoryParseException = StoryParseException String deriving (Eq,Show,Typeable)
instance Exception StoryParseException where

parseContainer ∷ MonadThrow m ⇒ Text → ([Node] → m α) → Node → m α
parseContainer expected_tag parseChildren node =
  case node of
    NodeInstruction _ →  throwM $ StoryParseException "unexpected XML instruction"
    NodeComment _ → parseChildren []
    NodeContent t
      | allSpaces t → parseChildren []
      | otherwise →  throwM $ StoryParseException $ "unexpected non-whitespace text outside of <p>"
    NodeElement (Element (Name tag _ _) attrs childs)
      | tag /= expected_tag →
           throwM $ StoryParseException [i|expected <#{expected_tag}> but got <#{tag}>|]
      | attrs |> (not <<< null) →
           throwM $ StoryParseException [i|expected no attributes in <#{tag}>"|]
      | otherwise → parseChildren childs

parseStoryFromNodes ∷ MonadThrow m ⇒ [Node] → m Story
parseStoryFromNodes = mapM (parseContainer "event" parseEventFromNodes)

parseEventFromNodes ∷ MonadThrow m ⇒ [Node] → m Event
parseEventFromNodes =
  mapM (parseContainer "p" parseParagraphFromNodes)
  >>>
  fmap (filter (not <<< null))

parseParagraphFromNodes ∷ MonadThrow m ⇒ [Node] → m Paragraph
parseParagraphFromNodes = mapM parseParagraphChild >>> fmap mconcat
  where
    parseParagraphChild ∷ MonadThrow m ⇒ Node → m Paragraph
    parseParagraphChild (NodeInstruction _) =  throwM $ StoryParseException "unexpected XML instruction"
    parseParagraphChild (NodeComment _) = return mempty
    parseParagraphChild (NodeContent t) = return $ Text_ t
    parseParagraphChild (NodeElement (Element (Name tag _ _) attrs childs)) =
      case lookup tag tags of
        Nothing →  throwM $ StoryParseException [i|unexpected tag <#{tag}>|]
        Just style
          | not <<< null $ attrs →  throwM $ StoryParseException [i|<#{tag}> had unexpected attributes|]
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

parseStoryFromDocument ∷ MonadThrow m ⇒ Document → m Story
parseStoryFromDocument =
  documentRoot
  >>>
  NodeElement
  >>>
  parseContainer "story" parseStoryFromNodes

parseEventFromDocument ∷ MonadThrow m ⇒ Document → m Event
parseEventFromDocument =
  documentRoot
  >>>
  NodeElement
  >>>
  parseContainer "event" parseEventFromNodes

parseStoryFromText ∷ MonadThrow m ⇒ Lazy.Text → m Story
parseStoryFromText = (parseText def >>> either throwM pure) >=> parseStoryFromDocument

parseEventFromText ∷ MonadThrow m ⇒ Lazy.Text → m Event
parseEventFromText = (parseText def >>> either throwM pure) >=> parseEventFromDocument
