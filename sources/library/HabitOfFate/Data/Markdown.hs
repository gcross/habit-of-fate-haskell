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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Markdown where

import HabitOfFate.Prelude

import CMark (commonmarkToHtml, commonmarkToLaTeX)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)
import GHC.Exts (IsString(..))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html)

newtype Markdown = Markdown { unwrapMarkdown ∷ Text } deriving (Eq,Ord,Read,Show)

instance IsString Markdown where
  fromString = pack >>> Markdown

instance ToJSON Markdown where
  toJSON = unwrapMarkdown >>> String

instance FromJSON Markdown where
  parseJSON = withText "markdown must be string-shaped" (Markdown >>> pure)

embolden ∷ Markdown → Markdown
embolden x = Markdown $ "**" ⊕ unwrapMarkdown x ⊕ "**"

renderMarkdownToHtml ∷ Markdown → Html
renderMarkdownToHtml = unwrapMarkdown >>> commonmarkToHtml [] >>> H.preEscapedToHtml

renderMarkdownToLaTeX ∷ Markdown → Text
renderMarkdownToLaTeX = unwrapMarkdown >>> commonmarkToLaTeX [] Nothing
