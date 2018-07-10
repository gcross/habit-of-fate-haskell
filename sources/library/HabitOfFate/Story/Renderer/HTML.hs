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

module HabitOfFate.Story.Renderer.HTML (renderEventToHTML) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Data.Void
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.XML

import HabitOfFate.Story

renderEventToHTML ∷ Event → H.Html
renderEventToHTML = foldMap (go >>> H.p)
  where
    go ∷ Paragraph → H.Html
    go (StyleP style p) = H.span ! A.class_ cls $ go p
     where
      cls = case style of
       Bold → "bold-text"
       Underline → "underlined-text"
       Color Red → "red-text"
       Color Blue → "blue-text"
       Color Green → "green-text"
       Introduce → "introduce-text"
    go (MergedP children) = foldMap go children
    go (TextP t) = H.toHtml t
    go (SubstitutionP sub) = absurd sub
