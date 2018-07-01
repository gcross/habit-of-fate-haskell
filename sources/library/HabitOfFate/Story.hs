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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import Instances.TH.Lift ()
import GHC.Generics hiding (from, to)
import Language.Haskell.TH.Lift (Lift)

import HabitOfFate.Substitution

data Color = Red | Green | Blue deriving (Enum,Eq,Generic,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color | Introduce deriving (Eq,Generic,Lift,Ord,Read,Show)

data Paragraph =
    StyleP Style Paragraph
  | MergedP (Seq Paragraph)
  | SubstitutionP SubstitutionData
  | TextP Text
  deriving (Eq,Generic,Lift,Ord,Read,Show)

type Event = [Paragraph]

allSpaces ∷ Text → Bool
allSpaces = allOf text (∈ " \t\r\n")

instance Monoid Paragraph where
  -- Drop empty sequences when possible
  mempty = MergedP mempty

  -- Drop empty blocks of text when possible
  mappend (TextP x) ys | allSpaces x = ys
  mappend xs (TextP y) | allSpaces y = xs

  -- Merge MergedP lists of nodes when possible
  mappend (MergedP xs) (MergedP ys) = MergedP $ xs ⊕ ys

  -- Append or prepend to an existing MergeP node when possible
  mappend (MergedP xs) y = MergedP $ xs ⊢ y
  mappend x (MergedP ys) = MergedP $ x ⊣ ys

  -- Create a new MergeP node when nothing else is possible
  mappend x y = MergedP $ singleton x ⊕ singleton y
