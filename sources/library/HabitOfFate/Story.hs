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

import Data.String (IsString(..))
import Instances.TH.Lift ()
import GHC.Generics hiding (from, to)
import Language.Haskell.TH.Lift (Lift)

data Color = Red | Green | Blue deriving (Enum,Eq,Generic,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color | Introduce deriving (Eq,Generic,Lift,Ord,Read,Show)

data Paragraph =
    Style Style Paragraph
  | Merged (Seq Paragraph)
  | Text_ Text
  deriving (Eq,Generic,Lift,Ord,Read,Show)

type Event = [Paragraph]

instance IsString Paragraph where
  fromString = pack >>> Text_

instance Monoid Paragraph where
  mempty = Merged mempty
  mappend (Text_ x) ys | onull x = ys
  mappend xs (Text_ y) | onull y = xs
  mappend (Merged xs) (Merged ys) = xs ⊕ ys |> Merged
  mappend (Merged xs) y = xs ⊢ y |> Merged
  mappend x (Merged ys) = x ⊣ ys |> Merged
  mappend x y = [x,y] |> fromList |> Merged

allSpaces ∷ Text → Bool
allSpaces = allOf text (∈ " \t\r\n")
