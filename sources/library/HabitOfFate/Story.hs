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
import Data.Void
import Instances.TH.Lift ()
import GHC.Generics hiding (from, to)
import Language.Haskell.TH.Lift (Lift)

import HabitOfFate.Substitution

data Color = Red | Green | Blue deriving (Enum,Eq,Generic,Lift,Ord,Read,Show)

data Style = Bold | Underline | Color Color | Introduce deriving (Eq,Generic,Lift,Ord,Read,Show)

data GenParagraph sub =
    StyleP Style (GenParagraph sub)
  | MergedP (Seq (GenParagraph sub))
  | TextP Text
  | SubstitutionP sub
  deriving (Eq,Generic,Lift,Ord,Read,Show)

type GenEvent sub = [GenParagraph sub]

type Paragraph = GenParagraph Void
type SubParagraph = GenParagraph SubstitutionData

type Event = GenEvent Void
type SubEvent = GenEvent SubstitutionData

instance IsString (GenParagraph s) where
  fromString = pack >>> TextP

instance Monoid (GenParagraph s) where
  mempty = MergedP mempty
  mappend (TextP x) ys | onull x = ys
  mappend xs (TextP y) | onull y = xs
  mappend (MergedP xs) (MergedP ys) = MergedP $ xs ⊕ ys
  mappend (MergedP xs) y = MergedP $ xs ⊢ y
  mappend x (MergedP ys) = MergedP $ x ⊣ ys
  mappend x y = MergedP $ fromList [x,y]

mapOverEventSubstitutions ∷ (SubstitutionData → Text) → SubEvent → Event
mapOverEventSubstitutions f = map go
 where
  go (StyleP style p) = StyleP style (go p)
  go (MergedP ps) = MergedP (fmap go ps)
  go (TextP t) = TextP t
  go (SubstitutionP s) = TextP $ f s

substitute ∷ HashMap Text Gendered → SubEvent → Event
substitute table =
  mapOverEventSubstitutions $ \s →
    case lookupAndApplySubstitution table s of
      Left e → "[" ⊕ (e |> show |> pack) ⊕ "]"
      Right t → t

replaceSubstitutionsWithKeys ∷ SubEvent → Event
replaceSubstitutionsWithKeys =
  mapOverEventSubstitutions $ \s → "[" ⊕ (s ^. key_) ⊕ "]"
