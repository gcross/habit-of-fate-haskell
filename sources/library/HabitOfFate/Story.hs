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

data GenParagraph α =
    Style Style (GenParagraph α)
  | Merged (Seq (GenParagraph α))
  | Text_ α
  deriving (Eq,Foldable,Functor,Generic,Lift,Ord,Read,Show,Traversable)

replaceTextM ∷ Applicative f ⇒ (α → f (GenParagraph β)) → GenParagraph α → f (GenParagraph β)
replaceTextM f (Style s x) = Style s <$> replaceTextM f x
replaceTextM f (Merged xs) = Merged <$> traverse (replaceTextM f) xs
replaceTextM f (Text_ t) = f t

newtype GenEvent α = GenEvent { unwrapGenEvent ∷ [GenParagraph α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenEvent

newtype GenQuest α = GenQuest { unwrapGenQuest ∷ [GenEvent α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenQuest

newtype GenStory α = GenStory { unwrapGenStory ∷ [GenQuest α] }
  deriving (Eq,Generic,Lift,Monoid,Ord,Read,Show)
makeWrapped ''GenStory

paragraphs ∷ IndexedTraversal Int (GenEvent α) (GenEvent β) (GenParagraph α) (GenParagraph β)
paragraphs f (GenEvent ps) = GenEvent <$> (traversed f ps)

events ∷ IndexedTraversal Int (GenQuest α) (GenQuest β) (GenEvent α) (GenEvent β)
events f (GenQuest ps) = GenQuest <$> (traversed f ps)

quests ∷ IndexedTraversal Int (GenStory α) (GenStory β) (GenQuest α) (GenQuest β)
quests f (GenStory ps) = GenStory <$> (traversed f ps)

createEvent ∷ Foldable t ⇒ t Paragraph → Event
createEvent = toList >>> GenEvent

createQuest ∷ Foldable t ⇒ t Event → Quest
createQuest = toList >>> GenQuest

createStory ∷ Foldable t ⇒ t Quest → Story
createStory = toList >>> GenStory

eventToLists ∷ GenEvent α → [GenParagraph α]
eventToLists = unwrapGenEvent

questToLists ∷ GenQuest α → [[GenParagraph α]]
questToLists = unwrapGenQuest >>> fmap eventToLists

storyToLists ∷ GenStory α → [[[GenParagraph α]]]
storyToLists = unwrapGenStory >>> fmap questToLists

type Paragraph = GenParagraph Text
type Event = GenEvent Text
type Quest = GenQuest Text
type Story = GenStory Text

instance IsString Paragraph where
  fromString = pack >>> Text_

data SubText = Key Text | Literal Text deriving (Eq,Generic,Lift,Ord,Read,Show)
makePrisms ''SubText

type SubParagraph = GenParagraph SubText
type SubEvent = GenEvent SubText
type SubQuest = GenQuest SubText
type SubStory = GenStory SubText

class HasLiterals α where
  literals ∷ IndexedFold Int (GenParagraph α) Text

instance HasLiterals Text where
  literals = folded

instance HasLiterals SubText where
  literals = folded . _Literal

class GenText α where
  textIsNull ∷ α → Bool
  textIsAllSpaces ∷ α → Bool

instance GenText Text where
  textIsNull = onull

  textIsAllSpaces = allSpaces

instance GenText SubText where
  textIsNull (Literal t) = onull t
  textIsNull _ = False

  textIsAllSpaces (Literal t) = allSpaces t
  textIsAllSpaces _ = False

instance GenText α ⇒ Monoid (GenParagraph α) where
  mempty = Merged mempty
  mappend (Text_ x) ys | textIsNull x = ys
  mappend xs (Text_ y) | textIsNull y = xs
  mappend (Merged xs) (Merged ys) = xs ⊕ ys |> Merged
  mappend (Merged xs) y = xs ⊢ y |> Merged
  mappend x (Merged ys) = x ⊣ ys |> Merged
  mappend x y = [x,y] |> fromList |> Merged

textFromParagraph ∷ Paragraph → Text
textFromParagraph = fold

allSpaces ∷ Text → Bool
allSpaces = allOf text (∈ " \t\r\n")

clearNullElements ∷ (Wrapped s, Unwrapped s ~ [t]) ⇒ (t → Bool) → s → s
clearNullElements isNull = _Wrapped' %~ filter (not <<< isNull)

dropEmptyThingsFromStory ∷ GenText α ⇒ GenStory α → GenStory α
dropEmptyThingsFromStory =
  quests . events %~ clearNullElements (all textIsAllSpaces)
  >>>
  quests %~ clearNullElements (nullOf paragraphs)
  >>>
  clearNullElements (nullOf events)
