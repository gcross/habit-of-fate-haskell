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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Data.Aeson
import qualified Data.Text.Lazy as Lazy

import Text.Blaze (Markup, ToMarkup(..))

import Web.Scotty (Parsable(..))

import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Bounded,Enum,Eq,Ord,Read,Show)
deriveJSON ''Scale

instance Parsable Scale where
  parseParam p =
    p
    |> unpack
    |> readMaybe
    |> maybe (Left $ "Unrecognized scale \"" ⊕ p ⊕ "\".") Right

instance ToMarkup Scale where
  toMarkup = displayScale >>> toMarkup

showScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ String → α → String
showScale name =
  (^. _Wrapped')
  >>>
  show
  >>>
  (⊕ name)

toMarkupScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ Text → α → Markup
toMarkupScale name =
  (^. _Wrapped')
  >>>
  displayScale
  >>>
  (\scale_str → scale_str ⊕ " " ⊕ name)
  >>>
  toMarkup

readPrecsScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ String → Int → String → [(α, String)]
readPrecsScale name p =
  readsPrec p
  >>>
  \case
    [(scale, rest)]
      | rest == name → [(scale ^. _Unwrapped', "")]
      | otherwise → []
    _ → []

parseParamScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ Lazy.Text → Lazy.Text → Either Lazy.Text α
parseParamScale name p =
  case words p of
    [scale_text, name_]
      | name_ == name →
          parseParam scale_text
          |> bimap
              (\error_message →
                 "Error parsing scale \"" ⊕ scale_text ⊕ "\" in \"" ⊕ p ⊕ "\": " ⊕ error_message)
              (^. _Unwrapped')
      | otherwise → Left $ "Second word in \"%" ⊕ p ⊕ "\" was not " ⊕ name
    _ → Left $ "Wrong number of words in \"" ⊕ p ⊕ "\""

newtype Difficulty = Difficulty { unwrapDifficulty ∷ Scale }
  deriving (Bounded,Enum,Eq,FromJSON,Ord,ToJSON)

instance Wrapped Difficulty where
  type Unwrapped Difficulty = Scale
  _Wrapped' = iso unwrapDifficulty Difficulty

instance Show Difficulty where
  show = showScale "Difficulty"

instance ToMarkup Difficulty where
  toMarkup = toMarkupScale "Difficulty"

instance Read Difficulty where
  readsPrec = readPrecsScale "Difficulty"

instance Parsable Difficulty where
  parseParam = parseParamScale "Difficulty"

newtype Importance = Importance { unwrapImportance ∷ Scale }
  deriving (Bounded,Enum,Eq,FromJSON,Ord,ToJSON)

instance Wrapped Importance where
  type Unwrapped Importance = Scale
  _Wrapped' = iso unwrapImportance Importance

instance Show Importance where
  show = showScale "Importance"

instance ToMarkup Importance where
  toMarkup = toMarkupScale "Importance"

instance Read Importance where
  readsPrec = readPrecsScale "Importance"

instance Parsable Importance where
  parseParam = parseParamScale "Importance"

scales ∷ [Scale]
scales = enumFromTo minBound maxBound

displayScale ∷ Scale → Text
displayScale VeryLow = "Very Low"
displayScale Low = "Low"
displayScale Medium = "Medium"
displayScale High = "High"
displayScale VeryHigh = "Very High"

scaleFactor ∷ Scale → Double
scaleFactor VeryLow = 1/4
scaleFactor Low = 1/2
scaleFactor Medium = 1
scaleFactor High = 2
scaleFactor VeryHigh = 4

data Habit = Habit
  { _name ∷ Text
  , _difficulty ∷ Difficulty
  , _importance ∷ Importance
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit

name ∷ Lens' Habit Text
name = lens _name (\old new_name → old { _name = new_name })

difficulty ∷ Lens' Habit Scale
difficulty =
  lens
    (_difficulty >>> unwrapDifficulty)
    (\old new_scale → old { _difficulty = Difficulty new_scale })

importance ∷ Lens' Habit Scale
importance =
  lens
    (_importance >>> unwrapImportance)
    (\old new_scale → old { _importance = Importance new_scale })

instance Default Habit where
  def = Habit "" (Difficulty Medium) (Importance Medium)
