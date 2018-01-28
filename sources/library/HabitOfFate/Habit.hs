{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Web.Scotty (Parsable(..))

import HabitOfFate.JSON ()
import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Bounded,Enum,Eq,Ord,Read,Show)
deriveJSON ''Scale

instance Parsable Scale where
  parseParam =
    unpack
    >>>
    readMaybe
    >>>
    maybe (Left "Unrecognized scale.") Right

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
  { _name ∷ String
  , _difficulty ∷ Scale
  , _importance ∷ Scale
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

instance Default Habit where
  def = Habit "" Medium Medium
