{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Data.Aeson
import Data.Default

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (Read(readPrec), readMaybe)

import HabitOfFate.JSON ()
import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Enum,Eq,Ord,Read,Show)
deriveJSON ''Scale

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
