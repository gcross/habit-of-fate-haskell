{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Data.Aeson

import HabitOfFate.JSON ()
import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Enum,Eq,Ord,Read,Show)

showScale ∷ Scale → Text
showScale VeryLow = "very low"
showScale Low = "low"
showScale Medium = "medium"
showScale High = "high"
showScale VeryHigh = "very high"

readMaybeScale ∷ Text → Maybe Scale
readMaybeScale "very low" = Just VeryLow
readMaybeScale "low" = Just Low
readMaybeScale "medium" = Just Medium
readMaybeScale "high" = Just High
readMaybeScale "very high" = Just VeryHigh
readMaybeScale _ = Nothing

instance ToJSON Scale where
  toJSON = String ∘ showScale

instance FromJSON Scale where
  parseJSON (String x) =
    maybe (fail "invalid value for scale") pure
    ∘
    readMaybeScale
    $
    x
  parseJSON _ = fail "value for scale must be a string"

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
