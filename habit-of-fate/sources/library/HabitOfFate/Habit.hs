{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Data.Aeson

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (Read(readPrec), readMaybe)

import HabitOfFate.JSON ()
import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Enum,Eq,Ord)

instance Show Scale where
  show VeryLow = "very low"
  show Low = "low"
  show Medium = "medium"
  show High = "high"
  show VeryHigh = "very high"

instance Read Scale where
  readPrec = ReadPrec.lift $
    choice
      [ string "very low" $> VeryLow
      , string "low" $> Low
      , string "medium" $> Medium
      , string "high" $> High
      , string "very high" $> VeryHigh
      ]

instance ToJSON Scale where
  toJSON = String ∘ pack ∘ show

instance FromJSON Scale where
  parseJSON (String x) =
    maybe (fail "invalid value for scale") pure
    ∘
    readMaybe
    ∘
    unpack
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
