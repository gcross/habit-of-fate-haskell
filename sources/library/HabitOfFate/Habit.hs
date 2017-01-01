{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.UUID (UUID)

import HabitOfFate.JSON
import HabitOfFate.TH

data Habit = Habit
  { _name ∷ String
  , _success_credits ∷ Double
  , _failure_credits ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

generateHabitDoc ∷ Text → UUID → Habit → Value
generateHabitDoc = generateDocWithObject "habit"

generateHabitsDoc ∷ Text → Map UUID Habit → Value
generateHabitsDoc = generateDocWithObjects "habit"

parseHabitDoc ∷ Value → Parser (Maybe UUID, Habit)
parseHabitDoc = parseDocWithObject "habit"

parseHabitsDoc ∷ Value → Parser (Map UUID Habit)
parseHabitsDoc = parseDocWithObjects "habit"
