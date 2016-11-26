{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Behaviors.Habit where

import Control.Lens (makeLenses)

import HabitOfFate.TH

data Habit = Habit
  { _name ∷ String
  , _success_credits ∷ Int
  , _failure_credits ∷ Int
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit
