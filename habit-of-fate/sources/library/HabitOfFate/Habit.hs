{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import Control.Lens
import Data.UUID

import HabitOfFate.Credits
import HabitOfFate.JSON ()
import HabitOfFate.TH

data Habit = Habit
  { _name ∷ String
  , __credits ∷ Credits
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

instance HasCredits Habit where
  credits = _credits
