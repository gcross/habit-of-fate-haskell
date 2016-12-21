{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Behaviors where

import Control.Lens (makeLenses)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import HabitOfFate.Behaviors.Habit (Habit)
import qualified HabitOfFate.Behaviors.Habit as Habit
import HabitOfFate.TH

data Behaviors = Behaviors
  { _habits ∷ Seq Habit
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Behaviors
makeLenses ''Behaviors

newBehaviors ∷ Behaviors
newBehaviors = Behaviors Seq.empty
