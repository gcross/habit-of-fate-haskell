{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data where

import Control.Lens

import HabitOfFate.Behaviors
import HabitOfFate.Game
import HabitOfFate.Quests
import HabitOfFate.TH

data Data = Data
  {   _behaviors ∷ Behaviors
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe QuestState
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Data
makeLenses ''Data

newData ∷ Data
newData = Data newBehaviors newGame Nothing
