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
  ,   _quest ∷ Maybe CurrentQuestState
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Data
makeLenses ''Data

newData ∷ Data
newData = Data newBehaviors newGame Nothing

runData ∷ Data → IO ([[String]], Data)
runData d =
  runGame (d ^. game) (runCurrentQuest (d ^. quest))
  <&>
  \result →
    (result ^. paragraphs
    ,d & game .~ result ^. new_game
       & quest .~ result ^. returned_value
    )
