{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Set

import HabitOfFate.MonadGame
import HabitOfFate.Quest
import qualified HabitOfFate.Quests as Quests
import HabitOfFate.TH

data Data = Data
    {   _game ∷ GameState
    ,   _quest ∷ Maybe Quests.State
    } deriving (Read, Show)
deriveJSON ''Data
makeLenses ''Data

new ∷ Data
new = Data newGame Nothing

act ∷ MonadGame m ⇒ GoodBad → Data → m Data
act goodbad (Data old_game old_quest) = do
  (new_quest, new_game) ← runStateT (Quests.act goodbad old_quest) old_game
  return $ Data new_game new_quest
