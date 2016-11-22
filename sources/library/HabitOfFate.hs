{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Set

import HabitOfFate.Game
import qualified HabitOfFate.Quests as Quests
import HabitOfFate.TH

data Data = Data
  {   _game_state ∷ GameState
  ,   _quest ∷ Maybe Quests.State
  } deriving (Read, Show)
deriveJSON ''Data
makeLenses ''Data

new ∷ Data
new = Data newGame Nothing

act ∷ GameInput → Data → IO (Data, [[String]])
act input (Data old_game_state old_quest) = do
  GameResult{..} ← runGame old_game_state $ Quests.act input old_quest
  return $ (Data new_game_state returned_value, paragraphs)
