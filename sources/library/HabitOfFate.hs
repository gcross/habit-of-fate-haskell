{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.ByteString as BS
import Data.Set
import Data.Yaml
import System.Directory
import System.FilePath

import HabitOfFate.Game
import qualified HabitOfFate.Quests as Quests
import HabitOfFate.TH

data Data = Data
  {   _game_state ∷ GameState
  ,   _quest ∷ Maybe Quests.State
  } deriving (Read, Show)
deriveJSON ''Data
makeLenses ''Data

act ∷ GameInput → Data → IO (Data, [[String]])
act input (Data old_game_state old_quest) = do
  GameResult{..} ← runGame old_game_state $ Quests.act input old_quest
  return $ (Data new_game_state returned_value, paragraphs)

actWithFile ∷ GameInput → FilePath → IO [[String]]
actWithFile input filepath = do
  exists ← doesFileExist filepath
  game_data ← if exists
    then BS.readFile filepath >>= either error return . decodeEither
    else return $ Data newGame Nothing
  (new_game_data, paragraphs) ← act input game_data
  encodeFile filepath new_game_data
  return paragraphs