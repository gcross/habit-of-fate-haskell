{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data where

import HabitOfFate.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Random
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Yaml hiding ((.=))
import System.Directory
import System.Environment
import System.FilePath

import HabitOfFate.Game
import qualified HabitOfFate.Game as Game
import HabitOfFate.Habit
import HabitOfFate.JSON ()
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = toJSON ∘ show

instance FromJSON StdGen where
  parseJSON = fmap read ∘ parseJSON

data Data = Data
  {   _habits ∷ Map UUID Habit
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe CurrentQuestState
  ,   _rng :: StdGen
  } deriving (Read,Show)
deriveJSON ''Data
makeLenses ''Data

newData ∷ IO Data
newData = Data mempty newGame Nothing <$> newStdGen

data RunDataResult = RunDataResult
  { _story ∷ Seq Paragraph
  , _quest_completed ∷ Bool
  , _new_data ∷ Data
  }
makeLenses ''RunDataResult

runData ∷ Data → RunDataResult
runData d =
  (flip runRand (d ^. rng)
   $
   runGame (d ^. game) (runCurrentQuest (d ^. quest))
  )
  &
  \(r, new_rng) →
    RunDataResult
      (r ^. game_paragraphs)
      (isNothing (r ^. returned_value))
      (d & game .~ r ^. new_game
         & quest .~ r ^. returned_value
         & rng .~ new_rng
      )

readData ∷ FilePath → IO Data
readData = BS.readFile >=> either error return . decodeEither

writeData ∷ FilePath → Data → IO ()
writeData = encodeFile

getDataFilePath ∷ IO FilePath
getDataFilePath =
  getArgs >>= \case
    [] → getHomeDirectory <&> (</> ".habit")
    [filepath] → return filepath
    _ → error "Only one argument may be provided."

stillHasCredits ∷ Data → Bool
stillHasCredits d = (||)
  (d ^. game . Game.success_credits /= 0)
  (d ^. game . Game.failure_credits /= 0)
