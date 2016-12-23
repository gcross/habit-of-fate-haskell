{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data where

import Control.Lens
import Control.Monad
import Control.Monad.Random
import qualified Data.ByteString as BS
import Data.Function
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Yaml hiding ((.=))
import System.Directory
import System.Environment
import System.FilePath
import System.Random

import HabitOfFate.Behaviors.Habit
import HabitOfFate.Game
import qualified HabitOfFate.Game as Game
import HabitOfFate.Quests
import HabitOfFate.TH
import HabitOfFate.Unicode

instance ToJSON StdGen where
  toJSON = toJSON ∘ show

instance FromJSON StdGen where
  parseJSON = fmap read ∘ parseJSON

instance Eq StdGen where
  (==) = (==) `on` show

data Data = Data
  {   _habits ∷ Seq Habit
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe CurrentQuestState
  ,   _rng :: StdGen
  } deriving (Eq,Read,Show)
deriveJSON ''Data
makeLenses ''Data

newData ∷ IO Data
newData = Data Seq.empty newGame Nothing <$> newStdGen

data RunDataResult = RunDataResult
  { _paragraphs ∷ [[String]]
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
