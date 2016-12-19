{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Yaml hiding ((.=))
import System.Directory
import System.Environment
import System.FilePath

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

runData ∷ Data → IO ([[String]], Bool, Data)
runData d =
  runGame (d ^. game) (runCurrentQuest (d ^. quest))
  <&>
  \result →
    (result ^. paragraphs
    ,isNothing (result ^. returned_value)
    ,d & game .~ result ^. new_game
       & quest .~ result ^. returned_value
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
