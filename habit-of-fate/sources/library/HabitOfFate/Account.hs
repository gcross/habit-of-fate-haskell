{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Account where

import HabitOfFate.Prelude

import Control.Monad.Random
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.UUID (UUID)
import Data.Yaml hiding ((.=))
import System.Directory
import System.Environment
import System.FilePath

import HabitOfFate.Credits
import HabitOfFate.Game
import HabitOfFate.Habit
import HabitOfFate.JSON ()
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = toJSON ∘ show

instance FromJSON StdGen where
  parseJSON = fmap read ∘ parseJSON

data Account = Account
  {   _password ∷ String
  ,   _habits ∷ Map UUID Habit
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe CurrentQuestState
  ,   _rng :: StdGen
  } deriving (Read,Show)
deriveJSON ''Account
makeLenses ''Account

newAccount ∷ String → IO Account
newAccount password = Account password mempty newGame Nothing <$> newStdGen

data RunAccountResult = RunAccountResult
  { _story ∷ Seq Paragraph
  , _quest_completed ∷ Bool
  , _new_data ∷ Account
  }
makeLenses ''RunAccountResult

runAccount ∷ Account → RunAccountResult
runAccount d =
  (flip runRand (d ^. rng)
   $
   runGame (d ^. game) (runCurrentQuest (d ^. quest))
  )
  &
  \(r, new_rng) →
    RunAccountResult
      (r ^. game_paragraphs)
      (isNothing (r ^. returned_value))
      (d & game .~ r ^. new_game
         & quest .~ r ^. returned_value
         & rng .~ new_rng
      )

readAccount ∷ FilePath → IO Account
readAccount = BS.readFile >=> either error return . decodeEither

writeAccount ∷ FilePath → Account → IO ()
writeAccount = encodeFile

getAccountFilePath ∷ IO FilePath
getAccountFilePath =
  getArgs >>= \case
    [] → getHomeDirectory <&> (</> ".habit")
    [filepath] → return filepath
    _ → error "Only one argument may be provided."

stillHasCredits ∷ Account → Bool
stillHasCredits d = (||)
  (d ^. game . credits . success /= 0)
  (d ^. game . credits . failure /= 0)

data HabitsToMark = HabitsToMark
  { _successes ∷ [UUID]
  , _failures ∷ [UUID]
  } deriving (Eq, Ord, Read, Show)
deriveJSON ''HabitsToMark
makeLenses ''HabitsToMark
