{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Account where

import HabitOfFate.Prelude hiding ((.=))

import Control.Exception
import Control.Monad.Random
import Crypto.PasswordStore
import Data.Aeson
import Data.Map (Map)
import Data.UUID (UUID)
import System.Directory
import System.FilePath

import HabitOfFate.Credits
import HabitOfFate.Game
import HabitOfFate.Habit
import HabitOfFate.JSON ()
import HabitOfFate.Logging
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = toJSON ∘ show

instance FromJSON StdGen where
  parseJSON = fmap read ∘ parseJSON

data Account = Account
  {   _password ∷ Text
  ,   _habits ∷ Map UUID Habit
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe CurrentQuestState
  ,   _rng :: StdGen
  } deriving (Read,Show)
deriveJSON ''Account
makeLenses ''Account

newAccount ∷ Text → IO Account
newAccount password =
  Account
    <$> (do
          logIO [i|111|]
          logIO [i|   .Z #{encodeUtf8 password}|]
          p1 ← makePassword (encodeUtf8 password) 15
          logIO [i|   .A #{p1}|]
          p2 ← evaluate ∘ decodeUtf8 $ p1
          logIO [i|   .B #{p2}|]
          return p2
        )
    <*> (logIO "222" >> pure mempty)
    <*> (logIO "333" >> pure newGame)
    <*> (logIO "444" >> pure Nothing)
    <*> (logIO "555" >> newStdGen)

passwordIsValid ∷ Text → Account → Bool
passwordIsValid password_ account =
  verifyPassword (encodeUtf8 password_) (encodeUtf8 $ account ^. password)

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
