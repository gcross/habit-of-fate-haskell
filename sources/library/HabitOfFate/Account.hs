{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import HabitOfFate.Credits
import HabitOfFate.Game
import HabitOfFate.Habit
import HabitOfFate.Quests
import HabitOfFate.Story
import HabitOfFate.TH

instance ToJSON StdGen where
  toJSON = show >>> toJSON

instance FromJSON StdGen where
  parseJSON = parseJSON >>> fmap read

data Account = Account
  {   _password ∷ Text
  ,   _habits ∷ Habits
  ,   _game ∷ GameState
  ,   _quest ∷ Maybe CurrentQuestState
  ,   _rng :: StdGen
  } deriving (Read,Show)
deriveJSON ''Account
makeLenses ''Account

newAccount ∷ Text → IO Account
newAccount password =
  Account
    <$> (
          makePassword (encodeUtf8 password) 17
          >>=
          (decodeUtf8 >>> evaluate)
        )
    <*> pure def
    <*> pure newGame
    <*> pure Nothing
    <*> newStdGen

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
  (d ^. game . credits . successes /= 0)
  (d ^. game . credits . failures /= 0)

data HabitsToMark = HabitsToMark
  { _succeeded ∷ [UUID]
  , _failed ∷ [UUID]
  } deriving (Eq, Ord, Read, Show)
deriveJSON ''HabitsToMark
makeLenses ''HabitsToMark
