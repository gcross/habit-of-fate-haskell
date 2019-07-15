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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Account where

import HabitOfFate.Prelude

import Control.Exception
import Control.Monad.Random (StdGen, newStdGen)
import Crypto.PasswordStore
import Data.Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , ToJSON(..)
  , ToJSONKey(..)
  , (.:)
  , withObject
  , withText
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Web.Scotty (Parsable)

import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Group
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Mark
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.QuestState
import HabitOfFate.JSON
import HabitOfFate.Quests

instance ToJSON StdGen where
  toJSON = show >>> toJSON

instance FromJSON StdGen where
  parseJSON = parseJSON >>> fmap read

type Groups = ItemsSequence Group

type Marks = Seq Mark

data Account = Account
  {   _password_ ∷ Text
  ,   _habits_ ∷ ItemsSequence Habit
  ,   _marks_ ∷ Marks
  ,   _quest_state_ ∷ QuestState Markdown
  ,   _rng_ ∷ StdGen
  ,   _configuration_ ∷ Configuration
  ,   _last_seen_ ∷ UTCTime
  ,   _groups_ ∷ Groups
  ,   _deeds_ ∷ [Deed]
  } deriving (Read,Show)

instance ToJSON Account where
  toJSON Account{..} = runJSONBuilder $ do
    writeField "password" _password_
    writeField "habits" _habits_
    writeField "marks" _marks_
    writeField "quest" _quest_state_
    writeField "rng" _rng_
    writeField "configuration" _configuration_
    writeTextField "last_seen" $ pack $ formatTime defaultTimeLocale "%FT%T" _last_seen_
    writeField "groups" _groups_
    writeField "deeds" _deeds_

instance FromJSON Account where
  parseJSON = withObject "account must be object-shaped" $ \o →
    Account
      <$> (o .: "password")
      <*> (o .: "habits")
      <*> (o .: "marks")
      <*> (o .: "quest")
      <*> (o .: "rng")
      <*> (o .: "configuration")
      <*> (o .: "last_seen" >>= withText "UTCTime must be a string" (
            unpack >>> parseTimeM False defaultTimeLocale "%FT%T"
          ))
      <*> (o .: "groups")
      <*> (o .: "deeds")

password_ ∷ Lens' Account Text
password_ f a = (\nx → a {_password_ = nx}) <$> f (_password_ a)

habits_ ∷ Lens' Account (ItemsSequence Habit)
habits_ f a = (\nx → a {_habits_ = nx}) <$> f (_habits_ a)

marks_ ∷ Lens' Account Marks
marks_ f a = (\nx → a {_marks_ = nx}) <$> f (_marks_ a)

quest_state_ ∷ Lens' Account (QuestState Markdown)
quest_state_ f a = (\nx → a {_quest_state_ = nx}) <$> f (_quest_state_ a)

configuration_ ∷ Lens' Account Configuration
configuration_ f a = (\nx → a {_configuration_ = nx}) <$> f (_configuration_ a)

last_seen_ ∷ Getter Account UTCTime
last_seen_ = to _last_seen_

groups_ ∷ Lens' Account Groups
groups_ f a = (\nx → a {_groups_ = nx}) <$> f (_groups_ a)

deeds_ ∷ Lens' Account [Deed]
deeds_ f a = (\nx → a {_deeds_ = nx}) <$> f (_deeds_ a)

newAccount ∷ Text → IO Account
newAccount password =
  Account
    <$> (
          makePassword (encodeUtf8 password) 17
          >>=
          (decodeUtf8 >>> evaluate)
        )
    <*> pure def
    <*> pure def
    <*> randomQuestState
    <*> newStdGen
    <*> pure def
    <*> getCurrentTime
    <*> pure []
    <*> pure []

passwordIsValid ∷ Text → Account → Bool
passwordIsValid password account =
  verifyPassword (encodeUtf8 password) (encodeUtf8 $ account ^. password_)

newtype Username = Username { unwrapUsername ∷ Text } deriving
  ( Eq
  , FromJSONKey
  , Ord
  , Parsable
  , Read
  , Show
  , ToJSONKey
  )

instance FromJSON Username where
  parseJSON = parseJSON >>> fmap Username

instance ToJSON Username where
  toJSON = unwrapUsername >>> toJSON
  toEncoding = unwrapUsername >>> toEncoding
