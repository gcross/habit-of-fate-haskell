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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Implementation.Common where

import HabitOfFate.Prelude

import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Concurrent.STM.TMVar (TMVar)
import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, fromText)
import Web.Scotty (Parsable(..))

import HabitOfFate.Data.Account

instance Parsable UUID where
  parseParam = view strict >>> fromText >>> maybe (Left "badly formed UUID") Right

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

newtype Cookie = Cookie Text deriving (Eq,FromJSON,Ord,Parsable,Read,Show,ToJSON)

data Environment = Environment
  { accounts_tvar ∷ TVar (Map Username (TVar Account))
  , cookies_tvar ∷ TVar (Map Cookie (UTCTime, Username))
  , expirations_tvar ∷ TVar (Set (UTCTime, Cookie))
  , write_request_var ∷ TMVar ()
  }

readTVarMonadIO ∷ MonadIO m ⇒ TVar α → m α
readTVarMonadIO = readTVarIO >>> liftIO
