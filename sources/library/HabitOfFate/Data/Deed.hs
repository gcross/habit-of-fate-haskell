{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Deed where

import HabitOfFate.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), object, withObject)
import Data.Time.LocalTime
import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.JSON

data Deed = Deed SuccessOrFailureResult Lazy.Text LocalTime deriving (Eq,Ord,Read,Show)

instance ToJSON Deed where
  toJSON (Deed result text when) = object
    [ "result" .== result
    , "story" .== text
    , "when" .== when
    ]

instance FromJSON Deed where
  parseJSON = withObject "deed must be object-shaped" $ \o →
    Deed
      <$> (o .: "result")
      <*> (o .: "story")
      <*> (o .: "when")
