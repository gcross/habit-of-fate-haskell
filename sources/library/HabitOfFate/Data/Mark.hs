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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Mark where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), withObject)

import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.JSON

data Mark = Mark
  { mark_result ∷ !SuccessOrFailureResult
  , mark_scale ∷ !Scale
  } deriving (Read,Show,Eq,Ord)

instance ToJSON Mark where
  toJSON Mark{..} = runJSONBuilder $ do
    writeField "result" mark_result
    writeField "scale" mark_scale

instance FromJSON Mark where
  parseJSON = withObject "mark must have object shape" $ \o →
    Mark <$> (o .: "result") <*> (o .: "scale")

instance NFData Mark where
  rnf !_ = ()
