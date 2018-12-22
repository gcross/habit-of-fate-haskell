{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Configuration where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Time.Zones.All (TZLabel(America__New_York))

import HabitOfFate.TH

instance ToJSON TZLabel where
  toJSON = show >>> toJSON

instance FromJSON TZLabel where
  parseJSON (String label) =
    case readEither (unpack label) of
      Left error_message → fail error_message
      Right timezone → pure timezone
  parseJSON _ = fail "Expected a string."

data Configuration = Configuration
  {  _timezone_ ∷ !TZLabel
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Configuration
makeLenses ''Configuration

instance NFData Configuration where rnf (Configuration tz) = rnf tz

instance Default Configuration where
  def = Configuration America__New_York
