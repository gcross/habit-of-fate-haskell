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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Credits where

import HabitOfFate.Prelude

import Data.Aeson

import HabitOfFate.TH

newtype Successes = Successes { unwrapSuccesses ∷ Double }
  deriving (Eq,FromJSON,Ord,Read,Show,ToJSON)

instance Wrapped Successes where
  type Unwrapped Successes = Double
  _Wrapped' = iso unwrapSuccesses Successes

newtype Failures = Failures { unwrapFailures ∷ Double }
  deriving (Eq,FromJSON,Ord,Read,Show,ToJSON)

instance Wrapped Failures where
  type Unwrapped Failures = Double
  _Wrapped' = iso unwrapFailures Failures

data Credits = Credits
  { _successes_ ∷ Successes
  , _failures_ ∷ Failures
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Credits

instance Default Credits where
  def = Credits (Successes 0) (Failures 0)

successes_ ∷ Lens' Credits Double
successes_ =
  lens
    (_successes_ >>> unwrapSuccesses)
    (\old new → old { _successes_ = Successes new })

failures_ ∷ Lens' Credits Double
failures_ =
  lens
    (_failures_ >>> unwrapFailures)
    (\old new → old { _failures_ = Failures new })
