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

module HabitOfFate.Credits where

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
  { _successes ∷ Successes
  , _failures ∷ Failures
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Credits

successes ∷ Lens' Credits Double
successes =
  lens
    (_successes >>> unwrapSuccesses)
    (\old new → old { _successes = Successes new })

failures ∷ Lens' Credits Double
failures =
  lens
    (_failures >>> unwrapFailures)
    (\old new → old { _failures = Failures new })
