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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Tagged where

import HabitOfFate.Prelude

import Data.Aeson (FromJSON(), ToJSON())

import HabitOfFate.TH

newtype Success α = Success { unwrapSuccess ∷ α }
  deriving (Eq,FromJSON,Functor,Ord,Read,Show,ToJSON)

instance Wrapped (Success α) where
  type Unwrapped (Success α) = α
  _Wrapped' = iso unwrapSuccess Success

newtype Failure α = Failure { unwrapFailure ∷ α }
  deriving (Eq,FromJSON,Functor,Ord,Read,Show,ToJSON)

instance Wrapped (Failure α) where
  type Unwrapped (Failure α) = α
  _Wrapped' = iso unwrapFailure Failure

data Tagged α = Tagged
  { _success_ ∷ Success α
  , _failure_ ∷ Failure α
  } deriving (Eq,Functor,Ord,Read,Show)
deriveJSON ''Tagged

instance Default α ⇒ Default (Tagged α) where
  def = Tagged (Success def) (Failure def)

success_ ∷ Lens' (Tagged α) α
success_ =
  lens
    (_success_ >>> unwrapSuccess)
    (\old new → old { _success_ = Success new })

failure_ ∷ Lens' (Tagged α) α
failure_ =
  lens
    (_failure_ >>> unwrapFailure)
    (\old new → old { _failure_ = Failure new })
