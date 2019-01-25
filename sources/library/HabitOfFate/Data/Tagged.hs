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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.Tagged where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), object, withObject)

import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.JSON

newtype Success α = Success { unwrapSuccess ∷ α }
  deriving (Eq,Foldable,FromJSON,Functor,Ord,Read,Show,ToJSON,Traversable)

instance Wrapped (Success α) where
  type Unwrapped (Success α) = α
  _Wrapped' = iso unwrapSuccess Success

newtype Failure α = Failure { unwrapFailure ∷ α }
  deriving (Eq,Foldable,FromJSON,Functor,Ord,Read,Show,ToJSON,Traversable)

instance Wrapped (Failure α) where
  type Unwrapped (Failure α) = α
  _Wrapped' = iso unwrapFailure Failure

data Tagged α = Tagged
  { _success_ ∷ Success α
  , _failure_ ∷ Failure α
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

instance ToJSON α ⇒ ToJSON (Tagged α) where
  toJSON Tagged{..} = object [ "success" .== _success_, "failure" .== _failure_ ]

instance FromJSON α ⇒ FromJSON (Tagged α) where
  parseJSON = withObject "entity should be shaped like an object" $ \o →
    Tagged <$> o .: "success" <*> o .: "failure"

instance NFData α ⇒ NFData (Tagged α) where
  rnf (Tagged (Success s) (Failure f)) = rnf s `seq` rnf f `seq` ()

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

taggedLensForResult ∷ SuccessOrFailureResult → Lens' (Tagged α) α
taggedLensForResult SuccessResult = success_
taggedLensForResult FailureResult = failure_

forEachTaggedLens ∷ Applicative f ⇒ ((∀ α. Lens' (Tagged α) α) → f β) → f (Tagged β)
forEachTaggedLens runWithLens =
  Tagged
    <$> (Success <$> runWithLens success_)
    <*> (Failure <$> runWithLens failure_)

forEachTaggedLens_ ∷ Applicative f ⇒ ((∀ α. Lens' (Tagged α) α) → f ()) → f ()
forEachTaggedLens_ runWithLens = liftA2 (\_ _ → ()) (runWithLens success_) (runWithLens failure_)
