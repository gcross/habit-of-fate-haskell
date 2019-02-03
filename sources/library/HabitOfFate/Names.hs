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
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Names where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Control.Monad.Random (MonadRandom(..), MonadSplit(..))
import Data.List (head, splitAt, tail)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Vector (Vector)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import HabitOfFate.Data.Account
import HabitOfFate.Server.Transaction

newtype Characters = Characters (Vector Text) deriving (Lift)

names ∷ QuasiQuoter
names = QuasiQuoter
  process
  (error "Cannot use names as a pattern")
  (error "Cannot use names as a type")
  (error "Cannot use names as a dec")
 where
  process ∷ String → Q Exp
  process =
    Text.pack
    >>>
    lines
    >>>
    V.fromList
    >>>
    Lift.lift

data OutOfCharacters = OutOfCharacters deriving (Eq,Show)
instance Exception OutOfCharacters where
  displayException _ = "Out of names!"

allocateNameFrom ∷ Vector Text → Age → Transaction Text
allocateNameFrom characters age = do
  rescued ← use rescued_
  case lookup age rescued of
    Just cs | not (onull cs) → do
      (front, back) ← getRandomR (0, olength cs-1) <&> flip splitAt cs
      rescued_ . at age .= Just (front ⊕ (tail back))
      pure $ head back
    _ → do
      appeared ← use appeared_
      let go n
            | n > olength characters =
                throwM OutOfCharacters -- might not actually be true but close enough
            | otherwise = do
                i ← getRandomR (0, olength characters-1)
                let c = characters ^?! ix i
                if notMember c appeared
                  then (appeared_ %= insertSet c) >> pure c
                  else go (n+1)
      go 0
