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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Names where

import HabitOfFate.Prelude

import Control.Monad.Random (MonadRandom(..), MonadSplit(..))
import Data.Char (toLower, toUpper)
import Data.List (head)
import Data.MarkovChain (runMulti)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.Random (RandomGen)

generateNameFrom ∷ (RandomGen g, MonadRandom m, MonadSplit g m) ⇒ Int → [String] → m Text
generateNameFrom n input = do
  g ← getSplit
  start ← getRandomR (0, length input-1)
  let result ∷ String
      result = head $ runMulti 4 (input |> map (map toLower)) start g
  pure (result & _head %~ toUpper & pack)

names ∷ QuasiQuoter
names = QuasiQuoter
  process
  (error "Cannot use names as a pattern")
  (error "Cannot use names as a type")
  (error "Cannot use names as a dec")
 where
  process ∷ String → Q Exp
  process input = [|generateNameFrom 4 (lines input)|]
