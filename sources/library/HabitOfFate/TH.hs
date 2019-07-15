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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import HabitOfFate.Prelude

import qualified Data.ByteString.Char8 as BS8
import Instances.TH.Lift ()
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote

textChar8 = QuasiQuoter
  (BS8.pack >>> Lift.lift)
  (error "Cannot use textChar8 as a pattern")
  (error "Cannot use textChar8 as a type")
  (error "Cannot use textChar8 as a dec")
