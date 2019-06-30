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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.NameGenerator.Trigrams.TH where

import HabitOfFate.Prelude

import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Instances.TH.Lift ()

trigram_table ∷ QuasiQuoter
trigram_table = QuasiQuoter
  (
    lines
    >>>
    zip [1 ∷ Int ..]
    >>>
    map (\(row, line) →
      line
        |> words
        |> (\case
            [tri, weight_as_string] → case readMaybe weight_as_string of
              Just weight → (tri, weight)
              Nothing → error [i|Bad trigraph weight "#{weight_as_string}" in line #{row}: #{line}|]
            x → error [i|Number of words (#{length x}) /= 2 in line #{row}: #{line}|]
           )
    )
    >>>
    (mapFromList ∷ [(String, Int)] → Map String Int)
    >>>
    Lift.lift
  )
  (error "Cannot use trigraph table as a pattern")
  (error "Cannot use trigraph table as a type")
  (error "Cannot use trigraph table as a dec")
