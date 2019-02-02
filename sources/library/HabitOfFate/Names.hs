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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Names where

import HabitOfFate.Prelude

import Control.Monad.Random (MonadRandom, fromList)
import qualified Control.Monad.Random as Random

data MarkovModel = MarkovModel Int [(Text, Rational)] (HashMap Text [(Maybe Char, Rational)])

buildModel ∷ Int → Int → [Text] → MarkovModel
buildModel p n training = MarkovModel n prefixes transitions
 where
  prefixes =
    training
      |> map (take p)
      |> sort
      |> group
      |> map (\grp@(prefix:_) → (prefix, grp |> length |> toRational))
      |> mapFromList

  transitions =
    -- Take each word and slice it up into length-n pieces along with the
    -- character that followed each piece.
    [ (take n subword, subword ^? ix n)
    | word ← training
    , i ← [0, 1 .. olength word-n-1]
    , let subword = drop i word ∷ Text
    ]
    -- Group the slices, and then for each group...
    |> sort
    |> groupBy ((==) `on` fst)
    |> map (\grp1@((window,_):_) →
          -- ...we return a pair with the slice in the first element and...
          ( window
          -- ...with the possible following characters and counts in the second element.
          , grp1
              |> map snd
              |> group
              |> map (\grp2@(mc:_) →
                  ( mc
                  , grp2 |> length |> toRational
                  )
                 )
          )
       )
    |> mapFromList

runModel ∷ MonadRandom m ⇒ MarkovModel → m Text
runModel (MarkovModel n prefixes transitions) = do
  prefix ← Random.fromList prefixes
  go (prefix |> reverse |> unpack) prefix
 where
  go ∷ MonadRandom m ⇒ String → Text → m Text
  go accum prefix = do
    transition ← maybe (pure Nothing) Random.fromList (lookup prefix transitions)
    case transition of
      Nothing → accum |> pack |> reverse |> pure
      Just c → go (c:accum) (prefix |> drop 1 |> (`snoc` c))
