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

=============================== IMPORTANT NOTICE ===============================

    This file has been heavily derived from the NameGenerator package authored
    by Leo Tenenbaum <pommicket@gmail.com>, which was licensed under version 3
    of the GNU General Public License; all existing code continues to be derived
    from that license (see LICENSE-NameGenerator for the details) while all
    changed code derives from the v3 Affero GPL, which is allowed by the v3 GPL.

    I have made the following changes (in addition to others I may have missed,
    see the repository for more details):

      * Stylistic changes to make it consistent with the rest of the project.

      * Changed it to use MonadRandom instead of IO.

      * Changed it to use a built in table for the trigraphs instead of loading
        them from an external file.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.NameGenerator (generateName) where

import HabitOfFate.Prelude

import Control.Monad.Random.Class (MonadRandom(getRandomR))
import Data.Char (toUpper)
import Data.List (isPrefixOf)

import HabitOfFate.NameGenerator.Trigrams

type Trigrams = Map String Int

filterStartsWith ∷ String → Trigrams → Trigrams
filterStartsWith start =
  mapToList
  >>>
  filter (\(k, _) → start `isPrefixOf` k)
  >>>
  mapFromList

pick ∷ Int → (String, Int) → (String, Int) → (String, Int)
pick n ("", sumSoFar) (k, v)
  | sum' >= n = (k, 0)
  | otherwise = ("", sum')
 where
  sum' = sumSoFar + v
pick _ x _ = x

pickFromTrigrams ∷ MonadRandom m ⇒ Trigrams → m String
pickFromTrigrams trigrams = do
  let total = sum $ otoList trigrams
  chosen ← getRandomR (0, total)
  trigrams
    |> mapToList
    |> foldl (pick chosen) ("", 0)
    |> fst
    |> pure

first2Chars ∷ MonadRandom m ⇒ Trigrams → m String
first2Chars trigrams = pickFromTrigrams (filterStartsWith " " trigrams) <&> drop 1

takeEnd ∷ Int → [α] → [α]
takeEnd n l = drop (length l - n) l

nextChar ∷ MonadRandom m ⇒ Trigrams → String → m Char
nextChar trigrams name =
  trigrams
    |> filterStartsWith (takeEnd 2 name)
    |> pickFromTrigrams
    |> fmap (^?! _last)

generateName' ∷ MonadRandom m ⇒ Trigrams → m String → m String
generateName' trigrams currentName = do
  name ← currentName
  next ← nextChar trigrams name
  if next == ' '
    then currentName
    else generateName' trigrams $ fmap (⊕ [next]) currentName

generateName ∷ MonadRandom m ⇒ m String
generateName = fmap (text %~ toUpper) $ generateName' trigrams $ first2Chars trigrams
