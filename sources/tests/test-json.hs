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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Data.Aeson (eitherDecode, encode)
import Data.CallStack (HasCallStack)
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck (ioProperty)

import HabitOfFate.Data.Account
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Data.QuestState
import HabitOfFate.Data.Scale
import HabitOfFate.Data.Tagged
import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions
import HabitOfFate.Testing.Instances ()

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ QC.testProperty "Frequency" $ \(x ∷ Frequency) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , SC.testProperty "Scale" $ \(x ∷ Scale) → (encode >>> eitherDecode) x == Right x
  , QC.testProperty "Habit" $ \(x ∷ Habit) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , QC.testProperty "ItemsSequence" $ \(x ∷ ItemsSequence Int) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , QC.testProperty "Tagged" $ \(x ∷ Tagged Int) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , SC.testProperty "Gender" $ \(x ∷ Gender) → (encode >>> eitherDecode) x == Right x
  , SC.testProperty "Gendered" $ \(x ∷ Gendered) → (encode >>> eitherDecode) x == Right x
  , QC.testProperty "Outcomes" $ \(x ∷ Outcomes Markdown) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , QC.testProperty "Content" $ \(x ∷ Content Markdown) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , QC.testProperty "QuestState" $ \(x ∷ QuestState Markdown) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  , QC.testProperty "Configuration" $ \(x ∷ Configuration) → (encode >>> eitherDecode) x == Right x
  , QC.testProperty "Account" $ \(x ∷ Account) → ioProperty $ (encode >>> eitherDecode) x @?= Right x
  ]
