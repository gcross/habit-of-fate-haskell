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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Testing.Instances where

import HabitOfFate.Prelude hiding (elements)

import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), dayFractionToTimeOfDay)
import Data.UUID (UUID)
import System.Random (StdGen)
import Test.QuickCheck (Arbitrary(..), Gen, Positive(..), choose, elements, oneof, suchThat)
import Test.QuickCheck.Gen (chooseAny)
import Test.SmallCheck.Series (Serial(..), (\/), cons0, cons1, cons2, cons3, decDepth)

import HabitOfFate.Data.Account
import HabitOfFate.Data.Configuration
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Data.QuestState
import HabitOfFate.Data.Repeated
import HabitOfFate.Data.Scale
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Substitution

newtype LocalToSecond = LocalToSecond LocalTime deriving (Eq, Ord, Show)
instance Arbitrary LocalToSecond where
  arbitrary =
    LocalToSecond
      <$> (LocalTime
            <$> (ModifiedJulianDay <$> choose (0, 999999))
            <*> (TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> (fromIntegral <$> choose (0 ∷ Int, 59)))
          )

instance Arbitrary DaysToKeep where
  arbitrary = do
    n ← arbitrary `suchThat` (> 0)
    elements [KeepNumberOfDays, KeepDaysInPast] <&> ($ n)

instance Arbitrary DaysToRepeat where
  arbitrary =
    (DaysToRepeat
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    )
    `suchThat`
    (/= def)

instance Arbitrary Repeated where
  arbitrary = oneof [Daily <$> arbitrary, Weekly <$> arbitrary <*> arbitrary]

instance Arbitrary Frequency where
  arbitrary = oneof
    [ pure Indefinite
    , Once <$> arbitrary
    , Repeated <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Scale where arbitrary = elements [None, VeryLow .. VeryHigh]

instance Arbitrary α ⇒ Arbitrary (Tagged α) where
  arbitrary = Tagged <$> (Success <$> arbitrary) <*> (Failure <$> arbitrary)

instance Arbitrary UUID where arbitrary = chooseAny

instance Arbitrary α ⇒ Arbitrary (ItemsSequence α) where
  arbitrary = arbitrary <&> itemsFromList

instance Arbitrary Habit where
  arbitrary =
    Habit
      <$> (arbitrary <&> pack)
      <*> arbitrary
      <*> arbitrary
      <*> (arbitrary <&> setFromList)
      <*> arbitrary

instance Monad m ⇒ Serial m Gender where
  series = cons0 Male \/ cons0 Female \/ cons0 Neuter

instance Monad m ⇒ Serial m Text where
  series = series <&> pack

instance Monad m ⇒ Serial m Gendered where
  series = cons2 Gendered

instance Monad m ⇒ Serial m (HashMap Text Gendered) where
  series = series <&> mapFromList

instance Monad m ⇒ Serial m Scale where
  series =
       cons0 None
    \/ cons0 VeryLow
    \/ cons0 Low
    \/ cons0 Medium
    \/ cons0 High
    \/ cons0 VeryHigh

instance Arbitrary Text where
  arbitrary = arbitrary <&> pack

instance Arbitrary Gender where
  arbitrary = elements [Male, Female, Neuter]

instance Arbitrary Gendered where
  arbitrary = Gendered <$> arbitrary <*> arbitrary

instance Arbitrary (HashMap Text Gendered) where
  arbitrary = arbitrary <&> mapFromList

instance Arbitrary content ⇒ Arbitrary (Content content) where
  arbitrary = oneof
    [ EventContent <$> arbitrary
    , NarrativeContent <$> arbitrary
    , RandomStoriesContent <$> arbitrary
    , StatusContent <$> arbitrary
    ]

instance Arbitrary content ⇒ Arbitrary (QuestState content) where
  arbitrary = QuestState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Configuration where
  arbitrary = Configuration <$> elements [minBound..]

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> (choose (0, 3600) <&> secondsToDiffTime)

instance Arbitrary StdGen where
  arbitrary = do
    x ← arbitrary ∷ Gen (Positive Int)
    y ← arbitrary ∷ Gen (Positive Int)
    pure $ read [i|#{getPositive x} #{getPositive y}|]

instance Eq StdGen where (==) x y = show x == show y

deriving instance Eq Account

instance Arbitrary SuccessOrFailureResult where
  arbitrary = elements [SuccessResult, FailureResult]

instance Arbitrary Markdown where
  arbitrary = Markdown <$> arbitrary

instance Arbitrary Deed where
  arbitrary = Deed <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Account where
  arbitrary =
    Account
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary `suchThat` (> 0)

instance Arbitrary TimeOfDay where
  arbitrary = dayFractionToTimeOfDay <$> (toRational <$> choose (0 ∷ Float, 1) `suchThat` (< 1))

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary α ⇒ Arbitrary (Outcomes α) where
  arbitrary = oneof
    [ SuccessFailure
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , SuccessAvertedFailure
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , SuccessDangerAvertedFailure
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    ]
