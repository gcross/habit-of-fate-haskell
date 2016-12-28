{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.UUID
import Data.UUID.Aeson ()

import HabitOfFate.JSON
import HabitOfFate.TH
import HabitOfFate.Unicode

data Habit = Habit
  { _uuid ∷ UUID
  , _name ∷ String
  , _success_credits ∷ Double
  , _failure_credits ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

generateHabitDoc ∷ Text → Habit → Value
generateHabitDoc = generateDocWithObject "habit"

generateHabitsDoc ∷ Text → Seq Habit → Value
generateHabitsDoc = generateDocWithObjects "habit"

parseHabitDoc ∷ Value → Parser Habit
parseHabitDoc = parseDocWithObject "habit"

parseHabitsDoc ∷ Value → Parser (Seq Habit)
parseHabitsDoc = fmap Seq.fromList ∘ parseDocWithObjects "habit"
