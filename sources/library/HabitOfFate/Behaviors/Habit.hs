{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Behaviors.Habit where

import Prelude hiding (id)

import Control.Lens ((^.), makeLenses)
import Data.Aeson
import Data.Text ()
import Data.UUID
import Data.UUID.Aeson

import HabitOfFate.TH

data Habit = Habit
  { _id ∷ UUID
  , _name ∷ String
  , _success_credits ∷ Double
  , _failure_credits ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

habitToJSON ∷ Habit → Value
habitToJSON habit = object
  [
    "type" .= String "habit"
  , "id" .= toText (habit ^. id)
  , "attributes" .= object
      [ "name" .= (habit ^. name)
      , "success" .= (habit ^. success_credits)
      , "failure" .= (habit ^. failure_credits)
      ]
  ]
