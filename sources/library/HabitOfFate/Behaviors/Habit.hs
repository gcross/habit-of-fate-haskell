{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Behaviors.Habit where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import Data.UUID
import Data.UUID.Aeson ()

import HabitOfFate.TH

data Habit = Habit
  { _uuid ∷ UUID
  , _name ∷ String
  , _success_credits ∷ Double
  , _failure_credits ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit
makeLenses ''Habit

toDoc ∷ ToJSON α ⇒ Text → α → Value
toDoc typ x = object
  [
    "type" .= String typ
  , "id" .= fromJust (HashMap.lookup "uuid" fields)
  , "attributes" .= Object (HashMap.delete "uuid" fields)
  ]
  where
    Object fields = toJSON x
