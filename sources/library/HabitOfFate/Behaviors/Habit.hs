{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Behaviors.Habit where

import Prelude hiding (id)

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)
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

data Attr α = ∀ β. Show β ⇒ Attr Text (Getter α β)

toDoc ∷ Text → Getter α UUID → [Attr α] → α → Value
toDoc typ id attrs x = object
  [
    "type" .= String typ
  , "id" .= toText (x ^. id)
  , "attributes" .= object
      [ name .= show (x ^. getter) | Attr name getter ← attrs ]
  ]

habitToDoc ∷ Habit → Value
habitToDoc = toDoc "habit" id
  [ Attr "name" name
  , Attr "success" success_credits
  , Attr "failure" failure_credits
  ]
