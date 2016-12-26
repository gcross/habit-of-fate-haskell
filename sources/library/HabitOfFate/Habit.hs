{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text, unpack)
import Data.UUID
import Data.UUID.Aeson ()
import Text.Printf

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

habitToDoc ∷ Habit → Value
habitToDoc = toDoc "habit"

toDoc ∷ ToJSON α ⇒ Text → α → Value
toDoc typ x = object
  [
    "type" .= String typ
  , "id" .= fromJust (HashMap.lookup "uuid" fields)
  , "attributes" .= Object (HashMap.delete "uuid" fields)
  ]
  where
    Object fields = toJSON x

habitFromDoc ∷ Maybe UUID → Value → Parser Habit
habitFromDoc = fromDoc "habit"

fromDoc ∷ FromJSON α ⇒ Text → Maybe UUID → Value → Parser α
fromDoc typ maybe_uuid value =
  flip (withObject $ "Invalid doc of type " ⊕ unpack typ) value $ \fields → do
    doc_type ← fields .: "type"
    unless (doc_type == typ) ∘ fail $
      printf
        "Expected type %s does not match given type %s"
        typ
        doc_type
    attributes ← fields .: "attributes" >>= withObject "Attributes was not an object" return
    maybe (fields .: "id") (return ∘ toText) maybe_uuid
      >>=
      parseJSON ∘ Object ∘ flip (HashMap.insert "uuid") attributes ∘ String
