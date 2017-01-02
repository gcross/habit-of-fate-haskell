{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON where

import Prelude hiding (id)

import Control.Lens
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(Array, Object)
  , parseJSON
  , toJSON
  )
import Data.Foldable (toList)
import qualified Data.Text as S
import Data.UUID (UUID)

import HabitOfFate.JSONInstances ()
import HabitOfFate.TH
import HabitOfFate.Unicode

data Links = Links
  { _self ∷ S.Text
  } deriving (Eq,Ord,Read,Show)
makeLenses ''Links
deriveJSON ''Links

data WrappedObject α = WrappedObject
  { _id ∷ Maybe UUID
  , __type ∷ S.Text
  , _attributes ∷ α
  } deriving (Eq,Ord,Read,Show)
makeLenses ''WrappedObject
deriveJSON ''WrappedObject

makeWrappedObject ∷ S.Text → α → WrappedObject α
makeWrappedObject = WrappedObject Nothing

data DocData α =
    SingleWrappedObject (WrappedObject α)
  | MultipleWrappedObjects [WrappedObject α]
  deriving (Eq,Ord,Read,Show)

instance FromJSON α ⇒ FromJSON (DocData α) where
  parseJSON x@(Object _) = SingleWrappedObject <$> parseJSON x
  parseJSON (Array xs) = MultipleWrappedObjects ∘ toList <$> mapM parseJSON xs
  parseJSON _ = fail "data must be an object or an array"

instance ToJSON α ⇒ ToJSON (DocData α) where
  toJSON (SingleWrappedObject x) = toJSON x
  toJSON (MultipleWrappedObjects xs) = toJSON xs

data Doc α = Doc
  { _links ∷ Maybe Links
  , __data ∷ DocData α
  } deriving (Eq,Ord,Read,Show)
makeLenses ''Doc
deriveJSON ''Doc

makeDocWithWrappedObject ∷ WrappedObject α → Doc α
makeDocWithWrappedObject = Doc Nothing ∘ SingleWrappedObject

makeDocWithWrappedObjects ∷ [WrappedObject α] → Doc α
makeDocWithWrappedObjects = Doc Nothing ∘ MultipleWrappedObjects
