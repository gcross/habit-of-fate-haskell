{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSONInstances where

import Control.Lens
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID
import System.Random

import HabitOfFate.Unicode

instance ToJSON StdGen where
  toJSON = toJSON ∘ show

instance FromJSON StdGen where
  parseJSON = fmap read ∘ parseJSON

instance ToJSON UUID where
  toJSON = String ∘ toText

instance FromJSON UUID where
  parseJSON =
    withText "expected string"
    $
    maybe (fail "invalid UUID") return ∘ fromText

instance ToJSON α ⇒ ToJSON (Map UUID α) where
  toJSON =
    Object
    ∘
    HashMap.fromList
    ∘
    map ((_1 %~ toText) ∘ (_2 %~ toJSON))
    ∘
    Map.toList

instance FromJSON α ⇒ FromJSON (Map UUID α) where
  parseJSON =
    withObject "expected object"
    $
    fmap Map.fromList
    ∘
    mapM (\(key,value) →
      (,) <$> maybe (fail "invalid UUID") return (fromText key)
          <*> parseJSON value
    )
    ∘
    HashMap.toList
