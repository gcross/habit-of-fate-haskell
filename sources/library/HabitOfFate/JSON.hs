{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON where

import HabitOfFate.Prelude

import Data.Aeson
import Data.UUID

instance ToJSON UUID where
  toJSON = String ∘ toText

instance FromJSON UUID where
  parseJSON =
    withText "expected string"
    $
    maybe (fail "invalid UUID") return ∘ fromText

instance (Ord k, ToJSON k, ToJSON v) ⇒ ToJSON (Map k v) where
  toJSON = toJSON ∘ mapToList

instance (Ord k, FromJSON k, FromJSON v) ⇒ FromJSON (Map k v) where
  parseJSON = fmap mapFromList ∘ parseJSON
