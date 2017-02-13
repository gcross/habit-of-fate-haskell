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
import Data.Aeson.Types
import Data.UUID

instance ToJSON UUID where
  toJSON = String ∘ toText

instance FromJSON UUID where
  parseJSON =
    withText "expected string"
    $
    maybe (fail "invalid UUID") return ∘ fromText

instance ToJSON α ⇒ ToJSON (Map UUID α) where
  toJSON = Object ∘ mapFromList ∘ map (toText *** toJSON) ∘ mapToList

instance FromJSON α ⇒ FromJSON (Map UUID α) where
  parseJSON = withObject "uuid map" $
    fmap mapFromList
    ∘
    traverse (\(k,v) → (,) <$> maybe (fail "bad UUID") return (fromText k) <*> parseJSON v)
    ∘
    mapToList
