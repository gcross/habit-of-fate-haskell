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
  toJSON = toText >>> String

instance FromJSON UUID where
  parseJSON =
    withText "expected string" $ fromText >>> maybe (fail "invalid UUID") return

instance ToJSON α ⇒ ToJSON (Map UUID α) where
  toJSON = mapToList >>> map (toText *** toJSON) >>> mapFromList >>> Object

instance FromJSON α ⇒ FromJSON (Map UUID α) where
  parseJSON = withObject "uuid map" $
    mapToList
    >>>
    traverse (\(k,v) → (,) <$> maybe (fail "bad UUID") return (fromText k) <*> parseJSON v)
    >>>
    fmap mapFromList
