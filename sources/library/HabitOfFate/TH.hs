{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import HabitOfFate.Prelude

import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions)
import qualified Data.Aeson.TH as AesonTH

deriveJSON = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = dropWhile (== '_')
  }

deriveJSONDropping n = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = drop n
  }
