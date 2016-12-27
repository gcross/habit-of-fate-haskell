{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions)
import qualified Data.Aeson.TH as AesonTH

deriveJSON = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = dropWhile (== '_')
  }
