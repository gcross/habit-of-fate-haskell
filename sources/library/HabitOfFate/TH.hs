{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions)
import qualified Data.Aeson.TH as AesonTH

deriveJSON = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier =
      map (\c → if c == '_' then ' ' else c)
      .
      (\label → if head label == '_' then tail label else label)
  }
