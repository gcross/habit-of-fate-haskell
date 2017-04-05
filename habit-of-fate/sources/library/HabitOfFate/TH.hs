{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.TH where

import HabitOfFate.Prelude

import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions)
import qualified Data.Aeson.TH as AesonTH
import qualified Data.ByteString.Char8 as BS8
import Instances.TH.Lift ()
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote

deriveJSON = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = dropWhile (== '_')
  }

deriveJSONDropping n = AesonTH.deriveJSON $ defaultOptions
  { fieldLabelModifier = drop n
  }

textChar8 = QuasiQuoter
  (Lift.lift ∘ BS8.pack)
  (error "Cannot use textChar8 as a pattern")
  (error "Cannot use textChar8 as a type")
  (error "Cannot use textChar8 as a dec")
