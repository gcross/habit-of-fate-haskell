{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data where

import Control.Lens (makeLenses)
import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions, deriveJSON)

data Data = Data
    {   _number_of_kills :: Int
    } deriving (Read, Show)

$(deriveJSON
    defaultOptions
        { fieldLabelModifier = \label → [if c == '_' then ' ' else c | c ← tail label]
        }
    ''Data)
$(makeLenses ''Data)
