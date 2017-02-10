{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Credits where

import Control.Lens

import HabitOfFate.TH

data Credits = Credits
  { _success ∷ Double
  , _failure ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Credits
makeLenses ''Credits

class HasCredits α where
  credits ∷ Lens' α Credits
