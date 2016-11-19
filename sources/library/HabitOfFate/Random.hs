{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.MonadRandom where

import Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Random

weighted :: MonadRandom m ⇒ [(Rational, α)] → m α
weighted = Random.fromList ∘ map (\(x,y) → (y,x))

uniform :: MonadRandom m ⇒ [α] → m α
uniform = Random.uniform
