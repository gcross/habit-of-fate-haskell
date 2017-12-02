{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Trial where

import Control.Lens
import Control.Monad.Random (MonadRandom(getRandomR))
import Control.Monad.State

numberUntilEvent ∷ MonadRandom m ⇒ Double → m Double
numberUntilEvent median = getRandomR (0,1) <&> (\rnd → logBase 2 (1-rnd) * (-median))

data Outcome = NoCredits | NothingHappened | SomethingHappened
  deriving (Eq, Ord, Read, Show)

spendCredits ∷ MonadState s m ⇒ Lens' s Double → Lens' s Double → m Outcome
spendCredits credits_until_event_lens credits_lens = do
  credits ← use credits_lens
  if credits <= 0
    then return NoCredits
    else do
      credits_until_event ← use credits_until_event_lens
      if credits < credits_until_event
        then do
          credits_lens .= 0
          credits_until_event_lens .= credits_until_event - credits
          return NothingHappened
        else do
          credits_lens .= credits - credits_until_event
          credits_until_event_lens .= 0
          return SomethingHappened
