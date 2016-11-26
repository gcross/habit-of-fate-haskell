{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Trial where

import Control.Lens
import Control.Monad.Random (MonadRandom(getRandomR))

import HabitOfFate.Game

numberUntilEvent ∷ MonadRandom m ⇒ Double → m Int
numberUntilEvent p = do
  let p_scaled = p / 100
  rnd ← getRandomR (0,1)
  let go k last_pmf cdf
        | cdf >= rnd = k
        | otherwise = go (k+1) new_last_pmf new_cdf
        where
          new_last_pmf = last_pmf * p_scaled
          new_cdf = cdf + new_last_pmf
  return $ go 1 (1-p_scaled) (1-p_scaled)

data Result = Success | Failure

performTrials ∷ Double → Double → Game (Maybe Result)
performTrials success_rate failure_rate = do
  number_until_success ← numberUntilEvent success_rate
  number_until_failure ← numberUntilEvent failure_rate
  let number_of_trials = number_until_success `min` number_until_failure
      spend credits
        | number_of_trials <= credits = (True, credits - number_of_trials)
        | otherwise = (False, 0)
  success_happened ← success_credits %%= spend
  failure_happened ← failure_credits %%= spend
  case (success_happened, failure_happened) of
    (False, False) → return $ Nothing
    (True, False) → return $ Just Success
    (False, True) → return $ Just Failure
    (True, True) → Just <$>
      case number_until_success `compare` number_until_failure of
        LT → return Success
        GT → return Failure
        EQ → uniform [Success, Failure]
