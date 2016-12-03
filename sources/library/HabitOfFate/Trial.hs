{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Trial where

import Control.Lens
import Control.Monad.Random (MonadRandom(getRandomR))

import HabitOfFate.Game

numberUntilEvent ∷ MonadRandom m ⇒ Double → m Double
numberUntilEvent p = logBase (1-p) <$> getRandomR (0,1)

data Outcome = NoCredits | NothingHappened | SomethingHappened
  deriving (Eq, Ord, Read, Show)

data Result = Result
  { _success_outcome ∷ Outcome
  , _failure_outcome ∷ Outcome
  } deriving (Eq, Ord, Read, Show)

performTrials ∷ Double → Double → Game Result
performTrials success_rate failure_rate = do
  number_until_success ← numberUntilEvent success_rate
  number_until_failure ← numberUntilEvent failure_rate
  let number_of_trials = number_until_success `min` number_until_failure
      spend credits
        | credits == 0 = (NoCredits, 0)
        | number_of_trials > credits = (NothingHappened, 0)
        | otherwise = (SomethingHappened, credits - number_of_trials)
  Result
    <$> (success_credits %%= spend)
    <*> (failure_credits %%= spend)
