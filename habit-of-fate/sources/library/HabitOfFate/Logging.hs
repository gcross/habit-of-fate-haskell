{-# LANGUAGE UnicodeSyntax #-}
module HabitOfFate.Logging where

import HabitOfFate.Prelude

import Data.Time.Clock
import Data.Time.Format

logIO ∷ MonadIO m ⇒ String → m ()
logIO message = liftIO $ do
  current_time ← getCurrentTime
  putStrLn ∘ pack $ formatTime defaultTimeLocale "[%F %X] " current_time ⊕ message
