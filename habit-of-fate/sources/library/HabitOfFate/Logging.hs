{-# LANGUAGE UnicodeSyntax #-}
module HabitOfFate.Logging where

import HabitOfFate.Prelude

import Data.Time.Clock
import Data.Time.Format
import System.IO

logIO ∷ MonadIO m ⇒ String → m ()
logIO message = liftIO $ do
  current_time ← getCurrentTime
  hPutStrLn stderr $ formatTime defaultTimeLocale "[%F %X] " current_time ⊕ message
