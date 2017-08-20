{-# LANGUAGE UnicodeSyntax #-}
module HabitOfFate.Logging where

import HabitOfFate.Prelude

import Data.Text.IO (hPutStrLn)
import Data.Time.Clock
import Data.Time.Format
import System.IO (stderr)

logIO ∷ MonadIO m ⇒ String → m ()
logIO message = liftIO $ do
  current_time ← getCurrentTime
  pack >>> hPutStrLn stderr $
    formatTime defaultTimeLocale "[%F %X] " current_time ⊕ message
