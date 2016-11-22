{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Maybe
import Data.Yaml
import System.Environment

import HabitOfFate
import HabitOfFate.Console
import HabitOfFate.Game

main = do
  [action, filepath] ← getArgs
  let parsed_action = case action of
        "good" → Good
        "bad" → Bad
        _ → error "Unrecognized action."
  actWithFile parsed_action filepath >>= printParagraphs
  putStrLn ""