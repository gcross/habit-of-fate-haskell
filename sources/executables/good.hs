{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Maybe
import Data.Yaml
import System.Environment

import HabitOfFate
import HabitOfFate.Console
import HabitOfFate.Game

main = do
  [filepath] ← getArgs
  actWithFile Good filepath >>= printParagraphs
  putStrLn ""