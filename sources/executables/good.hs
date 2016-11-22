{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Maybe
import Data.Yaml
import System.Environment

import HabitOfFate
import HabitOfFate.Console
import HabitOfFate.Game

main = do
  [filename] ← getArgs
  (new_data, paragraphs) ← decodeFile filename >>= act Good . fromJust
  encodeFile filename new_data
  printParagraphs paragraphs
  putStrLn ""
