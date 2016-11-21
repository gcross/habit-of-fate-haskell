{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Yaml
import System.Environment

import HabitOfFate

main = do
  [filename] ← getArgs
  encodeFile filename new
