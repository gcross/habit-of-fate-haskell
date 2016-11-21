{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Maybe
import Data.Yaml
import System.Environment

import HabitOfFate
import HabitOfFate.Console
import HabitOfFate.Quest

main = do
  [filename] ← getArgs
  decodeFile filename
    >>=
    runGame . act Good . fromJust
    >>=
    encodeFile filename
