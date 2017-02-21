module Main where

import Data.Functor
import Data.List
import System.Environment

main = do
  header <- unwords <$> getArgs
  let dash_count = 80 - 2 - length header
      right_dash_count = dash_count `div` 2
      left_dash_count = dash_count - right_dash_count
  putStrLn $ replicate 80 '-'
  putStrLn $ replicate left_dash_count '-' ++ " " ++ header ++ " " ++ replicate right_dash_count '-'
  putStrLn $ replicate 80 '-'
