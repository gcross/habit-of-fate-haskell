module Main where

import HabitOfFate.Data (Data(Data))

import Data.ByteString.Char8 (unpack)
import Data.Yaml (encode)

main = do
    putStrLn . unpack . encode $ Data 10
