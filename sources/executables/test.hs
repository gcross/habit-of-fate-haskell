module Main where

import HabitOfFate.Data (Data(Data))
import HabitOfFate.Quests.Forest
import HabitOfFate.Utils.Text

import Data.ByteString.Char8 (unpack)
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Yaml (encode)

import Data.Set

-- main = do
--     putStrLn . unpack . encode $ Data (fromList [1,2,3])

main = printText (fromJust . flip Map.lookup table) intro
  where
    table = makeSubstitutionTable
        [("Susie","Sally",Female)
        ,("Tommy","Mary",Female)
        ,("Illsbane","Wolfswillow root",Neuter)
        ]

