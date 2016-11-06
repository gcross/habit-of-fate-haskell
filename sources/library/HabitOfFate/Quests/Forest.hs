{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.Forest where

import Data.String.QQ

intro :: String
intro = [s|
The last thing in the world that {Susie} wanted to do was to wander around
alone in the forest this night, but {Tommy} was sick and would not live through
the night unless {she|Susie} could find {an Illsbane} plant to brew medicine for
{him|Tommy}.

She began hear search.
|]
