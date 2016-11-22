{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests where

import Control.Lens (Prism', (^.), (^?), makePrisms, re)

import HabitOfFate.Game
import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.TH

data State =
    Forest Forest.State
  deriving (Eq,Ord,Read,Show)
deriveJSON ''State
makePrisms ''State

data Quest = ∀ α. Quest
  (Prism' State α)
  (Game α)
  (Action → α → Game (Maybe α))

quests ∷ [Quest]
quests =
  [Quest _Forest Forest.new Forest.act
  ]

act ∷ Action → Maybe State → Game (Maybe State)
act input Nothing =
  uniform quests
  >>=
  (\(Quest prism new _) → (Just . (^.re prism)) <$> new)
  >>=
  act input
act input (Just state) = go quests
  where
    go ∷ [Quest] → Game (Maybe State)
    go [] = error "Unrecognized quest type"
    go (Quest prism _ act:rest) =
      case state ^? prism of
        Nothing → go rest
        Just x → fmap (^.re prism) <$> act input x
