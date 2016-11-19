{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests where

import Control.Lens (Prism', (^.), (^?), makePrisms, re)

import HabitOfFate.MonadGame
import HabitOfFate.Quest
import qualified HabitOfFate.Quests.Forest as Forest

data State =
    Forest Forest.State
makePrisms ''State

data Quest = ∀ α. Quest
  (Prism' State α)
  (GameAction α)
  (GoodBad → α → GameAction (Maybe α))

quests :: [Quest]
quests =
  [Quest _Forest Forest.new Forest.act
  ]

run :: GoodBad → Maybe State → GameAction (Maybe State)
run goodbad Nothing =
  uniform quests
  >>=
  (\(Quest prism new _) → (Just . (^.re prism)) <$> new)
  >>=
  run goodbad
run goodbad (Just state) = go quests
  where
    go :: [Quest] → GameAction (Maybe State)
    go [] = error "Unrecognized quest type"
    go (Quest prism _ act:rest) =
      case state ^? prism of
        Nothing → go rest
        Just x → fmap (^.re prism) <$> act goodbad x

good, bad :: Maybe State → GameAction (Maybe State)
good = run Good
bad = run Bad
