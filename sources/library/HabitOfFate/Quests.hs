{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests where

import Control.Lens (Prism', (^.), (^?), makePrisms, re)

import HabitOfFate.Game
import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Quest
import HabitOfFate.TH
import HabitOfFate.Unicode

data CurrentQuestState =
    Forest Forest.State
  deriving (Eq,Ord,Read,Show)
deriveJSON ''CurrentQuestState
makePrisms ''CurrentQuestState


data Quest = ∀ α. Quest
  (Prism' CurrentQuestState α)
  (Game α)
  (QuestAction α ())

quests ∷ [Quest]
quests =
  [Quest _Forest Forest.new Forest.run
  ]

runCurrentQuest ∷ Maybe CurrentQuestState → Game (Maybe CurrentQuestState)
runCurrentQuest Nothing =
  uniform quests
  >>=
  (\(Quest prism new _) → (Just . (^.re prism)) <$> new)
runCurrentQuest (Just state) = go quests
  where
    go ∷ [Quest] → Game (Maybe CurrentQuestState)
    go [] = error "Unrecognized quest type"
    go (Quest prism _ action:rest) =
      case state ^? prism of
        Nothing → go rest
        Just x → fmap (^.re prism) <$> runQuest x action
