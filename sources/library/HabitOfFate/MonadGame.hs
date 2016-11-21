{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.MonadGame where

import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.Random (MonadRandom(..), fromList)
import qualified Control.Monad.Random as Random
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT())

import HabitOfFate.TH

data GameState = GameState
  { _belief ∷ Int
  } deriving (Read, Show)
deriveJSON ''GameState
makeLenses ''GameState

class MonadRandom m ⇒ MonadGame m where
  renderText ∷ String → m ()

type GameAction' = StateT GameState
type GameAction α = ∀ m. MonadGame m ⇒ GameAction' m α

instance MonadGame m ⇒ MonadGame (GameAction' m) where
  renderText = lift . renderText

weighted ∷ MonadRandom m ⇒ [(Rational,α)] → m α
weighted = fromList . map (\(x,y) → (y,x))

weightedAction ∷ MonadRandom m ⇒ [(Rational,m α)] → m α
weightedAction = join . weighted

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = join . uniform

newGame :: GameState
newGame = GameState 0
