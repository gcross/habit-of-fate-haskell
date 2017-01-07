{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Game where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Random hiding (split, uniform)
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Monad.Random as Random
import Data.Text (Text)
import Data.Text.Lazy.Builder

import HabitOfFate.TH
import HabitOfFate.Unicode

data GameState = GameState
  { _belief ∷ Int
  , _success_credits ∷ Double
  , _failure_credits ∷ Double
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''GameState
makeLenses ''GameState

newtype Game α =
    Game { unwrapGame ∷ StateT GameState (WriterT Builder (Rand StdGen)) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    ,MonadState GameState
    ,MonadWriter Builder
    )

data RunGameResult α = RunGameResult
  { _returned_value ∷ α
  , _new_game ∷ GameState
  , _game_text ∷ Text
  } deriving (Eq,Ord,Read,Show)
makeLenses ''RunGameResult

class MonadRandom m ⇒ MonadGame m where
  text ∷ Text → m ()

instance MonadGame Game where
  text = gameText

instance MonadGame m ⇒ MonadGame (StateT s m) where
  text = lift . text

newGame ∷ GameState
newGame = GameState 0 0 0

runGame ∷ GameState → Game α → Rand StdGen (RunGameResult α)
runGame state =
  fmap (uncurry ∘ uncurry $ RunGameResult)
  ∘
  fmap (_2 %~ (^. strict) ∘ toLazyText)
  ∘
  runWriterT
  ∘
  flip runStateT state
  ∘
  unwrapGame

gameText ∷ Text → Game ()
gameText = tell ∘ fromText

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = join . uniform

weighted ∷ MonadRandom m ⇒ [(Rational,α)] → m α
weighted = fromList . map (\(x,y) → (y,x))

weightedAction ∷ MonadRandom m ⇒ [(Rational,m α)] → m α
weightedAction = join . weighted
