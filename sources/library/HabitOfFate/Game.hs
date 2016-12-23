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

import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.Random hiding (split, uniform)
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Monad.Random as Random
import Data.List.Split
import System.Random hiding (split)

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
    Game { unwrapGame ∷ StateT GameState (WriterT [[String]] (Rand StdGen)) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    ,MonadState GameState
    ,MonadWriter [[String]]
    )

data RunGameResult α = RunGameResult
  { _returned_value ∷ α
  , _new_game ∷ GameState
  , _game_paragraphs ∷ [[String]]
  } deriving (Eq,Ord,Read,Show)
makeLenses ''RunGameResult

class MonadRandom m ⇒ MonadGame m where
  text ∷ String → m ()

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
  runWriterT
  ∘
  flip runStateT state
  ∘
  unwrapGame

gameText ∷ String → Game ()
gameText =
  tell
  ∘
  map (concatMap words)
  ∘
  filter (not ∘ null)
  ∘
  splitWhen null
  ∘
  lines

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = join . uniform

weighted ∷ MonadRandom m ⇒ [(Rational,α)] → m α
weighted = fromList . map (\(x,y) → (y,x))

weightedAction ∷ MonadRandom m ⇒ [(Rational,m α)] → m α
weightedAction = join . weighted
