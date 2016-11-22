{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Game
  ( Action(..)
  , Game
  , GameResult(..)
  , GameState(..)
    , belief
  , newGame
  , runGame
  , text
  , uniform
  , uniformAction
  , weighted
  , weightedAction
  ) where

import Control.Lens (makeLenses)
import Control.Monad (join)
import Control.Monad.Random (MonadRandom(..), Rand(), evalRand, fromList)
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter, tell)
import qualified Control.Monad.Random as Random
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(), runStateT)
import Control.Monad.Trans.Writer.Strict (WriterT(), runWriterT)
import System.Random

import HabitOfFate.TH

data Action =
    Good
  | Bad

data GameState = GameState
  { _belief ∷ Int
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

data GameResult α = GameResult
  { new_game_state ∷ GameState
  , paragraphs ∷ [[String]]
  , returned_value ∷ α
  } deriving (Eq,Ord,Read,Show)

newGame ∷ GameState
newGame = GameState 0

runGame ∷ GameState → Game α → IO (GameResult α)
runGame state game = do
  g ← newStdGen
  let ((returned_value, new_game_result), paragraphs) =
        flip evalRand g
        .
        runWriterT
        .
        flip runStateT state
        .
        unwrapGame
        $
        game
  return $ GameResult new_game_result paragraphs returned_value

text ∷ String → Game ()
text = tell . map (concatMap words) . splitParagraphs . lines
  where
    splitParagraphs ∷ [String] → [[String]]
    splitParagraphs [] = []
    splitParagraphs ("":rest) = splitParagraphs rest
    splitParagraphs rest = paragraph:splitParagraphs remainder
      where (paragraph,remainder) = break (== "") rest

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = join . uniform

weighted ∷ MonadRandom m ⇒ [(Rational,α)] → m α
weighted = fromList . map (\(x,y) → (y,x))

weightedAction ∷ MonadRandom m ⇒ [(Rational,m α)] → m α
weightedAction = join . weighted
