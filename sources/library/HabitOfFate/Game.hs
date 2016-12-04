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
import Control.Monad.Random hiding (uniform)
import Control.Monad.State
import Control.Monad.Writer
import qualified Control.Monad.Random as Random
import System.Random

import HabitOfFate.TH

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

data GameResult α = GameResult
  { _new_game ∷ GameState
  , _paragraphs ∷ [[String]]
  , _returned_value ∷ α
  } deriving (Eq,Ord,Read,Show)
makeLenses ''GameResult

class MonadRandom m ⇒ MonadGame m where
  text ∷ String → m ()

instance MonadGame Game where
  text = gameText

instance MonadGame m ⇒ MonadGame (StateT s m) where
  text = lift . text

newGame ∷ GameState
newGame = GameState 0 0 0

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

gameText ∷ String → Game ()
gameText = tell . map (concatMap words) . splitParagraphs . lines
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
