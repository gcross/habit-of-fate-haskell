{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Game where

import HabitOfFate.Prelude

import Control.Monad.Random hiding (split, uniform)
import qualified Control.Monad.Random as Random

import HabitOfFate.Credits
import HabitOfFate.Story
import HabitOfFate.TH

data GameState = GameState
  { _belief ∷ Int
  , _credits ∷ Credits
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''GameState
makeLenses ''GameState

newtype Game α =
    Game { unwrapGame ∷ StateT GameState (WriterT (Seq Paragraph) (Rand StdGen)) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    ,MonadState GameState
    ,MonadWriter (Seq Paragraph)
    )

data RunGameResult α = RunGameResult
  { _returned_value ∷ α
  , _new_game ∷ GameState
  , _game_paragraphs ∷ Seq Paragraph
  } deriving (Eq,Ord,Read,Show)
makeLenses ''RunGameResult

class MonadRandom m ⇒ MonadGame m where
  addParagraph ∷ Paragraph → m ()

instance MonadGame Game where
  addParagraph = gameAddParagraph

instance MonadGame m ⇒ MonadGame (StateT s m) where
  addParagraph = lift . addParagraph

newGame ∷ GameState
newGame = GameState 0 (Credits 0 0)

runGame ∷ GameState → Game α → Rand StdGen (RunGameResult α)
runGame state =
  fmap (uncurry ∘ uncurry $ RunGameResult)
  ∘
  runWriterT
  ∘
  flip runStateT state
  ∘
  unwrapGame

gameAddParagraph ∷ Paragraph → Game ()
gameAddParagraph = tell ∘ singleton

substituteAndAddParagraphs ∷ MonadGame m ⇒ Substitutor → [SubParagraph] → m ()
substituteAndAddParagraphs subs =
  traverse_ addParagraph
  ∘
  either error identity
  ∘
  traverse (substitute subs)

uniform ∷ MonadRandom m ⇒ [α] → m α
uniform = Random.uniform

uniformAction ∷ MonadRandom m ⇒ [m α] → m α
uniformAction = join . uniform

weighted ∷ MonadRandom m ⇒ [(Rational,α)] → m α
weighted = Random.fromList . fmap swap

weightedAction ∷ MonadRandom m ⇒ [(Rational,m α)] → m α
weightedAction = join . weighted
