{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest where

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import HabitOfFate.Game
import HabitOfFate.Unicode

data QuestState α = QuestState
  { _game ∷ GameState
  , _quest ∷ α
  }
makeLenses ''QuestState

type GameWithCont s = ContT (Maybe s) Game

newtype QuestAction s α = QuestAction
  { unwrapQuestAction ∷ ReaderT (GameWithCont s ()) (StateT s (GameWithCont s)) α }
  deriving
    (Applicative
    ,Functor
    ,Monad
    ,MonadRandom
    )

instance MonadGame (QuestAction s) where
  story = QuestAction ∘ lift ∘ lift ∘ lift ∘ story

instance MonadState (QuestState s) (QuestAction s) where
  get =
    QuestAction
    $
    (QuestState
      <$> (lift ∘ lift) get
      <*> get
    )

  put (QuestState game quest) = QuestAction $ do
    lift ∘ lift ∘ put $ game
    put quest

runQuest ∷ s → QuestAction s () → Game (Maybe s)
runQuest state action =
  flip runContT return $ callCC $ \quit →
    (Just <$>)
    ∘
    flip execStateT state
    ∘
    flip runReaderT (quit Nothing)
    ∘
    unwrapQuestAction
    $
    action

questHasEnded = QuestAction $ ask >>= lift ∘ lift
