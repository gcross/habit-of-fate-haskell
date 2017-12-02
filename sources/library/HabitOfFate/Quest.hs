{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quest where

import HabitOfFate.Prelude

import Control.Monad.Cont
import Control.Monad.Random

import HabitOfFate.Game

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
  addParagraph = addParagraph >>> lift >>> lift >>> lift >>> QuestAction

instance MonadState (QuestState s) (QuestAction s) where
  get =
    QuestAction
    $
    (QuestState
      <$> (lift >>> lift) get
      <*> get
    )

  put (QuestState game quest) = QuestAction $ do
    game |> put |> lift |> lift
    put quest

runQuest ∷ s → QuestAction s () → Game (Maybe s)
runQuest state action =
  flip runContT return $ callCC $ \quit →
    action
      |> unwrapQuestAction
      |> flip runReaderT (quit Nothing)
      |> flip execStateT state
      |> fmap Just

questHasEnded = ask >>= (lift >>> lift) |> QuestAction
