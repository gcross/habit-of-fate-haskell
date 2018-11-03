{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2017 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.Forest.Parser where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(..))
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift

import HabitOfFate.Substitution

data FailureStory = FailureStory
  { _common_story_ ∷ Story
  , _averted_story_ ∷ Story
  , _happened_story_ ∷ Story
  } deriving (Eq, Lift, Ord, Show, Read)
makeLenses ''FailureStory

data ParseFailureException = ParseFailureException String deriving (Eq, Show)
instance Exception ParseFailureException

parseFailureStories ∷ MonadThrow m ⇒ [[Story]] → m [FailureStory]
parseFailureStories = mapM parseFailureStory
 where
  parseFailureStory ∷ MonadThrow m ⇒ [Story] → m FailureStory
  parseFailureStory [common, averted, happened] = pure $ FailureStory common averted happened
  parseFailureStory other = throwM $ ParseFailureException $ "Saw " ⊕ show (length other) ⊕ " failure story parts instead of 3."

loadFailureStories ∷ Q Exp
loadFailureStories =
  loadSubStoriesWithoutLifting "forest" "failure"
  >>=
  parseFailureStories
  >>=
  Lift.lift
