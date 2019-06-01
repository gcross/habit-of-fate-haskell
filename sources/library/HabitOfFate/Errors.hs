{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Errors
  ( Errors
  , TextErrors
  , extractResult
  , report
  , reportMessage
  ) where

import HabitOfFate.Prelude

newtype Errors e α = Errors { extractResult ∷ Either [e] α } deriving (Eq,Functor,Ord,Read,Show)

instance Applicative (Errors e) where
  pure = Right >>> Errors

  Errors (Right f) <*> Errors (Right x) = Errors (Right (f x))
  Errors (Left fe) <*> Errors (Right _) = Errors (Left fe)
  Errors (Right _) <*> Errors (Left xe) = Errors (Left xe)
  Errors (Left fe) <*> Errors (Left xe) = Errors (Left (fe ⊕ xe))

  liftA2 f (Errors (Right x)) (Errors (Right y)) = Errors (Right (f x y))
  liftA2 _ (Errors (Left xe)) (Errors (Right _)) = Errors (Left xe)
  liftA2 _ (Errors (Right _)) (Errors (Left ye)) = Errors (Left ye)
  liftA2 _ (Errors (Left xe)) (Errors (Left ye)) = Errors (Left (xe ⊕ ye))

  Errors (Right _) *> Errors (Right y) = Errors (Right y)
  Errors (Left xe) *> Errors (Right _) = Errors (Left xe)
  Errors (Right _) *> Errors (Left ye) = Errors (Left ye)
  Errors (Left xe) *> Errors (Left ye) = Errors (Left (xe ⊕ ye))

  Errors (Right x) <* Errors (Right _) = Errors (Right x)
  Errors (Left xe) <* Errors (Right _) = Errors (Left xe)
  Errors (Right _) <* Errors (Left ye) = Errors (Left ye)
  Errors (Left xe) <* Errors (Left ye) = Errors (Left (xe ⊕ ye))

instance Monad (Errors e) where
  return = pure

  Errors (Right x) >>= f = f x
  Errors (Left e) >>= _ = Errors (Left e)

  Errors (Right _) >> Errors (Right y) = Errors (Right y)
  Errors (Left xe) >> Errors (Right _) = Errors (Left xe)
  Errors (Right _) >> Errors (Left ye) = Errors (Left ye)
  Errors (Left xe) >> Errors (Left ye) = Errors (Left (xe ⊕ ye))

report ∷ e → Errors e α
report e = Errors (Left [e])

type TextErrors = Errors String
reportMessage ∷ String → TextErrors α
reportMessage = report
