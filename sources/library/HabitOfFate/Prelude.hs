{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Prelude
  (
  -- Modules
    module Control.Applicative
  , module Control.Arrow
  , module Control.Lens
  , module Control.Monad.Except
  , module Control.Monad.Fail
  , module Control.Monad.Reader
  , module Control.Monad.State.Strict
  , module Control.Monad.Writer.Strict
  , module Data.Bool
  , module Data.Containers
  , module Data.Default
  , module Data.Either
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Maybe
  , module Data.Monoid
  , module Data.MonoTraversable
  , module Data.MonoTraversable.Unprefixed
  , module Data.Sequences
  , module Data.Text.Lens
  , module Labels
  , module Text.Printf
  -- Operators
  , (^)
  , (^^)
  , (∘)
  , (⊕)
  , (⊕~)
  , (⊕=)
  , (∈)
  , (∉)
  , (≤)
  , (≥)
  , (⊥)
  -- Typeclasses
  , Eq(..)
  , Ord(..)
  , Read(..)
  , Show(..)
  -- Types
  , 𝔹
  , Bool(..)
  , ByteString
  , Char
  , Double
  , Enum(..)
  , FilePath
  , Float
  , Floating(..)
  , Fractional(..)
  , HashMap
  , Int
  , Integer
  , Integral(..)
  , IO
  , l_
  , Map
  , Maybe(..)
  , Num(..)
  , Rational
  , Real(..)
  , RealFloat(..)
  , RealFrac(..)
  , Seq
  , Set
  , String
  , Text
  , Word
  -- Functions
  , curry
  , error
  , even
  , fromIntegral
  , fst
  , gcd
  , identity
  , lcm
  , map
  , odd
  , read
  , realToFrac
  , showText
  , snd
  , subtract
  , swap
  , uncurry
  , zip
  ) where

import Control.Applicative hiding (many)

import Control.Arrow hiding (loop)

import Control.Lens

import Control.Monad

import Control.Monad.Except
  hiding
    ( fail
    , filterM
    , replicateM
    )

import Control.Monad.Fail

import Control.Monad.Reader
  hiding
    ( fail
    , filterM
    , replicateM
    )

import Control.Monad.State.Strict
  hiding
    ( fail
    , filterM
    , replicateM
    )

import Control.Monad.Writer.Strict
  hiding
    ( fail
    , filterM
    , replicateM
    )

import Data.Bool

import Data.ByteString (ByteString)

import Data.Containers

import Data.Default

import Data.Either

import Data.Foldable hiding (length)

import Data.Function

import Data.Functor

import Data.HashMap.Strict (HashMap)

import Data.Map (Map)

import Data.Maybe hiding (catMaybes)

import Data.Monoid

import Data.MonoTraversable

import Data.MonoTraversable.Unprefixed (intercalate, length)

import Data.Sequence (Seq)

import Data.Sequences
  hiding
    ( Index
    , cons
    , find
    , index
    , snoc
    , uncons
    , unsnoc
    )

import Data.Set (Set)

import Data.Text (Text)

import Data.Text.Lens

import Labels ((:=)(..))
import qualified Labels as Labels

import Text.Parsec

import Text.Printf

instance MonadFail (ParsecT s u m) where
  fail = parserFail

instance MonadFail (Either String) where
  fail = Left

infixr 6 ⊕
(⊕) ∷ Monoid m ⇒ m → m → m
(⊕) = mappend
{-# INLINE (⊕) #-}

infixr 4 ⊕~
(⊕~) ∷ Monoid α ⇒ ASetter s t α α → α → s → t
(⊕~) = (<>~)
{-# INLINE (⊕~) #-}

infixr 4 ⊕=
(⊕=) ∷ (MonadState s m, Monoid α) => ASetter' s α -> α -> m ()
(⊕=) = (<>=)
{-# INLINE (⊕=) #-}

infixr 9 ∘
(∘) ∷ (β → δ) → (α → β) → (α → δ)
(∘) = (.)
{-# INLINE (∘) #-}

infixr 6 ∈
(∈) ∷ Eq α ⇒ α → [α] → Bool
(∈) = elem
{-# INLINE (∈)  #-}

infixr 6 ∉
(∉) ∷ Eq α ⇒ α → [α] → Bool
(∉) x xs = not (x ∈ xs)
{-# INLINE (∉)  #-}

infix  4 ≤
(≤) ∷ Ord α ⇒ α → α → Bool
(≤) = (<=)

infix  4 ≥
(≥) ∷ Ord α ⇒ α → α → Bool
(≥) = (>=)

(⊥) ∷ α
(⊥) = undefined

type 𝔹 = Bool

l_ = Labels.lens

identity ∷ α → α
identity = id

showText ∷ Show α ⇒ α → Text
showText = view packed ∘ show

swap ∷ (α,β) → (β,α)
swap (x,y) = (y,x)
