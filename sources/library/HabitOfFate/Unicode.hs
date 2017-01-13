{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Unicode where

import Control.Lens ((<>~), (<>=), ASetter, ASetter')
import Control.Monad.State (MonadState)
import Data.Monoid (mappend)

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

(⊘) ∷ Monoid m ⇒ m
(⊘) = mempty
{-# INLINE (⊘) #-}

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
