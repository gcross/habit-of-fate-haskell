{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Unicode where

import Data.Monoid (mappend)

infixr 6 ⊕
(⊕) :: Monoid m ⇒ m → m → m
(⊕) = mappend
{-# INLINE (⊕) #-}

infixr 9 ∘
(∘) ∷ (β → δ) → (α → β) → (α → δ)
(∘) = (.)
{-# INLINE (∘) #-}

infixr 5 ⊞
(⊞) ∷ [α] → [α] → [α]
(⊞) = (++)
{-# INLINE (⊞) #-}

infixr 6 ∈
(∈) ∷ Eq α ⇒ α → [α] → Bool
(∈) = elem
{-# INLINE (∈)  #-}

infix  4 ≤
(≤) ∷ Ord α ⇒ α → α → Bool
(≤) = (<=)

infix  4 ≥
(≥) ∷ Ord α ⇒ α → α → Bool
(≥) = (>=)
