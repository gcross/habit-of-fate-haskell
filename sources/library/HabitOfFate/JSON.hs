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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.JSON
  ( JSONBuilder
  , (.==)
  , runJSONBuilder
  , writeField
  , writeMaybeField
  , writeTextField
) where

import HabitOfFate.Prelude hiding ((.=))

import Data.Aeson

infixr 8 .==
(.==) ∷ (KeyValue kv, ToJSON v) ⇒ Text → v → kv
(.==) = (.=)
{-# INLINE (.==) #-}

type JSONBuilder = State Object

writeField ∷ ToJSON α ⇒ Text → α → JSONBuilder ()
writeField key value = modify $ insertMap key (toJSON value)

writeTextField ∷ Text → Text → JSONBuilder ()
writeTextField = writeField

writeMaybeField ∷ ToJSON α ⇒ Text → Maybe α → JSONBuilder ()
writeMaybeField key = maybe (pure ()) (writeField key)

runJSONBuilder ∷ JSONBuilder () → Value
runJSONBuilder builder = Object $ execState builder mempty
