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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding (elements, text)

import Control.Exception (try)
import Data.CallStack (HasCallStack)

import HabitOfFate.Substitution
import HabitOfFate.Testing
import HabitOfFate.Testing.Assertions

main ∷ HasCallStack ⇒ IO ()
main = doMain
  [ testCase "literal, one char" $ do
      story ← parseSubstitutions "l"
      extractPlaceholders story @?= []
      substitute mempty story >>= (@?= "l")
  , testCase "literal, whole string" $
      parseSubstitutions "xyz"
      >>=
      substitute mempty
      >>=
      (@?= "xyz")
  , testCase "substitution, name" $ do
      story ← parseSubstitutions "|name"
      extractPlaceholders story @?= ["name"]
      substitute (singletonMap "name" (Gendered "value" Male)) story >>= (@?= "value")
  , testCase "substitution, subject" $
      parseSubstitutions "he/she|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "she")
  , testCase "substitution, possessive" $
      parseSubstitutions "his/her|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Male))
      >>=
      (@?= "his")
  , testCase "substitution, proper possessive" $
      parseSubstitutions "his/hers|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "hers")
  , testCase "substitution, proper possessive, male capitalized" $
      parseSubstitutions "His/hers|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "Hers")
  , testCase "substitution, proper possessive, both capitalized" $
      parseSubstitutions "His/Hers|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "Hers")
  , testCase "substitution, with a article" $
      parseSubstitutions "an |name"
      >>=
      substitute (singletonMap "name" (Gendered "cat" Female))
      >>=
      (@?= "a cat")
  , testCase "substitution, with an article" $
      parseSubstitutions "a |name"
      >>=
      substitute (singletonMap "name" (Gendered "apple" Female))
      >>=
      (@?= "an apple")
  , testCase "substitution, with capitalized article" $
      parseSubstitutions "An |name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "A value")
  , testCase "substitution, with article and a newline" $
      parseSubstitutions "an\n|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Female))
      >>=
      (@?= "a value")
  , testCase "substitution, uppercase referrant" $
      parseSubstitutions "His/hers|name"
      >>=
      substitute (singletonMap "name" (Gendered "value" Male))
      >>= (@?= "His")
  , testCase "unrecognized key" $
      parseSubstitutions "His/hers|Bob"
      >>=
      (substitute mempty >>> try)
      >>= (@?= Left (NoSuchKeyException "Bob"))
  ]
