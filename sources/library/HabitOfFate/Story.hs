{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2018 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import Control.Monad.Catch (MonadThrow)
import Data.List.Split
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.FilePath ((</>))
import System.IO (readFile)

import HabitOfFate.Substitution

import Paths_habit_of_fate (getDataFileName)

type Story = [Chunk Text]

splitRegionsOn ∷ Char → [String] → [[String]]
splitRegionsOn marker =
  splitWhen (
    \case
      c:_ | c == marker → True
      _ → False
  )

addParagraphTags ∷ [String] → [String]
addParagraphTags = go False
 where
  go False [] = []
  go False (line:rest) =
    case find ((∈ " \t") >>> not) line of
      Nothing → go False rest
      Just '<' → line:go False rest
      _ → "<p>":line:go True rest
  go True [] = ["</p>"]
  go True (line:rest)
    | all (∈ " \t") line = "</p>":go True rest
    | otherwise = line:go True rest

splitStories ∷ String → [String]
splitStories =
  lines
  >>>
  splitRegionsOn '='
  >>>
  map (addParagraphTags >>> unlines)

splitAndParseSubstitutions ∷ MonadThrow m ⇒ String → m [Story]
splitAndParseSubstitutions = splitStories >>> mapM parseSubstitutions

stories ∷ QuasiQuoter
stories = QuasiQuoter
  (splitAndParseSubstitutions >=> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

stories_fixed ∷ QuasiQuoter
stories_fixed = QuasiQuoter
  (
    splitAndParseSubstitutions
    >=>
    (\case
      [] → [|()|]
      [x1] → [|x1|]
      [x1,x2] → [|(x1,x2)|]
      [x1,x2,x3] → [|(x1,x2,x3)|]
      [x1,x2,x3,x4] → [|(x1,x2,x3,x4)|]
      [x1,x2,x3,x4,x5] → [|(x1,x2,x3,x4,x5)|]
      [x1,x2,x3,x4,x5,x6] → [|(x1,x2,x3,x4,x5,x6)|]
      [x1,x2,x3,x4,x5,x6,x7] → [|(x1,x2,x3,x4,x5,x6,x7)|]
      [x1,x2,x3,x4,x5,x6,x7,x8] → [|(x1,x2,x3,x4,x5,x6,x7,x8)|]
      [x1,x2,x3,x4,x5,x6,x7,x8,x9] → [|(x1,x2,x3,x4,x5,x6,x7,x8,x9)|]
      [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] → [|(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)|]
      xs → error [i|saw #{olength xs} events, which is too many (> 10)|]
    )
  )
  (error "Cannot use s1 as a pattern")
  (error "Cannot use s1 as a type")
  (error "Cannot use s1 as a dec")

loadStories ∷ String → String → Q Exp
loadStories quest section =
  liftIO (
    getDataFileName ("stories" </> quest </> section ⊕ ".txt")
    >>=
    readFile
  )
  >>=
  splitAndParseSubstitutions
  >>=
  Lift.lift

subSplitStories ∷ String → [[String]]
subSplitStories =
  lines
  >>>
  splitRegionsOn '='
  >>>
  map (
    splitRegionsOn '-'
    >>>
    map (addParagraphTags >>> unlines)
  )

subSplitAndParseSubstitutions ∷ MonadThrow m ⇒ String → m [[Story]]
subSplitAndParseSubstitutions = subSplitStories >>> mapM (mapM parseSubstitutions)

subStories ∷ QuasiQuoter
subStories = QuasiQuoter
  (subSplitAndParseSubstitutions >=> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

loadSubStoriesWithoutLifting ∷ (MonadIO m, MonadThrow m) ⇒ String → String → m [[Story]]
loadSubStoriesWithoutLifting quest section =
  liftIO (
    getDataFileName ("stories" </> quest </> section ⊕ ".txt")
    >>=
    readFile
  )
  >>=
  subSplitAndParseSubstitutions

loadSubStories ∷ String → String → Q Exp
loadSubStories quest section =
  loadSubStoriesWithoutLifting quest section
  >>=
  Lift.lift
