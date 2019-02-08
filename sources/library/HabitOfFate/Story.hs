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

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(throwM))
import Data.List (dropWhileEnd, tail)
import Data.List.Split
import Data.Typeable (Typeable)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.FilePath ((</>))
import System.IO (readFile)

import HabitOfFate.Substitution

import Paths_habit_of_fate (getDataFileName)

addParagraphTags ∷ String → String
addParagraphTags text = "<p>" ⊕ go text
 where
  go ('\n':'\n':rest) = "</p><p>" ⊕ go rest
  go (x:rest) = x:go rest
  go [] = "</p>"

story ∷ QuasiQuoter
story = QuasiQuoter
  ((addParagraphTags >>> parseSubstitutions) >=> Lift.lift)
  (error "Cannot use story as a pattern")
  (error "Cannot use story as a type")
  (error "Cannot use story as a dec")

splitRegionsOn ∷ Char → [String] → [[String]]
splitRegionsOn marker =
  splitWhen (
    \case
      c:_ | c == marker → True
      _ → False
  )

splitStoriesOn ∷ Char → String → [String]
splitStoriesOn c =
  lines
  >>>
  splitRegionsOn c
  >>>
  map (unlines >>> addParagraphTags)

splitAndParseSubstitutions ∷ MonadThrow m ⇒ String → m [Story]
splitAndParseSubstitutions = splitStoriesOn '=' >>> mapM parseSubstitutions

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
    map (unlines >>> addParagraphTags)
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

splitNamedRegionsOn ∷ Char → String → [(String, String)]
splitNamedRegionsOn marker =
  -- Break the string into lines to make it easier to see the deliminators.
      lines

  -- Split into regions deliminated by lines starting with the given marker
  >>> split (keepDelimsL $ whenElt (\line → line ^? _head == Just marker))

  -- Drop the first region; in theory it could be absent but in practice there
  -- will always be whitespace.
  >>> tail

  -- Extract the label from each region; note that an invariant of the split is
  -- that every region must have its first line be the header as deliminated by
  -- the marker.
  >>> map (\(header:text) →
        ( header
            |> dropWhile (== marker)
            |> dropWhile (== ' ')
            |> dropWhileEnd (== marker)
            |> dropWhileEnd (== ' ')
        , text |> unlines |> addParagraphTags
        )
      )

data StoryOutcomes = StoryOutcomes
  { _story_common_ ∷ Story
  , _story_success_ ∷ Story
  , _story_success_or_averted_ ∷ Story
  , _story_averted_or_failure_ ∷ Story
  , _story_averted_ ∷ Story
  , _story_failure_ ∷ Story
  , _story_fame_ ∷ [Story]
  , _story_shame_ ∷ [Story]
  } deriving (Eq,Lift,Ord,Read,Show)
makeLenses ''StoryOutcomes

instance Default StoryOutcomes where
  def = StoryOutcomes def def def def def def def def

story_outcome_singleton_labels ∷ [(String, ALens' StoryOutcomes Story)]
story_outcome_singleton_labels =
  [("Common", story_common_)
  ,("Success", story_success_)
  ,("Success/Averted", story_success_or_averted_)
  ,("Averted/Failure", story_averted_or_failure_)
  ,("Averted", story_averted_)
  ,("Failure", story_failure_)
  ]

story_outcome_multiple_labels ∷ [(String, ALens' StoryOutcomes [Story])]
story_outcome_multiple_labels =
  [("Fame", story_fame_)
  ,("Shame", story_shame_)
  ]

data WrongNumberOfStories = NoStoriesForLabel String | TooManyStoriesForLabel String Int
  deriving (Show)
instance Exception WrongNumberOfStories

story_outcome_modifiers ∷ MonadThrow m ⇒ [(String, [Story] → StoryOutcomes → m StoryOutcomes)]
story_outcome_modifiers =
  [ ( label,
      \stories outcomes → case stories of
       [story] → pure (outcomes & (lens_ #~ story))
       [] → throwM $ NoStoriesForLabel label
       _ → throwM $ TooManyStoriesForLabel label (length stories)
    )
  | ( label, lens_ ) ← story_outcome_singleton_labels
  ]
  ⊕
  [ ( label,
      \stories outcomes → pure (outcomes & (lens_ #~ stories))
    )
  | ( label, lens_ ) ← story_outcome_multiple_labels
  ]

data NoSuchStoryOutcomeLabel = NoSuchStoryOutcomeLabel String deriving (Show, Typeable)
instance Exception NoSuchStoryOutcomeLabel where

extractStoryOutcomes ∷ MonadThrow m ⇒ String → m StoryOutcomes
extractStoryOutcomes regions =
  (regions
    |> splitNamedRegionsOn '='
    |> map (second (splitStoriesOn '-'))
    |> traverse (\(label, region) → (label,) <$> mapM parseSubstitutions region)
  )
  >>=
  foldlM
    (\outcomes (label, region) →
      maybe
        (throwM $ NoSuchStoryOutcomeLabel label)
        (\f → f region outcomes)
        (lookup label story_outcome_modifiers)
    )
    def

story_outcomes ∷ QuasiQuoter
story_outcomes = QuasiQuoter
  (extractStoryOutcomes >=> Lift.lift)
  (error "Cannot use story_outcomes as a pattern")
  (error "Cannot use story_outcomes as a type")
  (error "Cannot use story_outcomes as a dec")

{-
Typical usage:

... = [story_outcomes|
------------------------------------ Common ------------------------------------
------------------------------------ Success -----------------------------------
-------------------------------- Averted/Failure -------------------------------
------------------------------------ Averted -----------------------------------
------------------------------------ Failure -----------------------------------
------------------------------------- Fame -------------------------------------
------------------------------------- Shame ------------------------------------
|]
-}

storyForSuccess ∷ StoryOutcomes → Story
storyForSuccess StoryOutcomes{..} = _story_common_ ⊕ _story_success_or_averted_ ⊕ _story_success_

storyForAverted ∷ StoryOutcomes → Story
storyForAverted StoryOutcomes{..} = _story_common_ ⊕ _story_success_or_averted_ ⊕ _story_averted_or_failure_ ⊕ _story_averted_

storyForFailure ∷ StoryOutcomes → Story
storyForFailure StoryOutcomes{..} = _story_common_ ⊕ _story_averted_or_failure_ ⊕ _story_failure_

data PageChoices = DeadEnd | NoChoice Text | Choices Text [(Text,Text)] deriving (Eq,Ord,Read,Show)

linked_page_ids_ ∷ Traversal' PageChoices Text
linked_page_ids_ _ DeadEnd = pure DeadEnd
linked_page_ids_ f (NoChoice c) = NoChoice <$> f c
linked_page_ids_ f (Choices query choices) =
  Choices query <$> traverse (traverseOf _2 f) choices
