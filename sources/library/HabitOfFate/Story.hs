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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story
  ( Story
  , story
  , stories
  , stories_and_labels

  , InvalidOutcomes(..)
  , outcomes

  , storyForSuccess
  , storyForAverted
  , storyForFailure

  , Narrative(..)
  , narrative

  , dashed_sections
  ) where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(throwM))
import Data.List (dropWhileEnd, tail, unzip)
import Data.List.Split
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Substitution

applyToFirst ∷ Applicative t ⇒ (α → t β) → (α,δ) → t (β,δ)
applyToFirst f (x,y) = f x <&> (,y)

story ∷ QuasiQuoter
story = QuasiQuoter
  (parseSubstitutions >=> Lift.lift)
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
  map unlines

splitAndParseSubstitutions ∷ MonadThrow m ⇒ String → m [Story]
splitAndParseSubstitutions =
  splitStoriesOn '='
  >>>
  filter (not <<< allSpaces)
  >>>
  mapM parseSubstitutions

stories ∷ QuasiQuoter
stories = QuasiQuoter
  (splitAndParseSubstitutions >=> Lift.lift)
  (error "Cannot use stories as a pattern")
  (error "Cannot use stories as a type")
  (error "Cannot use stories as a dec")

splitNamedRegionsOn ∷ Char → String → [(String, String)]
splitNamedRegionsOn marker =
  -- Strip any initial whitespace
      dropWhile (/= marker)

  -- Break the string into lines to make it easier to see the deliminators.
  >>> lines

  -- Split into regions deliminated by lines starting with the given marker
  >>> split (keepDelimsL $ whenElt (\line → line ^? _head == Just marker))

  -- Drop the first empty region.
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
        , text |> unlines
        )
      )

parseSections ∷ MonadThrow m ⇒ String → m [(String, [Story])]
parseSections =
  splitNamedRegionsOn '='
  >>>
  map (second (splitStoriesOn '-' >>> map (dropWhileEnd (== '\n'))))
  >>>
  traverse (\(label, region) → (label,) <$> mapM parseSubstitutions region)

stories_and_labels ∷ QuasiQuoter
stories_and_labels = QuasiQuoter
  (\content → do
    let (labels, bodies) =
          content
            |> splitNamedRegionsOn '='
            |> unzip
    stories ← traverse parseSubstitutions bodies
    Lift.lift ((stories, map pack labels) ∷ ([Story], [Text]))
  )
  (error "Cannot use stories_with_labels as a pattern")
  (error "Cannot use stories_with_labels as a type")
  (error "Cannot use stories_with_labels as a dec")

data OutcomeHeading =
    CommonTitle
  | CommonStory
  | CommonQuestion
  | SuccessChoice
  | SuccessTitle
  | SuccessStory
  | DangerChoice
  | DangerTitle
  | DangerStory
  | DangerQuestion
  | AvertedChoice
  | AvertedTitle
  | AvertedStory
  | FailureChoice
  | FailureTitle
  | FailureStory
  | Shame
  deriving (Eq,Ord,Show)

data InvalidOutcomes =
    NoStoriesForHeading OutcomeHeading
  | EmptyStoryForHeading OutcomeHeading
  | TooManyStoriesForHeading OutcomeHeading Int
  | InvalidHeading String
  | UnrecognizedOutcomePattern [OutcomeHeading]
  deriving (Show)
instance Exception InvalidOutcomes

parseOutcomeHeading ∷ MonadThrow m ⇒ String → m OutcomeHeading
parseOutcomeHeading = \case
  "Common Title" → pure CommonTitle
  "Common Story" → pure CommonStory
  "Common Question" → pure CommonQuestion
  "Success Choice" → pure SuccessChoice
  "Success Title" → pure SuccessTitle
  "Success Story" → pure SuccessStory
  "Danger Choice" → pure DangerChoice
  "Danger Title" → pure DangerTitle
  "Danger Story" → pure DangerStory
  "Danger Question" → pure DangerQuestion
  "Averted Choice" → pure AvertedChoice
  "Averted Title" → pure AvertedTitle
  "Averted Story" → pure AvertedStory
  "Failure Choice" → pure FailureChoice
  "Failure Title" → pure FailureTitle
  "Failure Story" → pure FailureStory
  "Shame" → pure Shame
  invalid_heading → throwM $ InvalidHeading invalid_heading

gatherOutcomeStories ∷ MonadThrow m ⇒ [(OutcomeHeading, [Story])] → m (Map OutcomeHeading Story, [Story])
gatherOutcomeStories =
  foldM
    (\(outcome_map, shames) (heading, stories) →
      if heading == Shame
        then pure (outcome_map, shames ⊕ stories)
        else case stories of
          [] → throwM $ NoStoriesForHeading heading
          [more_story]
            | onull more_story → throwM $ EmptyStoryForHeading heading
            | otherwise → pure
                ( alterMap
                    ( maybe
                        more_story
                        (\story → if onull story
                          then more_story
                          else story ⊕ "\n\n" ⊕ more_story
                        )
                      >>>
                      Just
                    )
                    heading
                    outcome_map
                , shames
                )
          _ → throwM $ TooManyStoriesForHeading heading (length stories)
    )
    (mempty, mempty)
  >=>
  (\x@(outcome_map, shames) →
    if onull shames
      then throwM $ NoStoriesForHeading Shame
      else pure x
  )

commonFailureSuccess, commonAvertedFailureSuccess, commonDangerAvertedFailureSuccess ∷ Set OutcomeHeading

commonFailureSuccess =
  [ CommonQuestion
  , CommonStory
  , CommonTitle
  , FailureChoice
  , FailureStory
  , FailureTitle
  , SuccessChoice
  , SuccessStory
  , SuccessTitle
  ]

commonAvertedFailureSuccess =
  [ AvertedChoice
  , AvertedStory
  , AvertedTitle
  , CommonQuestion
  , CommonStory
  , CommonTitle
  , FailureChoice
  , FailureStory
  , FailureTitle
  , SuccessChoice
  , SuccessStory
  , SuccessTitle
  ]

commonDangerAvertedFailureSuccess =
  [ AvertedChoice
  , AvertedStory
  , AvertedTitle
  , CommonQuestion
  , CommonStory
  , CommonTitle
  , DangerChoice
  , DangerQuestion
  , DangerStory
  , DangerTitle
  , FailureChoice
  , FailureStory
  , FailureTitle
  , SuccessChoice
  , SuccessStory
  , SuccessTitle
  ]

constructOutcomes ∷ MonadThrow m ⇒ (Map OutcomeHeading Story, [Story]) → m (Outcomes Story)
constructOutcomes (outcomes, outcomes_shames)
  | headings_set == commonFailureSuccess = pure $ SuccessFailure
      { outcomes_common_title = extractOutcome CommonTitle
      , outcomes_common_story = extractOutcome CommonStory
      , outcomes_common_question = extractOutcome CommonQuestion
      , outcomes_success_choice = extractOutcome SuccessChoice
      , outcomes_success_title = extractOutcome SuccessTitle
      , outcomes_success_story = extractOutcome SuccessStory
      , outcomes_failure_choice = extractOutcome FailureChoice
      , outcomes_failure_title = extractOutcome FailureTitle
      , outcomes_failure_story = extractOutcome FailureStory
      , outcomes_shames = outcomes_shames
      }
  | headings_set == commonAvertedFailureSuccess = pure $ SuccessAvertedFailure
      { outcomes_common_title = extractOutcome CommonTitle
      , outcomes_common_story = extractOutcome CommonStory
      , outcomes_common_question = extractOutcome CommonQuestion
      , outcomes_success_choice = extractOutcome SuccessChoice
      , outcomes_success_title = extractOutcome SuccessTitle
      , outcomes_success_story = extractOutcome SuccessStory
      , outcomes_averted_choice = extractOutcome AvertedChoice
      , outcomes_averted_title = extractOutcome AvertedTitle
      , outcomes_averted_story = extractOutcome AvertedStory
      , outcomes_failure_choice = extractOutcome FailureChoice
      , outcomes_failure_title = extractOutcome FailureTitle
      , outcomes_failure_story = extractOutcome FailureStory
      , outcomes_shames = outcomes_shames
      }
  | headings_set == commonDangerAvertedFailureSuccess = pure SuccessDangerAvertedFailure
      { outcomes_common_title = extractOutcome CommonTitle
      , outcomes_common_story = extractOutcome CommonStory
      , outcomes_common_question = extractOutcome CommonQuestion
      , outcomes_success_choice = extractOutcome SuccessChoice
      , outcomes_success_title = extractOutcome SuccessTitle
      , outcomes_success_story = extractOutcome SuccessStory
      , outcomes_danger_choice = extractOutcome DangerChoice
      , outcomes_danger_title = extractOutcome DangerTitle
      , outcomes_danger_story = extractOutcome DangerStory
      , outcomes_danger_question = extractOutcome DangerQuestion
      , outcomes_averted_choice = extractOutcome AvertedChoice
      , outcomes_averted_title = extractOutcome AvertedTitle
      , outcomes_averted_story = extractOutcome AvertedStory
      , outcomes_failure_choice = extractOutcome FailureChoice
      , outcomes_failure_title = extractOutcome FailureTitle
      , outcomes_failure_story = extractOutcome FailureStory
      , outcomes_shames = outcomes_shames
      }
    | otherwise = throwM $ UnrecognizedOutcomePattern $ toList $ headings_set
 where
  headings_set = keysSet outcomes

  extractOutcome ∷ OutcomeHeading → Story
  extractOutcome heading =
    fromMaybe
      (error [i|invariant violated: no such heading ${heading}|])
      (lookup heading outcomes)

outcomes ∷ QuasiQuoter
outcomes = QuasiQuoter
  (
    parseSections
    >=>
    traverse (\(heading,stories) → parseOutcomeHeading heading <&> (,stories))
    >=>
    gatherOutcomeStories
    >=>
    constructOutcomes
    >=>
    Lift.lift
  )
  (error "Cannot use outcomes as a pattern")
  (error "Cannot use outcomes as a type")
  (error "Cannot use outcomes as a dec")

{-
Typical usage:

... = [outcomes_outcomes|
================================= Common Title =================================

================================= Common Story =================================

================================ Common Question ===============================

================================ Success Choice ================================

================================= Success Title ================================

================================= Success Story ================================

================================= Danger Choice ================================

================================= Danger Title =================================

================================= Danger Story =================================

================================ Danger Question ===============================

================================ Averted Choice ================================

================================= Averted Title ================================

================================= Averted Story ================================

================================ Failure Choice ================================

================================= Failure Title ================================

================================= Failure Story ================================

===================================== Shame ====================================

--------------------------------------------------------------------------------

|]
-}

paragraphs ∷ [Markdown] → Markdown
paragraphs = intersperse (Markdown "\n\n") >>> mconcat

storyForSuccess, storyForAverted, storyForFailure ∷ Outcomes Markdown → Markdown

storyForSuccess SuccessFailure{..} =
  paragraphs [outcomes_common_story, outcomes_success_story]
storyForSuccess SuccessAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_success_story]
storyForSuccess SuccessDangerAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_success_story]

storyForAverted SuccessFailure{..} =
  paragraphs [outcomes_common_story, outcomes_success_story]
storyForAverted SuccessAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_averted_story]
storyForAverted SuccessDangerAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_danger_story, outcomes_averted_story]

storyForFailure SuccessFailure{..} =
  paragraphs [outcomes_common_story, outcomes_failure_story]
storyForFailure SuccessAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_failure_story]
storyForFailure SuccessDangerAvertedFailure{..} =
  paragraphs [outcomes_common_story, outcomes_danger_story, outcomes_failure_story]

data Narrative content = Narrative
  { narrative_title ∷ content
  , narrative_story ∷ content
  } deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

data NarrativeHeading = Title | Story deriving (Eq,Ord,Show)

data InvalidNarrative =
    InvalidNarrativeHeading String
  | TooManyStoriesForNarrativeHeading NarrativeHeading Int
  | NoStoriesForNarrativeHeading NarrativeHeading
  | DuplicateHeading NarrativeHeading
  deriving (Eq,Show)
instance Exception InvalidNarrative where

parseNarrativeHeading ∷ ∀ m. MonadThrow m ⇒ String → m NarrativeHeading
parseNarrativeHeading = \case
  "Title" → pure Title
  "Story" → pure Story
  other → throwM $ InvalidNarrativeHeading other

separateNarrative ∷ ∀ m. MonadThrow m ⇒ [(NarrativeHeading, [Story])] → m (Narrative Story)
separateNarrative =
  foldM
    (\found (heading, stories) → case stories of
      [story] →
        let doFound ∷ Lens' (Maybe Story, Maybe Story) (Maybe Story) → m (Maybe Story, Maybe Story)
            doFound lens_ =
              case found ^. lens_ of
                Nothing → pure $ (found & lens_ .~ Just story)
                _ → throwM $ DuplicateHeading heading
        in case heading of
          Title → doFound _1
          Story → doFound _2
      _ → throwM $ TooManyStoriesForNarrativeHeading heading (length stories)
    )
    (Nothing, Nothing)
  >=>
  (\case
    (Just narrative_title, Just narrative_story) → pure $ Narrative{..}
    (Nothing, _) → throwM $ NoStoriesForNarrativeHeading Title
    (_, _) → throwM $ NoStoriesForNarrativeHeading Story
  )

narrative ∷ QuasiQuoter
narrative = QuasiQuoter
  (
    parseSections
    >=>
    traverse (applyToFirst parseNarrativeHeading)
    >=>
    separateNarrative
    >=>
    Lift.lift
  )
  (error "Cannot use narrative as a pattern")
  (error "Cannot use narrative as a type")
  (error "Cannot use narrative as a dec")

dashed_sections ∷ QuasiQuoter
dashed_sections = QuasiQuoter
  (splitStoriesOn '-' >>> (map pack ∷ [String] → [Text]) >>> Lift.lift)
  (error "Cannot use dashed_sections as a pattern")
  (error "Cannot use dashed_sections as a type")
  (error "Cannot use dashed_sections as a dec")
