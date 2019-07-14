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

data InvalidOutcomes =
    NoStoriesForHeading String
  | TooManyStoriesForHeading String Int
  | InvalidHeading String
  | DuplicateHeading String
  | UnrecognizedOutcomePattern [String]
  deriving (Show)
instance Exception InvalidOutcomes

separateOutcomes ∷ MonadThrow m ⇒ [(String, [Story])] → m ([(String, Story)], [Story])
separateOutcomes =
  foldM
    (\(outcomes, maybe_shames) (heading, stories) → if
      | heading ∈
          [ "Common Question"
          , "Common Story"
          , "Common Title"
          , "Success Choice"
          , "Success Story"
          , "Success Title"
          , "Danger Choice"
          , "Danger Question"
          , "Danger Story"
          , "Danger Title"
          , "Averted Choice"
          , "Averted Story"
          , "Averted Title"
          , "Failure Choice"
          , "Failure Story"
          , "Failure Title"
          ] →
            case stories of
              [story] → pure (((heading,story):outcomes), maybe_shames)
              _ → throwM $ TooManyStoriesForHeading heading (length stories)
      | heading == "Shame" → case maybe_shames of
          Nothing → pure (outcomes, (Just stories))
          Just _ → throwM $ DuplicateHeading "Shame"
      | otherwise → throwM $ InvalidHeading heading
    )
    ([], Nothing)
  >=>
  (\case
    (outcomes, Just shames) → pure (outcomes, shames)
    (_, Nothing) → throwM $ NoStoriesForHeading "Shame"
  )

extractOutcomeFrom ∷ [(String, Story)] → String → Story
extractOutcomeFrom outcomes heading =
  fromMaybe
    (error $ "invariant violated: no such heading " ⊕ heading)
    (lookup heading outcomes)

constructOutcomes ∷ MonadThrow m ⇒ ([(String, Story)], [Story]) → m (Outcomes Story)
constructOutcomes (outcomes, outcomes_shames) =
  case sort (keys outcomes) of
    ( [ "Common Question"
      , "Common Story"
      , "Common Title"
      , "Failure Choice"
      , "Failure Story"
      , "Failure Title"
      , "Success Choice"
      , "Success Story"
      , "Success Title"
      ] ) → pure $ SuccessFailure
        { outcomes_common_title = extractOutcome "Common Title"
        , outcomes_common_story = extractOutcome "Common Story"
        , outcomes_common_question = extractOutcome "Common Question"
        , outcomes_success_choice = extractOutcome "Success Choice"
        , outcomes_success_title = extractOutcome "Success Title"
        , outcomes_success_story = extractOutcome "Success Story"
        , outcomes_failure_choice = extractOutcome "Failure Choice"
        , outcomes_failure_title = extractOutcome "Failure Title"
        , outcomes_failure_story = extractOutcome "Failure Story"
        , outcomes_shames = outcomes_shames
        }
    ( [ "Averted Choice"
      , "Averted Story"
      , "Averted Title"
      , "Common Question"
      , "Common Story"
      , "Common Title"
      , "Failure Choice"
      , "Failure Story"
      , "Failure Title"
      , "Success Choice"
      , "Success Story"
      , "Success Title"
      ] ) → pure $ SuccessAvertedFailure
        { outcomes_common_title = extractOutcome "Common Title"
        , outcomes_common_story = extractOutcome "Common Story"
        , outcomes_common_question = extractOutcome "Common Question"
        , outcomes_success_choice = extractOutcome "Success Choice"
        , outcomes_success_title = extractOutcome "Success Title"
        , outcomes_success_story = extractOutcome "Success Story"
        , outcomes_averted_choice = extractOutcome "Averted Choice"
        , outcomes_averted_title = extractOutcome "Averted Title"
        , outcomes_averted_story = extractOutcome "Averted Story"
        , outcomes_failure_choice = extractOutcome "Failure Choice"
        , outcomes_failure_title = extractOutcome "Failure Title"
        , outcomes_failure_story = extractOutcome "Failure Story"
        , outcomes_shames = outcomes_shames
        }
    ( [ "Averted Choice"
      , "Averted Story"
      , "Averted Title"
      , "Common Question"
      , "Common Story"
      , "Common Title"
      , "Danger Choice"
      , "Danger Question"
      , "Danger Story"
      , "Danger Title"
      , "Failure Choice"
      , "Failure Story"
      , "Failure Title"
      , "Success Choice"
      , "Success Story"
      , "Success Title"
      ] ) → pure $ SuccessDangerAvertedFailure
        { outcomes_common_title = extractOutcome "Common Title"
        , outcomes_common_story = extractOutcome "Common Story"
        , outcomes_common_question = extractOutcome "Common Question"
        , outcomes_success_choice = extractOutcome "Success Choice"
        , outcomes_success_title = extractOutcome "Success Title"
        , outcomes_success_story = extractOutcome "Success Story"
        , outcomes_danger_choice = extractOutcome "Danger Choice"
        , outcomes_danger_title = extractOutcome "Danger Title"
        , outcomes_danger_story = extractOutcome "Danger Story"
        , outcomes_danger_question = extractOutcome "Danger Question"
        , outcomes_averted_choice = extractOutcome "Averted Choice"
        , outcomes_averted_title = extractOutcome "Averted Title"
        , outcomes_averted_story = extractOutcome "Averted Story"
        , outcomes_failure_choice = extractOutcome "Failure Choice"
        , outcomes_failure_title = extractOutcome "Failure Title"
        , outcomes_failure_story = extractOutcome "Failure Story"
        , outcomes_shames = outcomes_shames
        }
    x → throwM $ UnrecognizedOutcomePattern x
 where
  extractOutcome = extractOutcomeFrom outcomes

outcomes ∷ QuasiQuoter
outcomes = QuasiQuoter
  (parseSections >=> separateOutcomes >=> constructOutcomes >=> Lift.lift)
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

separateNarrative ∷ ∀ m. MonadThrow m ⇒ [(String, [Story])] → m (Narrative Story)
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
          "Title" → doFound _1
          "Story" → doFound _2
          _ → throwM $ InvalidHeading heading
      _ → throwM $ TooManyStoriesForHeading heading (length stories)
    )
    (Nothing, Nothing)
  >=>
  (\case
    (Just narrative_title, Just narrative_story) → pure $ Narrative{..}
    (Nothing, _) → throwM $ NoStoriesForHeading "Title"
    (_, _) → throwM $ NoStoriesForHeading "Story"
  )

narrative ∷ QuasiQuoter
narrative = QuasiQuoter
  (parseSections >=> separateNarrative >=> Lift.lift)
  (error "Cannot use narrative as a pattern")
  (error "Cannot use narrative as a type")
  (error "Cannot use narrative as a dec")

dashed_sections ∷ QuasiQuoter
dashed_sections = QuasiQuoter
  (splitStoriesOn '-' >>> (map pack ∷ [String] → [Text]) >>> Lift.lift)
  (error "Cannot use dashed_sections as a pattern")
  (error "Cannot use dashed_sections as a type")
  (error "Cannot use dashed_sections as a dec")
