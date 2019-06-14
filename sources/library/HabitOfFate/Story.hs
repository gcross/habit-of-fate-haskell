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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Story where

import HabitOfFate.Prelude

import Data.List (dropWhileEnd, tail)
import Data.List.Split
import Data.Typeable (Typeable)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lift (Lift)
import qualified Language.Haskell.TH.Lift as Lift
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.FilePath ((</>))

import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.JSON
import HabitOfFate.Substitution

import Paths_habit_of_fate (getDataFileName)

story ∷ QuasiQuoter
story = QuasiQuoter
  (parseSubstitutions >>> Lift.lift)
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

splitAndParseSubstitutions ∷ String → [Story]
splitAndParseSubstitutions = splitStoriesOn '=' >>> map parseSubstitutions

stories ∷ QuasiQuoter
stories = QuasiQuoter
  (splitAndParseSubstitutions >>> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

stories_fixed ∷ QuasiQuoter
stories_fixed = QuasiQuoter
  (
    splitAndParseSubstitutions
    >>>
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

subSplitStories ∷ String → [[String]]
subSplitStories =
  lines
  >>>
  splitRegionsOn '='
  >>>
  map (
    splitRegionsOn '-'
    >>>
    map unlines
  )

subSplitAndParseSubstitutions ∷ String → [[Story]]
subSplitAndParseSubstitutions = subSplitStories >>> map (map parseSubstitutions)

subStories ∷ QuasiQuoter
subStories = QuasiQuoter
  (subSplitAndParseSubstitutions >>> Lift.lift)
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")

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

parseSections ∷ String → [(String, [Story])]
parseSections =
  splitNamedRegionsOn '='
  >>>
  map (second (splitStoriesOn '-' >>> map (dropWhileEnd (== '\n'))))
  >>>
  map (second (map parseSubstitutions))

separateOutcomes ∷ [(String, [Story])] → ([(String, Story)], [Story])
separateOutcomes =
  foldl'
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
              [story] → (((heading,story):outcomes), maybe_shames)
              _ → error [i|Too many stories for heading "#{heading}" (#{length stories})|]
      | heading == "Shame" → case maybe_shames of
          Nothing → (outcomes, Just stories)
          Just _ → error [i|Duplicate heading "Shame"|]
      | otherwise → error [i|Invalid heading "#{heading}"|]
    )
    ([], Nothing)
  >>>
  (\case
    (outcomes, Just shames) → (outcomes, shames)
    (_, Nothing) → error [i|No stories for heading "Shame"|]
  )

extractOutcomeFrom ∷ [(String, Story)] → String → Story
extractOutcomeFrom outcomes heading =
  fromMaybe
    (error $ "invariant violated: no such heading " ⊕ heading)
    (lookup heading outcomes)

constructOutcomes ∷ ([(String, Story)], [Story]) → Outcomes Story
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
      ] ) → SuccessFailure
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
      ] ) → SuccessAvertedFailure
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
      ] ) → SuccessDangerAvertedFailure
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
    x → error [i|Unrecognized outcome pattern: #{x}|]
 where
  extractOutcome = extractOutcomeFrom outcomes

outcomes ∷ QuasiQuoter
outcomes = QuasiQuoter
  (parseSections >>> separateOutcomes >>> constructOutcomes >>> Lift.lift)
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

storyForSuccess ∷ Semigroup content ⇒ Outcomes content → content
storyForSuccess SuccessFailure{..} = outcomes_common_story ⊕ outcomes_success_story
storyForSuccess SuccessAvertedFailure{..} = outcomes_common_story ⊕ outcomes_success_story
storyForSuccess SuccessDangerAvertedFailure{..} = outcomes_common_story ⊕ outcomes_success_story

storyForAverted ∷ Semigroup content ⇒ Outcomes content → content
storyForAverted SuccessFailure{..} = outcomes_common_story ⊕ outcomes_success_story
storyForAverted SuccessAvertedFailure{..} = outcomes_common_story ⊕ outcomes_averted_story
storyForAverted SuccessDangerAvertedFailure{..} = outcomes_common_story ⊕ outcomes_danger_story ⊕ outcomes_averted_story

storyForFailure ∷ Semigroup content ⇒ Outcomes content → content
storyForFailure SuccessFailure{..} = outcomes_common_story ⊕ outcomes_failure_story
storyForFailure SuccessAvertedFailure{..} = outcomes_common_story ⊕ outcomes_failure_story
storyForFailure SuccessDangerAvertedFailure{..} = outcomes_common_story ⊕ outcomes_danger_story ⊕ outcomes_failure_story

data Narrative content = Narrative
  { narrative_title ∷ content
  , narrative_story ∷ content
  } deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

separateNarrative ∷ [(String, [Story])] → Narrative Story
separateNarrative =
  foldl'
    (\found (heading, stories) → case stories of
      [story] →
        let doFound ∷ Lens' (Maybe Story, Maybe Story) (Maybe Story) → (Maybe Story, Maybe Story)
            doFound lens_ =
              case found ^. lens_ of
                Nothing → found & lens_ .~ Just story
                _ → error [i|Duplicate heading "#{heading}"|]
        in case heading of
          "Title" → doFound _1
          "Story" → doFound _2
          _ → error [i|Invalid heading "#{heading}"|]
      _ → error [i|Too many stories for heading "#{heading}" (#{length stories})|]
    )
    (Nothing, Nothing)
  >>>
  (\case
    (Just narrative_title, Just narrative_story) → Narrative{..}
    (Nothing, _) → error [i|No stories for heading "Title"|]
    (_, _) → error [i|No stories for heading "Story"|]
  )

narrative ∷ QuasiQuoter
narrative = QuasiQuoter
  (parseSections >>> separateNarrative >>> Lift.lift)
  (error "Cannot use narrative as a pattern")
  (error "Cannot use narrative as a type")
  (error "Cannot use narrative as a dec")
