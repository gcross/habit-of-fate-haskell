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
{-# LANGUAGE ScopedTypeVariables #-}
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
    map unlines
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
        , text |> unlines
        )
      )

parseSections ∷ MonadThrow m ⇒ String → m [(String, [Story])]
parseSections =
  splitNamedRegionsOn '='
  >>>
  map (second (splitStoriesOn '-'))
  >>>
  traverse (\(label, region) → (label,) <$> mapM parseSubstitutions region)

data Outcomes content =
    SuccessFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      }
  | SuccessAvertedFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_averted_choice ∷ content
      , outcomes_averted_title ∷ content
      , outcomes_averted_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      }
  | SuccessDangerAvertedFailure
      { outcomes_common_title ∷ content
      , outcomes_common_story ∷ content
      , outcomes_common_question ∷ content
      , outcomes_success_choice ∷ content
      , outcomes_success_title ∷ content
      , outcomes_success_story ∷ content
      , outcomes_danger_choice ∷ content
      , outcomes_danger_title ∷ content
      , outcomes_danger_story ∷ content
      , outcomes_danger_question ∷ content
      , outcomes_averted_choice ∷ content
      , outcomes_averted_title ∷ content
      , outcomes_averted_story ∷ content
      , outcomes_failure_choice ∷ content
      , outcomes_failure_title ∷ content
      , outcomes_failure_story ∷ content
      , outcomes_shames ∷ [content]
      } deriving (Eq,Foldable,Functor,Lift,Ord,Read,Show,Traversable)

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
      ] ) → let outcomes_common_title = extractOutcome "Common Title"
                outcomes_common_story = extractOutcome "Common Story"
                outcomes_common_question = extractOutcome "Common Question"
                outcomes_success_choice = extractOutcome "Success Choice"
                outcomes_success_title = extractOutcome "Success Title"
                outcomes_success_story = extractOutcome "Success Story"
                outcomes_failure_choice = extractOutcome "Failure Choice"
                outcomes_failure_title = extractOutcome "Failure Title"
                outcomes_failure_story = extractOutcome "Failure Story"
            in pure $ SuccessFailure{..}
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
      ] ) → let outcomes_common_title = extractOutcome "Common Title"
                outcomes_common_story = extractOutcome "Common Story"
                outcomes_common_question = extractOutcome "Common Question"
                outcomes_success_choice = extractOutcome "Success Choice"
                outcomes_success_title = extractOutcome "Success Title"
                outcomes_success_story = extractOutcome "Success Story"
                outcomes_averted_choice = extractOutcome "Averted Choice"
                outcomes_averted_title = extractOutcome "Averted Title"
                outcomes_averted_story = extractOutcome "Averted Story"
                outcomes_failure_choice = extractOutcome "Failure Choice"
                outcomes_failure_title = extractOutcome "Failure Title"
                outcomes_failure_story = extractOutcome "Failure Story"
            in pure $ SuccessAvertedFailure{..}
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
      ] ) → let outcomes_common_title = extractOutcome "Common Title"
                outcomes_common_story = extractOutcome "Common Story"
                outcomes_common_question = extractOutcome "Common Question"
                outcomes_success_choice = extractOutcome "Success Choice"
                outcomes_success_title = extractOutcome "Success Title"
                outcomes_success_story = extractOutcome "Success Story"
                outcomes_danger_choice = extractOutcome "Danger Choice"
                outcomes_danger_title = extractOutcome "Danger Title"
                outcomes_danger_story = extractOutcome "Danger Story"
                outcomes_danger_question = extractOutcome "Danger Question"
                outcomes_averted_choice = extractOutcome "Averted Choice"
                outcomes_averted_title = extractOutcome "Averted Title"
                outcomes_averted_story = extractOutcome "Averted Story"
                outcomes_failure_choice = extractOutcome "Failure Choice"
                outcomes_failure_title = extractOutcome "Failure Title"
                outcomes_failure_story = extractOutcome "Failure Story"
            in pure $ SuccessDangerAvertedFailure{..}
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

data PageChoices =
    DeadEnd
  | NoChoice Text
  | Choices Text [(Text,Text)]
  deriving (Eq,Ord,Read,Show)

linked_page_ids_ ∷ Traversal' PageChoices Text
linked_page_ids_ _ DeadEnd = pure DeadEnd
linked_page_ids_ f (NoChoice c) = NoChoice <$> f c
linked_page_ids_ f (Choices query choices) =
  Choices query <$> traverse (traverseOf _2 f) choices
