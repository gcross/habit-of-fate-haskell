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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.Run (handler) where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception(displayException), throwM)
import Control.Monad.Random (uniform, weighted)
import qualified Data.Text.Lazy as Lazy
import Data.Time.LocalTime (LocalTime)
import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.Outcomes
import HabitOfFate.Data.QuestState
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Data.Tagged
import HabitOfFate.Quests
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction
import HabitOfFate.Story
import HabitOfFate.Trial

data InconsistentQuestState = UnexpectedEndOfContent Text deriving (Eq,Show)
instance Exception InconsistentQuestState where
  displayException (UnexpectedEndOfContent name) =
    [i|"Reached the end of content before seeing an event or narrative in "#{name}"."|]

data QuestResult = Stuck | Advancing | Failed Markdown

data NextIs = NextIsStuck | NextIsNarrative | NextIsEvent

type QuestStateT = StateT (QuestState Markdown) Transaction

runGame ∷ Transaction (Markdown, Bool)
runGame = do
  marks ← use marks_

  let resetQuestState ∷ QuestStateT ()
      resetQuestState = do
        quest_state ← randomQuestState
        interlude ← uniform interludes
        put (quest_state & quest_state_remaining_content_ %~ (NarrativeContent interlude:))

      walkFirst ∷ QuestStateT (Markdown, Marks, (NextIs, Maybe (LocalTime → Deed)))
      walkFirst =
        use quest_state_remaining_content_ >>= \case
        [] → use quest_state_name_ >>= (UnexpectedEndOfContent >>> throwM)
        (RandomStoriesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_random_stories_ .= c
          walkFirst
        (StatusContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_status_ .= c
          walkFirst
        (FamesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_fames_ .= c
          walkFirst
        (NarrativeContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          (c, marks, ) <$> walkSecond Nothing
        (EventContent outcomes:rest_content) → do
          let randomStory ∷ Marks → QuestStateT (Markdown, QuestResult, Marks)
              randomStory new_marks =
                (use quest_state_random_stories_ >>= uniform)
                <&>
                (, Stuck, new_marks)
          (c, result, new_marks) ←
            case (uncons (marks ^. success_), uncons (marks ^. failure_)) of
              (Just (scale, rest), _) → Just (scale, success_, rest, SuccessResult)
              (_, Just (scale, rest)) → Just (scale, failure_, rest, FailureResult)
              _                       → Nothing
            &
            \case
              Nothing → randomStory marks
              Just (scale, lens_, rest, result) → do
                let new_marks = marks & lens_ .~ rest
                tryBinomial (1/3) scale >>= bool
                  (randomStory new_marks)
                  (case result of
                    SuccessResult → pure (storyForSuccess outcomes, Advancing, new_marks)
                    FailureResult →
                      let failure = do
                            shame ← uniform (outcomes & outcomes_shames)
                            pure (storyForFailure outcomes, Failed shame, new_marks)
                          tryAverted = weighted [(SuccessResult, 1), (FailureResult, 2)] >>= \case
                            SuccessResult → pure (storyForAverted outcomes, Advancing, new_marks)
                            FailureResult → failure
                      in case outcomes of
                        SuccessFailure{..} → failure
                        SuccessAvertedFailure{..} → tryAverted
                        SuccessDangerAvertedFailure{..} → tryAverted
                  )
          case result of
            Stuck → do
              pure (c, new_marks, (NextIsStuck, Nothing))
            Advancing → do
              quest_state_remaining_content_ .= rest_content
              (c, new_marks, ) <$> walkSecond Nothing
            Failed shame → do
              resetQuestState
              (c, new_marks, ) <$> walkSecond (Just $ Deed FailureResult shame)

      walkSecond ∷ Maybe (LocalTime → Deed) → QuestStateT (NextIs, Maybe (LocalTime → Deed))
      walkSecond maybe_makeDeedFromTime =
        use quest_state_remaining_content_ >>= \case
        [] → do
          if isJust maybe_makeDeedFromTime
            then use quest_state_name_ >>= (UnexpectedEndOfContent >>> throwM)
            else do
              deed ← Deed SuccessResult <$> (use quest_state_fames_ >>= uniform)
              resetQuestState
              walkSecond (Just deed)
        (RandomStoriesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_random_stories_ .= c
          walkSecond maybe_makeDeedFromTime
        (StatusContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_status_ .= c
          walkSecond maybe_makeDeedFromTime
        (FamesContent c:rest_content) → do
          quest_state_remaining_content_ .= rest_content
          quest_state_fames_ .= c
          walkSecond maybe_makeDeedFromTime
        (NarrativeContent _:_) → pure (NextIsNarrative, maybe_makeDeedFromTime)
        (EventContent _:_) → pure (NextIsEvent, maybe_makeDeedFromTime)

  ((content, new_marks, (next_is, maybe_makeDeedFromTime)), new_quest_state) ←
    use quest_state_
    >>=
    runStateT walkFirst

  quest_state_ .= new_quest_state

  marks_ .= new_marks

  display_next_button ←
    case next_is of
      NextIsStuck → pure False
      NextIsNarrative → pure True
      NextIsEvent → marksArePresent

  case maybe_makeDeedFromTime of
    Nothing → pure ()
    Just makeDeedFromTime → do
      when ← getCurrentTimeAsLocalTime
      deeds_ %= (makeDeedFromTime when:)

  pure (content, display_next_button)

handleApi ∷ Environment → ScottyM ()
handleApi environment =
  Scotty.post "/api/run" <<< apiTransaction environment $
    runGame <&> (fst >>> markdownResult ok200)

handleWeb ∷ Environment → ScottyM ()
handleWeb environment = do
  Scotty.get "/run" <<< webTransaction environment $ action
  Scotty.post "/run" <<< webTransaction environment $ action
 where
  action =
    runGame
    <&>
    \(content, display_next_button) →
      renderTopOnlyPageResult "Habit of Fate - Event" (\_ → ["story"]) [] Nothing ok200 $ \_ → do
        H.div ! A.class_ "story" $ renderMarkdownToHtml content
        if display_next_button
        then
          H.form
            ! A.action "/run"
            ! A.method "post"
            $ H.input
              ! A.type_ "submit"
              ! A.value "Next"
        else
          H.a
            ! A.href "/"
            $ H.toHtml ("Done" ∷ Text)

handler ∷ Environment → ScottyM ()
handler environment = do
  handleApi environment
  handleWeb environment
