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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.GetAllHabits (handler) where

import HabitOfFate.Prelude

import Data.Time.Calendar (diffDays)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (localDay)
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Data.ItemsSequence
import HabitOfFate.Data.Scale
import HabitOfFate.Server.Common
import HabitOfFate.Server.Requests.Shared.GetQuestStatus
import HabitOfFate.Server.Transaction

redirectToDeadlinesIfNeeded ∷ Transaction TransactionResult → Transaction TransactionResult
redirectToDeadlinesIfNeeded action =
  any <$> (getCurrentTimeAsLocalTime <&> \current_time → (maybe False (<= current_time)))
      <*> (use $ habits_ . items_values_ . to (map getHabitDeadline))
  >>=
  bool action (pure $ redirectsToResult temporaryRedirect307 "/deadlines")

redirectToRunIfNeeded ∷ Transaction TransactionResult → Transaction TransactionResult
redirectToRunIfNeeded action =
  (use marks_ <&> (/= def))
  >>=
  bool action (pure $ redirectsToResult temporaryRedirect307 "/run")

redirectIfNeeded ∷ Transaction TransactionResult → Transaction TransactionResult
redirectIfNeeded = redirectToDeadlinesIfNeeded <<< redirectToRunIfNeeded

handler ∷ Environment → ScottyM ()
handler environment = do
  Scotty.get "/habits" <<< webTransaction environment $ redirectIfNeeded $ do
    groups ← use groups_
    maybe_group_id ← getParamMaybe "group"
    habit_list ←
      use (habits_ . items_list_)
      >>=
      \habits →
        case maybe_group_id of
          Nothing → pure habits
          Just group_id → do
            log [i|Filtering habits with group #{group_id}...|]
            pure $ filter (\(_, habit) → group_id `member` (habit ^. group_membership_)) habits
    quest_status ← getQuestStatus
    current_time_as_local_time ← getCurrentTimeAsLocalTime
    last_seen_as_local_time ← getLastSeenAsLocalTime
    renderPageResult "Habit of Fate - List of Habits" ["list"] [] Nothing ok200 >>> pure $ do
      generateTopHTML $ H.div ! A.class_ "story" $ H.preEscapedLazyText quest_status
      H.div ! A.class_ "groups" $ mconcat $
        map (\(class_, column) → H.div ! A.class_ ("header " ⊕ class_) $ H.toHtml column)
          [ ("position", "#")
          , ("name", "Group Name")
          , ("", ""∷Text)
          , ("", ""∷Text)
          , ("", ""∷Text)
          ]
        ⊕
        [ H.div ! A.class_ "position" $ H.toHtml ("0." ∷ Text)
        , H.div ! A.class_ "name_area" $
            (if maybe_group_id == Nothing
              then
                H.span ! A.class_ "selected_name"
              else
                H.a ! A.class_ "name" ! (A.href $ "/habits")
            ) $ H.toHtml ("All" ∷ Text)
        , H.div $ H.toHtml ("" ∷ Text)
        , H.div $ H.toHtml ("" ∷ Text)
        , H.div $ H.toHtml ("" ∷ Text)
        ]
        ⊕
        concat
          [ let evenodd = if n `mod` 2 == 0 then "even" else "odd"
                position = H.div ! A.class_ "position" $ H.toHtml (show n ⊕ ".")
                name_link =
                  (
                    H.div ! A.class_ "name_area"
                    >>>
                    if maybe_group_id == Just group_id
                      then
                        H.div ! A.class_ "name selected_name"
                      else
                        H.a ! A.class_ "name" ! (A.href $ "/habits?group=" ⊕ H.toValue (UUID.toText group_id))
                  ) $ H.toHtml group_name
                edit_button =
                  H.a
                    ! A.class_ "edit"
                    ! A.href (H.toValue [i|/groups/#{UUID.toText group_id}|])
                    $ H.img ! A.src "/images/edit.svgz" ! A.width "25px"
                move_form_id = H.toValue $ "move-group-" ⊕ show group_id
                move_button =
                  H.input
                    ! A.class_ "move_button"
                    ! A.type_ "submit"
                    ! A.value "Move"
                    ! A.form move_form_id
                move_input =
                  H.form
                    ! A.class_ "move_form"
                    ! A.id move_form_id
                    ! A.method "post"
                    ! A.action (H.toValue $ "/groups/" ⊕ show group_id ⊕ "/move")
                    $ do
                      H.input
                        ! A.type_ "text"
                        ! A.value (H.toValue n)
                        ! A.name "new_index"
                        ! A.class_ "move_input"
            in map (\contents → H.div ! A.class_ evenodd $ contents)
            [ position
            , name_link
            , edit_button
            , move_button
            , move_input
            ]
          | n ← [1∷Int ..]
          | (group_id, group_name) ← groups ^. items_list_
          ]
        ⊕
        replicate 3 (H.div mempty)
        ⊕
        [ H.div ! A.class_ "new_link new_group" $ H.a ! A.href "/groups/new" $ H.toHtml ("New" ∷ Text)
        ]

      H.div ! A.class_ "list" $ mconcat $
        map (\(class_, column) → H.div ! A.class_ ("header " ⊕ class_) $ H.toHtml column)
          [ ("mark_button", "I succeeded!")
          , ("mark_button", "I failed.")
          , ("position", "#")
          , ("name", "Habit Name")
          , ("", ""∷Text)
          , ("centered", "Last Marked")
          , ("centered", "Deadline")
          , ("centered", "Difficulty")
          , ("centered", "Importance")
          , ("", ""∷Text)
          , ("", ""∷Text)
          ]
        ⊕
        concat
          [ let evenodd = if n `mod` 2 == 0 then "even" else "odd"
                position = H.div ! A.class_ "position" $ H.toHtml (show n ⊕ ".")
                name = H.div ! A.class_ "name_area" $ H.div ! A.class_ "name" $ H.toHtml $ habit ^. name_
                edit_button =
                  H.a
                    ! A.class_ "edit"
                    ! A.href (H.toValue [i|/habits/#{UUID.toText uuid}|])
                    $ H.img ! A.src "/images/edit.svgz" ! A.width "25px"
                timeFor (text_if_nothing ∷ Text) =
                  ($ habit)
                  >>>
                  maybe
                    (H.toHtml text_if_nothing)
                    renderLocalTime
                  >>>
                  ((H.div ! A.class_ "centered") $)
                scaleFor scale_lens =
                  H.div ! A.class_ "centered" $ H.toHtml $ displayScale $ habit ^. scale_lens
                markButtonFor (habit_name ∷ Text) class_ scale_lens_
                  | habit ^. scale_lens_ == None = mempty
                  | otherwise =
                      H.form
                        ! A.class_ "mark_button"
                        ! A.method "post"
                        ! A.action (H.toValue [i|/habits/#{UUID.toText uuid}/mark/#{habit_name}|])
                        $ H.input ! A.type_ "submit" ! A.class_ ("smiley " ⊕ class_) ! A.value ""
                move_form_id = H.toValue $ "move-habit-" ⊕ show uuid
                move_button =
                  H.input
                    ! A.class_ "move_button"
                    ! A.type_ "submit"
                    ! A.value "Move"
                    ! A.form move_form_id
                move_input =
                  H.form
                    ! A.class_ "move_form"
                    ! A.id move_form_id
                    ! A.method "post"
                    ! A.action (H.toValue $ "/habits/" ⊕ show uuid ⊕ "/move")
                    $ do
                      H.input
                        ! A.type_ "text"
                        ! A.value (H.toValue n)
                        ! A.name "new_index"
                        ! A.class_ "move_input"
            in map (\contents → H.div ! A.class_ evenodd $ contents)
            [ markButtonFor "success" "good" difficulty_
            , markButtonFor "failure" "bad"  importance_
            , position
            , name
            , edit_button
            , timeFor "Never" (^. maybe_last_marked_)
            , timeFor "None" getHabitDeadline
            , scaleFor difficulty_
            , scaleFor importance_
            , move_button
            , move_input
            ]
          | n ← [1∷Int ..]
          | (uuid, habit) ← habit_list
          ]
        ⊕
        [ H.div ! A.class_ "new_link new_habit" $ H.a ! A.href "/habits/new" $ H.toHtml ("New" ∷ Text)
        ]

      H.hr

      H.div $ H.a ! A.class_ "logout_link" ! A.href "/logout" $ H.toHtml ("Logout" ∷ Text)

      H.hr

      H.hr

      let format_string =
            case localDay current_time_as_local_time `diffDays` localDay last_seen_as_local_time of
              0 → "Last seen Today at %l:%M%P.  "
              1 → "Last seen Yesterday at %l:%M%P.  "
              n | n < 365 → "Last seen on %b %e at %l:%M%P.  "
                | otherwise → "Last seen on %b %e, %Y, at %l:%M%P.  "
      H.toHtml $ formatTime defaultTimeLocale format_string last_seen_as_local_time

      H.a ! A.href "/timezone" $ H.toHtml ("(Change Time Zone)" ∷ Text)
