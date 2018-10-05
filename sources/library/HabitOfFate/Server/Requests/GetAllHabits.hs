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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.GetAllHabits (handleGetAllHabits) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Habit
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction.Common
import HabitOfFate.Server.Transaction.Reader
import HabitOfFate.Story.Renderer.HTML

handleGetAllHabitsApi ∷ Environment → ScottyM ()
handleGetAllHabitsApi environment = do
  Scotty.get "/api/habits" <<< apiReader environment $ do
    log "Requested all habits."
    view habits_ >>= returnJSON ok200

handleGetAllHabitsWeb ∷ Environment → ScottyM ()
handleGetAllHabitsWeb environment = do
  Scotty.get "/habits" <<< webReader environment $ do
    habit_list ← view (habits_ . habit_list_)
    quest_status ← ask <&> getAccountStatus
    renderPageAndReturn "Habit of Fate - List of Habits" ["list"] ok200 $ do
      generateTopHTML $ H.div ! A.class_ "story" $ renderEventToHTML quest_status
      H.div ! A.class_ "list" $ do
        H.table $ do
          H.thead $ foldMap (H.toHtml >>> H.th) [""∷Text, "#", "Name", "Difficulty", "Importance", "I succeeded!", "I failed."]
          H.tbody <<< mconcat $
            [ H.tr ! A.class_ ("row " ⊕ if n `mod` 2 == 0 then "even" else "odd") $ do
                H.td $ H.form ! A.method "post" ! A.action (H.toValue $ "/move/" ⊕ show uuid) $ do
                  H.input
                    ! A.type_ "submit"
                    ! A.value "Move To"
                  H.input
                    ! A.type_ "text"
                    ! A.value (H.toValue n)
                    ! A.name "new_index"
                    ! A.class_ "new-index"
                H.td $ H.toHtml (show n ⊕ ".")
                H.td ! A.class_ "name" $
                  H.a ! A.href (H.toValue ("/habits/" ⊕ pack (show uuid) ∷ Text)) $ H.toHtml (habit ^. name_)
                let addScaleElement scale_class scale_lens =
                      H.td ! A.class_ scale_class $ H.toHtml $ displayScale $ habit ^. scale_lens
                addScaleElement "difficulty" difficulty_
                addScaleElement "importance" importance_
                let addMarkElement name class_ =
                      H.td $
                        H.form ! A.method "post" ! A.action (H.toValue $ "/mark/" ⊕ name ⊕ "/" ⊕ show uuid) $
                          H.input ! A.type_ "submit" ! A.class_ ("smiley " ⊕ class_) ! A.value ""
                addMarkElement "success" "good"
                addMarkElement "failure" "bad"
            | n ← [1∷Int ..]
            | (uuid, habit) ← habit_list
            ]
        H.a ! A.class_ "new_link" ! A.href "/habits/new" $ H.toHtml ("New" ∷ Text)
        H.a ! A.class_ "logout_link" ! A.href "/logout" $ H.toHtml ("Logout" ∷ Text)

handleGetAllHabits ∷ Environment → ScottyM ()
handleGetAllHabits environment = do
  handleGetAllHabitsApi environment
  handleGetAllHabitsWeb environment
