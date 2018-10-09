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

module HabitOfFate.Server.Requests.Web.GetAllHabits (handler) where

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
import HabitOfFate.Server.Transaction
import HabitOfFate.Story.Renderer.HTML

handler ∷ Environment → ScottyM ()
handler environment = do
  Scotty.get "/" <<< webTransaction environment $ do
    habit_list ← use (habits_ . habit_list_)
    quest_status ← get <&> getAccountStatus
    renderPageResult "Habit of Fate - List of Habits" ["list"] ok200 >>> pure $ do
      generateTopHTML $ H.div ! A.class_ "story" $ renderEventToHTML quest_status
      H.div ! A.class_ "list" $ mconcat $
        map (\(class_, column) → H.div ! A.class_ ("header " ⊕ class_) $ H.toHtml column)
          [ ("mark_button", "I succeeded!")
          , ("mark_button", "I failed.")
          , ("position", "#")
          , ("name", "Name")
          , ("scale", "Difficulty")
          , ("scale", "Importance")
          , ("", ""∷Text)
          ]
        ⊕
        concat
          [ let evenodd = if n `mod` 2 == 0 then "even" else "odd"
                position = H.div ! A.class_ "position" $ H.toHtml (show n ⊕ ".")
                edit_link =
                  H.a
                    ! A.class_ "name"
                    ! A.href (H.toValue ("/edit/" ⊕ pack (show uuid) ∷ Text))
                    $ H.toHtml (habit ^. name_)
                scaleFor scale_lens =
                  H.div ! A.class_ "scale" $ H.toHtml $ displayScale $ habit ^. scale_lens
                markButtonFor name class_ scale_lens_
                  | habit ^. scale_lens_ == None = mempty
                  | otherwise =
                      H.form
                        ! A.class_ "mark_button"
                        ! A.method "post"
                        ! A.action (H.toValue $ "/mark/" ⊕ name ⊕ "/" ⊕ show uuid)
                        $ H.input ! A.type_ "submit" ! A.class_ ("smiley " ⊕ class_) ! A.value ""
                move_form =
                  H.form
                    ! A.class_ "move"
                    ! A.method "post"
                    ! A.action (H.toValue $ "/move/" ⊕ show uuid)
                    $ do
                      H.input
                        ! A.type_ "submit"
                        ! A.value "Move To"
                      H.input
                        ! A.type_ "text"
                        ! A.value (H.toValue n)
                        ! A.name "new_index"
                        ! A.class_ "new-index"
            in map (\contents → H.div ! A.class_ evenodd $ contents)
            [ markButtonFor "success" "good" difficulty_
            , markButtonFor "failure" "bad"  importance_
            , position
            , edit_link
            , scaleFor difficulty_
            , scaleFor importance_
            , move_form
            ]
          | n ← [1∷Int ..]
          | (uuid, habit) ← habit_list
          ]
        ⊕
        replicate 3 (H.div mempty)
        ⊕
        [ H.div ! A.class_ "new_link" $ H.a ! A.href "/new" $ H.toHtml ("New" ∷ Text)
        ]

      H.hr

      H.div $ H.a ! A.class_ "logout_link" ! A.href "/logout" $ H.toHtml ("Logout" ∷ Text)

      H.hr

      H.a ! A.href "/timezone" $ H.toHtml ("Change Time Zone" ∷ Text)
