{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.RunGame (handler) where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Markdown
import HabitOfFate.Server.Common
import HabitOfFate.Server.Requests.Shared.RunGame
import HabitOfFate.Server.Transaction

handler ∷ Environment → ScottyM ()
handler environment = do
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
