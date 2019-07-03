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

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.Deeds where

import HabitOfFate.Prelude

import Network.HTTP.Types.Status (ok200)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Data.Deed
import HabitOfFate.Data.Markdown
import HabitOfFate.Data.SuccessOrFailureResult
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

handler ∷ Environment → ScottyM ()
handler environment = do
  Scotty.get "/deeds" $ webTransaction environment $ do
    deeds ← use deeds_
    pure $ renderPageResult "Deeds" ["deeds"] [] Nothing ok200 $ do
      H.div ! A.class_ "logo" $ H.img ! A.src "/images/logo.svgz" ! A.width "100%"
      H.hr
      H.div ! A.class_ "return" $ H.a ! A.href "/habits" $ H.toHtml ("Return to Habits" ∷ Text)
      H.hr
      H.div ! A.class_ "deeds" $ forM_ deeds $ \(Deed result text when) → do
        let result_class = case result of
              SuccessResult → "treasure"
              FailureResult → "grave"
        H.div ! A.class_ ("result " ⊕ result_class) $ mempty
        H.div ! A.class_ ("text " ⊕ result_class) $ renderMarkdownToHtml text
        H.div ! A.class_ ("when " ⊕ result_class) $ H.toHtml $ renderLocalTime when
 where
  treasure = H.img ! A.src "/images/treasure-chest.svgz" ! A.width "100px"
  grave = H.img ! A.src "/images/grave.svgz" ! A.width "100px"
