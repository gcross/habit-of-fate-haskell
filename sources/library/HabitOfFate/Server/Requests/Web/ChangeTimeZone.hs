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
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.ChangeTimeZone (handler) where

import HabitOfFate.Prelude

import Data.Time.Zones.All (toTZName)
import Network.HTTP.Types.Status (badRequest400, ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

handleChangeTimeZoneGet ∷ Environment → ScottyM ()
handleChangeTimeZoneGet environment = do
  Scotty.get "/timezone" <<< webTransaction environment $ do
    timezone ← use timezone_
    renderTopOnlyPageResult "Habit of Fate - Changing the Time Zone" [] [] ok200 >>> pure $
      H.form ! A.method "post" $ do
        H.div ! A.class_ "fields" $ do
          H.div $ H.toHtml ("Time Zone:" ∷ Text)
          H.select
            ! A.name "timezone"
            ! A.required "true"
            $ flip foldMap [minBound..maxBound] $ \timezone' →
                let option_prefix_1 = H.option ! A.value (H.toValue $ show timezone')
                    option_prefix_2
                      | timezone == timezone' =
                          option_prefix_1 ! A.selected "selected"
                      | otherwise =
                          option_prefix_1
                in option_prefix_2 $ H.toHtml (toTZName timezone' |> decodeUtf8 ∷ Text)
          H.input ! A.type_ "submit"
          H.a ! A.href "/" $ H.toHtml ("Cancel" ∷ Text)

handleChangeTimeZonePost ∷ Environment → ScottyM ()
handleChangeTimeZonePost environment = do
  Scotty.post "/timezone" <<< webTransaction environment $ do
    maybe_timezone_label ← getParamMaybe "timezone"
    case maybe_timezone_label of
      Nothing → do
        renderPageResult "Bad Time Zone Change" [] [] badRequest400 >>> pure $ do
          H.body $ do
            H.h1 $ H.toHtml ("Missing Time Zone" ∷ Text)
            H.p $ H.toHtml ("Timezone field was not present." ∷ Text)
      Just timezone_label →
        case readEither timezone_label of
          Left error_message → do
            renderPageResult "Bad Time Zone Change" [] [] badRequest400 >>> pure $ do
              H.body $ do
                H.h1 $ H.toHtml ("Missing Time Zone" ∷ Text)
                H.p $ H.toHtml ("Timezone field value was not recognized: " ⊕ error_message)
          Right timezone → do
            timezone_ .= timezone
            pure $ redirectsToResult temporaryRedirect307 "/"

handler ∷ Environment → ScottyM ()
handler environment = do
  handleChangeTimeZoneGet environment
  handleChangeTimeZonePost environment
