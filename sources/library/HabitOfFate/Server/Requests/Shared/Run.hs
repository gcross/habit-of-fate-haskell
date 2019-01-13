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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Shared.Run (handler) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (ok200, temporaryRedirect307)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Data.Account
import HabitOfFate.Server.Common
import HabitOfFate.Server.Transaction

runEvent ∷ TransactionProgram (Maybe Lazy.Text)
runEvent = do
  marks ← use marks_
  account ← get
  let (maybe_event, new_account) = runState runAccount account
  put new_account
  pure maybe_event

handleApi ∷ Environment → ScottyM ()
handleApi environment =
  Scotty.post "/api/run" <<< apiTransaction environment $
    runEvent <&> (fromMaybe "" >>> lazyTextResult ok200)

handleWeb ∷ Environment → ScottyM ()
handleWeb environment = do
  Scotty.get "/run" <<< webTransaction environment $ action
  Scotty.post "/run" <<< webTransaction environment $ action
 where
  action =
    runEvent
    >>=
    maybe
      (log "No marks.  Redirecting to /" >> (pure $ redirectsToResult temporaryRedirect307 "/"))
      (\event → do
        marks ← use marks_
        marks_are_present ← marksArePresent
        renderTopOnlyPageResult "Habit of Fate - Event" ["story"] [] Nothing ok200 >>> pure $ do
          H.div ! A.class_ "story" $ H.preEscapedLazyText event
          if marks_are_present
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
      )

handler ∷ Environment → ScottyM ()
handler environment = do
  handleApi environment
  handleWeb environment
