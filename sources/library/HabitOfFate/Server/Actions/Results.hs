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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Actions.Results where

import HabitOfFate.Prelude

import Data.Aeson (ToJSON)
import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (Status(..))
import Network.Wai (rawPathInfo, rawQueryString, requestMethod)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, finish, status)
import qualified Web.Scotty as Scotty

import HabitOfFate.Logging (logIO)
import HabitOfFate.Story (Event)
import HabitOfFate.Story.Renderer.HTML (renderEventToHTML)

finishWithStatus ∷ Status → ActionM α
finishWithStatus s = do
  logIO $ "Finished with status: " ⊕ show s
  status s
  finish

finishWithStatusMessage ∷ Int → String → ActionM α
finishWithStatusMessage code = pack >>> encodeUtf8 >>> Status code >>> finishWithStatus

setContent ∷ Content → ActionM ()
setContent NoContent = pure ()
setContent (TextContent t) = Scotty.text t
setContent (TextContentAsHTML t) = Scotty.html t
setContent (HtmlContent h) = h |> toHtml |> renderHtml |> Scotty.html
setContent (JSONContent j) = Scotty.json j

setStatusAndLog ∷ Status → ActionM ()
setStatusAndLog status_@(Status code message) = do
  status status_
  let result
        | code < 200 || code >= 300 = "failed"
        | otherwise = "succeeded"
  logIO $ [i|Request #{result} - #{code} #{decodeUtf8 >>> unpack $ message}|]

data ProgramResult = ProgramRedirectsTo Lazy.Text | ProgramResult Status Content

data Content =
    NoContent
  | TextContent Lazy.Text
  | TextContentAsHTML Lazy.Text
  | HtmlContent Html
  | ∀ α. ToJSON α ⇒ JSONContent α

returnNothing ∷ Monad m ⇒ Status → m ProgramResult
returnNothing s = return $ ProgramResult s NoContent

returnLazyText ∷ Monad m ⇒ Status → Lazy.Text → m ProgramResult
returnLazyText s = TextContent >>> ProgramResult s >>> return

returnLazyTextAsHTML ∷ Monad m ⇒ Status → Lazy.Text → m ProgramResult
returnLazyTextAsHTML s = TextContentAsHTML >>> ProgramResult s >>> return

returnJSON ∷ (ToJSON α, Monad m) ⇒ Status → α → m ProgramResult
returnJSON s = JSONContent >>> ProgramResult s >>> return

redirectTo ∷ Monad m ⇒ Lazy.Text → m ProgramResult
redirectTo = ProgramRedirectsTo >>> return

renderHTMLUsingTemplate ∷ Text → [Text] → Html → Lazy.Text
renderHTMLUsingTemplate title stylesheets body =
  renderHtml $
    H.docTypeHtml $ do
      H.head $
        (H.title $ toHtml title)
        ⊕
        mconcat
          [ H.link
              ! A.rel "stylesheet"
              ! A.type_ "text/css"
              ! A.href (H.toValue $ mconcat ["css/", stylesheet, ".css"])
          | stylesheet ← stylesheets
          ]
      H.body $ do
        H.img ! A.src "images/logo.svgz"
        body

renderHTMLUsingTemplateAndReturn ∷ Monad m ⇒ Text → [Text] → Status → Html → m ProgramResult
renderHTMLUsingTemplateAndReturn title stylesheets status =
  renderHTMLUsingTemplate title stylesheets
  >>>
  returnLazyTextAsHTML status

renderEventToHTMLAndReturn ∷ Monad m ⇒ Text → [Text] → Status → Event → m ProgramResult
renderEventToHTMLAndReturn title stylesheets status =
  renderEventToHTML
  >>>
  renderHTMLUsingTemplateAndReturn title stylesheets status

logRequest ∷ ActionM ()
logRequest = do
  r ← Scotty.request
  logIO [i|URL requested: #{requestMethod r} #{rawPathInfo r}#{rawQueryString r}|]
