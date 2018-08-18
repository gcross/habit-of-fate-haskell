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
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, finish, status)
import qualified Web.Scotty as Scotty

import HabitOfFate.Logging (logIO)

finishWithStatus ∷ Status → ActionM α
finishWithStatus s = do
  logIO $ "Finished with status: " ⊕ show s
  status s
  finish

finishWithStatusMessage ∷ Int → String → ActionM α
finishWithStatusMessage code = pack >>> encodeUtf8 >>> Status code >>> finishWithStatus

data Content =
    NoContent
  | TextContent Lazy.Text
  | TextContentAsHTML Lazy.Text
  | HtmlContent Html
  | ∀ α. ToJSON α ⇒ JSONContent α

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
