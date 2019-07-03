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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Actions.Results where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, displayException, evaluate)
import Data.Aeson (ToJSON)
import qualified Data.Text.Lazy as Lazy
import Network.HTTP.Types.Status (Status(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, toHtml)
import Web.Scotty (ActionM, finish, redirect, status)
import qualified Web.Scotty as Scotty

import HabitOfFate.Logging (logIO)
import HabitOfFate.Server.Common

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
  | HtmlContent (Device → Html)
  | ∀ α. (NFData α, ToJSON α) ⇒ JSONContent α

setContent ∷ Device → Content → ActionM ()
setContent device = \case
  NoContent → pure ()
  TextContent t → check t >>= Scotty.text
  HtmlContent h → check (h device |> toHtml |> renderHtml) >>= Scotty.html
  JSONContent j → check j >>= Scotty.json
 where
  check ∷ NFData α ⇒ α → ActionM α
  check x =
    liftIO
      (
        (Right <$> (x |> force |> evaluate))
        `catch`
        (\(e ∷ SomeException) → pure $ Left e)
      )
    >>=
    \case
      Right result → pure result
      Left exc → do
        logIO "ERROR RENDERING PAGE CONTENT:"
        logIO $ pack $ displayException exc
        Scotty.raise "A server error occurred when rendering the page content.  Please try reloading the page."

setStatusAndLog ∷ Status → ActionM ()
setStatusAndLog status_@(Status code message) = do
  status status_
  let result
        | code < 200 || code >= 300 = "failed"
        | otherwise = "succeeded"
  logIO [i|Request #{result} - #{code} #{decodeUtf8 >>> unpack $ message}|]

setStatusAndRedirect ∷ Status → Lazy.Text → ActionM α
setStatusAndRedirect status_ url = do
  logIO [i|Redirecting to #{url} with code #{status_}|]
  status status_
  redirect url
