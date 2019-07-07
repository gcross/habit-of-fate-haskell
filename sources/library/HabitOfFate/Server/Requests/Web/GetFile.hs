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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Server.Requests.Web.GetFile (handler) where

import HabitOfFate.Prelude

import qualified Data.Text.Lazy as Lazy
import System.FilePath ((</>))
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as Scotty

import HabitOfFate.Logging

import Paths_habit_of_fate (getDataFileName)

handler ∷ ScottyM ()
handler = do
  Scotty.get "/css/:filename" $ fetch "css" "css" "text/css" Nothing
  Scotty.get "/fonts/:filename" $ fetch "fonts" "ttf" "application/font-ttf" Nothing
  Scotty.get "/images/:filename" $ fetch "images" "svgz" "image/svg+xml" (Just "gzip")
  Scotty.get "/js/:filename" $ fetch "js" "js" "text/javascript" Nothing
 where
  fetch ∷ FilePath → String → Lazy.Text → Maybe Lazy.Text → ActionM ()
  fetch subdirectory extension content_type maybe_compression = do
    filepath ← Scotty.param "filename"
    logIO [i|Requested file #{filepath} in #{subdirectory}|]
    when ('/' ∈ filepath) $ do
      logIO [i|Filepath #{filepath} has a slash.|]
      Scotty.next
    unless (('.':extension) `isSuffixOf` filepath) $ do
      logIO [i|Filename #{filepath} does not end with .#{extension}|]
      Scotty.next
    file_to_return ← (subdirectory </> filepath) |> getDataFileName |> liftIO
    logIO [i|Returning #{file_to_return}|]
    Scotty.addHeader "Content-Type" content_type
    maybe (pure ()) (Scotty.addHeader "Content-Encoding") maybe_compression
    Scotty.file file_to_return
