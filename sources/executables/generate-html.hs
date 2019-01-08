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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude

import Data.Text.Lazy.IO (writeFile)
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), joinPath, splitPath, takeDirectory)
import System.IO (putStrLn)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HabitOfFate.Pages
import qualified HabitOfFate.Quests.Forest as Forest
import HabitOfFate.Story

import Paths_habit_of_fate

addSuffix ∷ Text → Text
addSuffix x = x ⊕ case unsnoc x of
  Nothing → "index.html"
  Just (_, '/') → "index.html"
  _ → ".html"

processPageIds ∷ Text → [(Text, Page)] → [(Text, Page)]
processPageIds prefix = modifyAllPageIds (\x → prefix_with_slash ⊕ addSuffix x)
 where
  prefix_with_slash = prefix ⊕ "/"

data Configuration = Configuration
  { output_path ∷ FilePath
  }

main ∷ IO ()
main = do
  let configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> (strOption $ mconcat
              [ metavar "DIRECTORY"
              , help "Path to the output directory."
              , short 'o'
              , long "output"
              , action "directory"
              ]
            )
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (fullDesc <> header "generate-html -- generate story html files"
      )
  pagemap ←
    [ singleton
      ("index.html", Page
        "The Adventure Begins"
        ""
        (Choices "Which quest shall we begin?" $
          map (second (⊕ "/index.html"))
          [ ("The search in the Wicked Forest.", "forest")
          ]
        )
      )
    , processPageIds "forest" Forest.pages
    ]
    |> concat
    |> constructAndValidatePageMap
  createDirectoryIfMissing True output_path
  forM_ (walkPages "index.html" pagemap) $ \(page_path_packed, page) → do
    let page_path = unpack page_path_packed
        resource_directory = page_path |> splitPath |> length |> subtract 1 |> flip replicate ".." |> joinPath
    createDirectoryIfMissing True (output_path </> takeDirectory page_path)
    putStrLn [i|Writing page #{page_path_packed}|]
    writeFile (output_path </> page_path) $ renderHtml $ H.docTypeHtml $ do
      H.head $ do
        H.title $ toHtml (page ^. page_title_)
        H.link ! A.href "https://fonts.googleapis.com/css?family=Gloria+Hallelujah" ! A.rel "stylesheet"
        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href (H.toValue $ resource_directory </> "css/style.css")
      H.body $ do
        H.span ! A.class_ "title" $ toHtml (page ^. page_title_)
        H.div $ H.preEscapedLazyText $ page ^. page_content_
        case page ^. page_choices_ of
          DeadEnd → H.b $ H.toHtml ("You have reached the end." ∷ Text)
          NoChoice c_ref → H.a ! A.href (H.toValue c_ref) $ H.toHtml ("Continue." ∷ Text)
          Choices question choices → do
            H.span ! A.class_ "question" $ H.toHtml question
            H.ul $ forM_ choices $ \(c, c_ref) → H.li $ H.a ! A.href (H.toValue c_ref) $ H.toHtml c
  forM_
    [ "css/style.css"
    , "images/logo.svgz"
    ] $ \auxiliary_filename → do
    putStrLn [i|Writing auxiliary file #{auxiliary_filename}|]
    let destination_path = output_path </> auxiliary_filename
    createDirectoryIfMissing True $ takeDirectory destination_path
    source_path ← getDataFileName $ "data" </> "static" </> auxiliary_filename
    copyFile source_path destination_path
