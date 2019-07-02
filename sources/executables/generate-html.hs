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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding ((<.>))

import Data.Text.Lazy.IO (writeFile)
import qualified Data.Text.Strict.Lens as TextLens
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory)
import System.IO (putStrLn)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HabitOfFate.Data.Markdown
import HabitOfFate.Quest
import HabitOfFate.Quest.Pages
import HabitOfFate.Quest.Pages.Index
import HabitOfFate.Quests

import Paths_habit_of_fate

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
  pages ← generateAllPages
  forM_ pages $ \Page{..} → do
    let path_to_root =
          unpack
          $
          foldlOf
            TextLens.text
            (flip $ \case
              '/' → (⊕ "../")
              _ → identity
            )
            ("" ∷ Text)
            page_path
        relativePath = (path_to_root ⊕) >>> H.toValue
        output_filepath = output_path </> unpack page_path <.> "html"
    putStrLn [i|Writing story file #{output_filepath}|]
    createDirectoryIfMissing True $ takeDirectory output_filepath
    writeFile output_filepath $ renderHtml $ H.docTypeHtml $ do
      H.head $ do
        H.title $ H.toHtml $ unwrapMarkdown page_title
        H.link ! A.href "https://fonts.googleapis.com/css?family=Gloria+Hallelujah" ! A.rel "stylesheet"
        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href (relativePath "style.css")
      H.body $ do
        H.div ! A.class_ "logo" $ do
          H.div ! A.class_ "logo_entry" $
            H.img ! A.src (relativePath "treasure-chest.png") ! A.width "100px"
          H.div ! A.class_ "logo_entry" $
            H.a ! A.href (relativePath "index.html") $ H.img ! A.src (relativePath "logo.svg") ! A.width "300px"
          H.div ! A.class_ "logo_entry"
            $ H.img ! A.src (relativePath "grave.svg") ! A.width "100px"
        H.div $ do
          H.h1 $ renderMarkdownToHtml page_title
          renderMarkdownToHtml $ page_content
          case page_choices of
            NoChoice c_ref → do
              H.a ! A.href (relativePath $ unpack c_ref ⊕ ".html") $ H.strong $ H.toHtml ("Continue." ∷ Text)
            Choices question choices → do
              H.h2 $ renderMarkdownToHtml question
              let non_escape_choices = takeWhile (\(c, _) → unwrapMarkdown c ^?! _head /= '[') choices
                  escape_choices = drop (length non_escape_choices) choices
                  outputChoices choices =
                    H.ul
                    $
                    forM_ choices
                    $
                    \(c, c_ref) →
                      H.li
                      $
                      H.a ! A.href (relativePath $ unpack c_ref ⊕ ".html")
                      $
                      renderMarkdownToHtmlWithoutParagraphTags c
              outputChoices non_escape_choices
              outputChoices escape_choices
  forM_
    [ ("css", "style.css")
    , ("images", "grave.svg")
    , ("images", "logo.svg")
    , ("images", "treasure-chest.png")
    ] $ \(auxiliary_directory, auxiliary_filename) → do
    putStrLn [i|Writing auxiliary file #{auxiliary_filename}|]
    let destination_path = output_path </> auxiliary_filename
    source_path ← getDataFileName $ "data" </> "static" </> auxiliary_directory </> auxiliary_filename
    copyFile source_path destination_path
