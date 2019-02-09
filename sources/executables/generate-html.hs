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

import HabitOfFate.Prelude hiding ((<.>))

import Data.Text.Lazy.IO (writeFile)
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.IO (putStrLn)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HabitOfFate.Data.Markdown
import HabitOfFate.Pages
import qualified HabitOfFate.Quests.Forest.Pages as Forest
import HabitOfFate.Story

import Paths_habit_of_fate

all_pages ∷ [IO Pages]
all_pages =
  [ index_pages
  , Forest.pages
  ]

addSuffix ∷ Text → Text
addSuffix x = x ⊕ case unsnoc x of
  Nothing → "index.html"
  Just (_, '/') → "index.html"
  _ → ".html"

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
  pages ← sequence all_pages <&> concat
  pagemap ← constructAndValidatePageMap pages
  createDirectoryIfMissing True output_path
  forM_ (walkPages "index" pagemap) $ \(page_id, page) → do
    let output_filepath = output_path </> unpack page_id <.> "html"
    putStrLn [i|Writing story file #{output_filepath}|]
    writeFile output_filepath $ renderHtml $ H.docTypeHtml $ do
      H.head $ do
        H.title $ toHtml (page ^. title_)
        H.link ! A.href "https://fonts.googleapis.com/css?family=Gloria+Hallelujah" ! A.rel "stylesheet"
        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href (H.toValue $ ("style.css" ∷ Text))
      H.body $ do
        H.div ! A.class_ "logo" $ do
          H.div ! A.class_ "logo_entry" $ H.img ! A.src "treasure-chest.png" ! A.width "100px"
          H.div ! A.class_ "logo_entry" $ H.a ! A.href "index.html" $ H.img ! A.src "logo.svg" ! A.width "300px"
          H.div ! A.class_ "logo_entry" $ H.img ! A.src "grave.svg" ! A.width "100px"
        let showBackToChoices =
              H.ul $ forM_ (page ^. groups_) $ \GoBackTo{..} →
                H.li $ H.a ! A.href (H.toValue $ group_id ⊕ ".html") $ H.toHtml group_name
        H.div $ do
          H.h1 $ toHtml (page ^. title_)
          renderMarkdownToHtml $ page ^. content_
          case page ^. choices_ of
            DeadEnd → do
              H.b $ H.toHtml ("You have reached the end.  Go back to..." ∷ Text)
              showBackToChoices
            NoChoice c_ref → do
              H.a ! A.href (H.toValue $ c_ref ⊕ ".html") $ H.toHtml ("Continue." ∷ Text)
              H.br >> H.hr >> H.br >> H.toHtml ("Or, go back to..." ∷ Text)
              showBackToChoices
            Choices question choices → do
              H.h2 $ H.toHtml question
              H.ul $ forM_ choices $ \(c, c_ref) → H.li $ H.a ! A.href (H.toValue $ c_ref ⊕ ".html") $ H.toHtml c
              unless (page_id == "index") $ do
                H.br >> H.hr >> H.br >> H.toHtml ("Or, go back to..." ∷ Text)
                showBackToChoices
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
