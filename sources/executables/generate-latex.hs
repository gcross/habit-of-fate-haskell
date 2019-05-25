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

import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Lazy.IO (writeFile)
import Options.Applicative
import System.Directory
import System.FilePath
import Text.RawString.QQ (r)

import HabitOfFate.Data.Markdown
import HabitOfFate.Story
import HabitOfFate.StoryLine
import HabitOfFate.StoryLineQuests

import Paths_habit_of_fate

standard_prelude, standard_postlude ∷ Builder
standard_prelude = [r|
\documentclass[letterpaper]{book}

\usepackage{textcomp}
\usepackage{hyperref}

\begin{document}
|]

standard_postlude = [r|
\end{document}
|]

generatePageContent ∷ Page → Builder
generatePageContent Page{..} = fromString
  [i|\\section{#{renderMarkdownToLaTeX page_title}} \n\\label{#{page_path}}\n#{renderMarkdownToLaTeX page_content}\n|]

generatePageChoices ∷ Page → Builder
generatePageChoices Page{..} = case page_choices of
  NoChoice c_ref →
    fromString [i|Continue to page \\pageref{#{c_ref}}.\n|]
  Choices question choices →
    fromString [i|#{renderMarkdownToLaTeX question}\n\n\\begin{enumerate}\n|]
    ⊕
    mconcat
      [ fromString [i|\\item #{renderMarkdownToLaTeX c} (Go to page \\pageref{#{c_ref}}.)|]
      | (c, c_ref) ← choices
      ]
    ⊕
    fromString "\\end{enumerate}\n\n"

data Configuration = Configuration
  { output_path ∷ FilePath
  }

main ∷ IO ()
main = do
  let configuration_parser ∷ Parser Configuration
      configuration_parser = Configuration
        <$> (strOption $ mconcat
              [ metavar "FILE"
              , help "Path to the output file."
              , short 'o'
              , long "output"
              , action "file"
              ]
            )
  Configuration{..} ←
    execParser $ info
      (configuration_parser <**> helper)
      (fullDesc <> header "generate-latex -- generate story latex file"
      )
  pages ←
    (mapM substituteQuestWithStandardSubstitutions quests ∷ IO [Quest Markdown])
    <&>
    (concatMap buildPagesFromQuest >>> (index_page:))
  createDirectoryIfMissing True $ takeDirectory output_path
  toLazyText >>> writeFile output_path $
    standard_prelude
    ⊕
    foldl'
      (\chunks page →
        chunks ⊕ generatePageContent page ⊕ generatePageChoices page
      )
      mempty
      pages
    ⊕
    standard_postlude
