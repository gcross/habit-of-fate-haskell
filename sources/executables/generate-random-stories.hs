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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import HabitOfFate.Prelude hiding ((<.>))

import qualified Data.Text as T
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
import HabitOfFate.Story
import HabitOfFate.Substitution

import qualified HabitOfFate.Quests.Forest as Forest

import Paths_habit_of_fate

random_stories ∷ [(Text, Quest, [(Text, [(Text, Story)])])]
random_stories =
  [("Forest", Forest.quest,
    [("Wander Stories", zip Forest.wander_stories_labels Forest.wander_stories)
    ])
  ]

data Configuration = Configuration
  { output_filepath ∷ FilePath
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
      (fullDesc <> header "generate-random-stories -- generate random stories html file"
      )
  substituted_random_stories ∷ [(Text, [(Text, [(Text, Markdown)])])] ← traverse
    (\(name, quest, stories) →
      (name,) <$> traverse (traverseOf (_2 . traversed . _2) (substitute (defaultQuestSubstitutions quest))) stories
    )
    random_stories
  putStrLn [i|Writing file #{output_filepath}|]
  createDirectoryIfMissing True $ takeDirectory output_filepath
  writeFile output_filepath $ renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml ("Random Stories" ∷ Text)
    H.body $ do
      H.h1 $ H.toHtml ("Table of Contents" ∷ Text)
      let makeId quest_name story_group_name story_name =
            [quest_name, story_group_name, story_name]
              |> map (T.words >>> mconcat)
              |> T.intercalate "-"
              |> H.toValue

      H.ul $ forM_ substituted_random_stories $ \(quest_name, quest_random_stories) → do
        H.li $ H.toHtml quest_name
        H.ul $ forM_ quest_random_stories $ \(story_group_name, story_group) → do
          H.li $ H.toHtml story_group_name
          H.ul $ forM_ story_group $ \(story_name, _) → do
            H.li $ H.a ! A.href ("#" ⊕ makeId quest_name story_group_name story_name) $ H.toHtml story_name

      forM_ substituted_random_stories $ \(quest_name, quest_random_stories) → do
        H.h1 $ H.toHtml quest_name
        forM_ quest_random_stories $ \(story_group_name, story_group) → do
          H.h2 $ H.toHtml story_group_name
          forM_ story_group $ \(story_name, story) → do
            H.h3 ! A.id (makeId quest_name story_group_name story_name) $ H.toHtml story_name
            renderMarkdownToHtml story
