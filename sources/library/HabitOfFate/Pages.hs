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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Pages where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(throwM))
import qualified Data.Text.Lazy as Lazy

import HabitOfFate.Data.Markdown
import HabitOfFate.Story
import HabitOfFate.Substitution

data Page = Page
  { _title_ ∷ Text
  , _content_ ∷ Markdown
  , _choices_ ∷ PageChoices
  } deriving (Show)
makeLenses ''Page

choices_page_ids_ ∷ Traversal' Page Text
choices_page_ids_ = choices_ . linked_page_ids_

type Pages = [(Text, Page)]
type PageMap = HashMap Text Page

data PageTree = PageGroup Text [PageTree] | PageItem Text Text Story PageChoices

buildPages ∷ MonadThrow m ⇒ Substitutions → PageTree → m Pages
buildPages subs = go ""
 where
  go ∷ MonadThrow m ⇒ Text → PageTree → m Pages
  go prefix (PageItem id title content choices) = do
    substituted_content ← substitute subs content
    pure [(prependPrefix id,
           Page title substituted_content (choices & linked_page_ids_ %~ prependPrefix)
          )]
   where
    prependPrefix ∷ Text → Text
    prependPrefix "" = prefix
    prependPrefix x = case uncons x of
      Just ('-', rest) → rest
      _ → prefix ⊕ "-" ⊕ x
  go prefix (PageGroup name children) =
    mapM (go new_prefix) children <&> concat
   where
    new_prefix
      | onull prefix = name
      | onull name   = prefix
      | otherwise    = prefix ⊕ "-" ⊕ name

data OverlappingPageIds = OverlappingPageIds [(Text,Int)] deriving (Eq,Show)
instance Exception OverlappingPageIds

constructPageMap ∷ MonadThrow m ⇒ Pages → m PageMap
constructPageMap pages = do
  let number_of_pages = length pages
      pagecounts ∷ HashMap Text Int
      pagecounts =
        foldl'
          (flip $ \(page_id, _) → alterMap (maybe 1 (+1) >>> Just) page_id)
          mempty
          pages
  if olength pagecounts /= number_of_pages
    then pagecounts |> mapToList |> filter (snd >>> (> 1)) |> OverlappingPageIds |> throwM
    else pure $ mapFromList pages

data MissingPageIds = MissingPageIds [Text] deriving (Eq,Show)
instance Exception MissingPageIds

validatePageIds ∷ MonadThrow m ⇒ PageMap → m ()
validatePageIds pagemap = do
  let missing_page_ids ∷ [Text]
      missing_page_ids =
        pagemap
          |> mapToList
          |> concatMap (^.. _2 . choices_ . linked_page_ids_)
          |> filter (flip notMember pagemap)
          |> nub
          |> sort
  unless (onull missing_page_ids) $
    throwM $ MissingPageIds missing_page_ids

data CircularReferences = CircularReferences [Text] deriving (Eq,Show)
instance Exception CircularReferences

checkForCircularReferences ∷ MonadThrow m ⇒ Pages → m ()
checkForCircularReferences pages = do
  let circular_references ∷ [Text]
      circular_references =
        pages
          |> mapMaybe (
              \(page_id, page) →
                if elemOf choices_page_ids_ page_id page
                  then Just page_id
                  else Nothing
             )
          |> nub
  unless (onull circular_references) $
    throwM $ CircularReferences circular_references

constructAndValidatePageMap ∷ MonadThrow m ⇒ Pages → m PageMap
constructAndValidatePageMap pages = do
  checkForCircularReferences pages
  pagemap ← constructPageMap pages
  validatePageIds pagemap
  pure $ pagemap

walkPages ∷ Text → PageMap → Pages
walkPages root pagemap
  | root `notMember` pagemap = error $ unpack $ [i|internal error: root page "#{root}"|]
  | otherwise = go [root] mempty
 where
  go ∷ Seq Text → HashSet Text → Pages
  go queue visited = case uncons queue of
    Nothing → []
    Just (page_id, rest)
      | page_id `member` visited → go rest visited
      | otherwise →
          maybe
            (error $ unpack [i|internal error: "#{page_id}" is missing, violating a contract|])
            (\page → (page_id, page):go
              (foldlOf' choices_page_ids_ snoc rest page)
              (insertSet page_id visited)
            )
            (lookup page_id pagemap)

index_pages ∷ IO Pages
index_pages =
  pure
  $
  singleton
    ( "index"
    , (Page
        "The Adventure Begins"
        (((unlines ∷ [Text] → Text) >>> Markdown)
          ["Men, women, searchers, wanderers, people who just want to get home,"
          ,"all of them send their prayers to you in the hope you will hear"
          ,"them and grant them aid."
          ,""
          ,"But will you?  You are a God, after all, and these mortals can make"
          ,"such fun playthings."
          ]
        )
        (Choices "The choice is yours.  Where would you like to start?" $
          [ ("A prayer from someone searching for an herb in the Wicked Forest.", "forest")
          ]
        )
      )
    )
