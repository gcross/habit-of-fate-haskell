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

import HabitOfFate.Story
import HabitOfFate.Substitution

data GenPage α = Page
  { _page_title_ ∷ Text
  , _page_content_ ∷ α
  , _page_choices_ ∷ PageChoices
  } deriving (Foldable,Functor,Eq,Ord,Read,Show,Traversable)
makeLenses ''GenPage
type GenPages α = [(Text, GenPage α)]

page_choice_linked_ids_∷ Traversal' (GenPage α) Text
page_choice_linked_ids_ = page_choices_ . linked_page_ids_

type StoryPage = GenPage Story
type StoryPages = GenPages Story
type StoryPageMap = HashMap Text StoryPage
type Page = GenPage Lazy.Text
type Pages = GenPages Lazy.Text
type PageMap = HashMap Text Page

all_page_contents_ ∷ Traversal (GenPages α) (GenPages β) α β
all_page_contents_ f = traverse $ \(page_id, page) → (page_id,) <$> traverse f page

all_page_ids_ ∷ Traversal' (GenPages α) Text
all_page_ids_ f = traverse $ \(page_id, page) → liftA2 (,) (f page_id) (page_choice_linked_ids_ f page)

buildPages ∷ MonadThrow m ⇒ Substitutions → StoryPages → m Pages
buildPages subs = mapM $ \(page_id, page) → (page_id,) <$> traverse (substituteM subs) page

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
          |> concatMap (^.. _2 . page_choices_ . linked_page_ids_)
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
                if elemOf (page_choices_ . linked_page_ids_) page_id page
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
              (foldlOf' (page_choices_ . linked_page_ids_) snoc rest page)
              (insertSet page_id visited)
            )
            (lookup page_id pagemap)
