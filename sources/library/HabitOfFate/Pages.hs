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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Pages where

import HabitOfFate.Prelude

import Control.Monad.Catch (Exception, MonadThrow(throwM))

import HabitOfFate.Story

modifyAllPageIds ∷ (Text → Text) → [(Text, Page)] → [(Text, Page)]
modifyAllPageIds modify = map (modify *** (page_choices_ . linked_page_ids_ %~ modify))

data OverlappingPageIds = OverlappingPageIds [(Text,Int)] deriving (Eq,Show)
instance Exception OverlappingPageIds

constructPageMap ∷ MonadThrow m ⇒ [(Text,Page)] → m (HashMap Text Page)
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

validatePageIds ∷ MonadThrow m ⇒ HashMap Text Page → m ()
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

checkForCircularReferences ∷ MonadThrow m ⇒ [(Text,Page)] → m ()
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

constructAndValidatePageMap ∷ MonadThrow m ⇒ [(Text,Page)] → m (HashMap Text Page)
constructAndValidatePageMap pages = do
  checkForCircularReferences pages
  pagemap ← constructPageMap pages
  validatePageIds pagemap
  pure $ pagemap

walkPages ∷ Text → HashMap Text Page → [(Text,Page)]
walkPages root pagemap = go [root] mempty
 where
  go ∷ Seq Text → HashSet Text → [(Text,Page)]
  go queue visited = case uncons queue of
    Nothing → []
    Just (page_id, rest)
      | page_id `member` visited → go rest visited
      | otherwise →
          maybe
            (error $ unpack $ "internal error: " ⊕ page_id ⊕ " is missing, violating a contract")
            (\page → (page_id, page):go
              (foldlOf' (page_choices_ . linked_page_ids_) snoc rest page)
              (insertSet page_id visited)
            )
            (lookup page_id pagemap)
