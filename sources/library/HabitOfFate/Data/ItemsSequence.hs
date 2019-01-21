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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Data.ItemsSequence where

import HabitOfFate.Prelude

import Control.DeepSeq (NFData(..))
import Control.Exception
import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Sequence (deleteAt, elemIndexL, insertAt)
import Data.UUID (UUID)
import qualified GHC.Exts as Exts

import HabitOfFate.TH

data ItemsSequence α = ItemsSequence
  { _items_map_ ∷ !(HashMap UUID α)
  , _items_seq_ ∷ !(Seq UUID)
  } deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)
deriveJSON ''ItemsSequence
makeLenses ''ItemsSequence

instance NFData α ⇒ NFData (ItemsSequence α) where
  rnf (ItemsSequence m s) = rnf m `seq` rnf s `seq` ()

itemsContainKey ∷ UUID → ItemsSequence α → Bool
itemsContainKey k (ItemsSequence m _) = member k m

items_count_ ∷ Getter (ItemsSequence α) Int
items_count_ = items_seq_ . to length

items_list_ ∷ Show α ⇒ Lens' (ItemsSequence α) [(UUID, α)]
items_list_ =
  lens
    (\items@(ItemsSequence items_map items_seq) →
      [ let error_message =
              "IdSequence id " ⊕ show item_id ⊕ " was in the sequence but not in the map (items = " ⊕ show items ⊕ ")"
        in (item_id, fromMaybe (error error_message) (lookup item_id items_map))
      | item_id ← toList items_seq
      ]
    )
    (\items → itemsFromList)

items_values_ ∷ Show α ⇒ Getter (ItemsSequence α) [α]
items_values_ = to ((^. items_list_) >>> map snd)

instance Default (ItemsSequence α) where
  def = ItemsSequence mempty mempty

type instance Index (ItemsSequence α) = UUID
type instance IxValue (ItemsSequence α) = α

instance Ixed (ItemsSequence α) where
  ix item_id = items_map_ . ix item_id

instance At (ItemsSequence α) where
  at uuid f items@(ItemsSequence items_map items_seq) =
    f maybe_old_habit
    <&>
    \case
      Nothing
        | isJust maybe_old_habit →
            ItemsSequence (deleteMap uuid items_map) (delete uuid items_seq)
        | otherwise → items
      Just new_habit →
        let new_items_map = insertMap uuid new_habit items_map
            new_items_seq -- only append to the sequence if it doesn't exist
              | isNothing maybe_old_habit = items_seq `snoc` uuid
              | otherwise = items_seq
        in ItemsSequence new_items_map new_items_seq
    where
      maybe_old_habit = lookup uuid items_map
  {-# INLINE at #-}

data ReorderException =
    MismatchedLength
  | IdPositionMismatch
  | DuplicateIds
  | MissingId
  | MissingIndex
  deriving (Eq,Read,Show,Ord)

instance Exception ReorderException where

assertValidIdSequence ∷ MonadThrow m ⇒ ItemsSequence α → m (ItemsSequence α)
assertValidIdSequence items@(ItemsSequence items_map items_seq) = do
  let h_id_set = items_seq |> toList |> setFromList ∷ HashSet UUID
  unless (length h_id_set == length items_seq) $ throwM DuplicateIds
  forM_ items_seq $ \item_id →
    unless (member item_id items_map) $ throwM MissingId
  pure items

reorderById ∷ MonadThrow m ⇒ [UUID] → ItemsSequence α → m (ItemsSequence α)
reorderById new_items_seq_ (ItemsSequence items_map old_items_seq) = do
  let number_of_old_ids = length old_items_seq
      number_of_new_ids = length new_items_seq_
  when (number_of_old_ids /= number_of_new_ids) $ throwM MismatchedLength
  assertValidIdSequence $ ItemsSequence items_map (fromList new_items_seq_)

reorderByIndex ∷ MonadThrow m ⇒ [Int] → ItemsSequence α → m (ItemsSequence α)
reorderByIndex item_indices items@(ItemsSequence _ items_seq) =
  forM item_indices (\item_index → maybe (throwM MissingIndex) pure (items_seq ^? ix item_index))
  >>=
  flip reorderById items

_moveWithIdFromToIndex ∷ MonadThrow m ⇒ UUID → Int → Int → ItemsSequence α → m (ItemsSequence α)
_moveWithIdFromToIndex uuid old_index new_index items@(ItemsSequence items_map old_items_seq) = do
  unless (old_items_seq ^? ix old_index == Just uuid) $ throwM IdPositionMismatch
  when (new_index < 0 || new_index >= items ^. items_count_) $ throwM MissingIndex
  assertValidIdSequence $
    if old_index < new_index
      then
        old_items_seq
        |> deleteAt old_index
        |> insertAt new_index uuid
        |> ItemsSequence items_map
      else
        old_items_seq
        |> insertAt new_index uuid
        |> deleteAt (old_index+1)
        |> ItemsSequence items_map

moveWithIdToIndex ∷ MonadThrow m ⇒ UUID → Int → ItemsSequence α → m (ItemsSequence α)
moveWithIdToIndex uuid new_index items@(ItemsSequence _ items_seq) =
  maybe
    (throwM MissingId)
    (\old_index → _moveWithIdFromToIndex uuid old_index new_index items)
    (elemIndexL uuid items_seq)

moveWithIndexToIndex ∷ MonadThrow m ⇒ Int → Int → ItemsSequence α → m (ItemsSequence α)
moveWithIndexToIndex old_index new_index items@(ItemsSequence _ items_seq) =
  maybe
    (throwM MissingIndex)
    (\uuid → _moveWithIdFromToIndex uuid old_index new_index items)
    (items_seq ^? ix old_index)

itemsFromList ∷ [(UUID, α)] → ItemsSequence α
itemsFromList items = ItemsSequence (mapFromList items) (fromList $ map fst items)

instance Show α ⇒ Exts.IsList (ItemsSequence α) where
  type Item (ItemsSequence α) = (UUID, α)
  fromList = itemsFromList
  toList = (^. items_list_)
