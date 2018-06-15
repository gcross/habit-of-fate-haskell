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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Habit where

import HabitOfFate.Prelude

import Control.Monad.Catch

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Sequence (deleteAt, elemIndexL, insertAt)
import qualified Data.Text.Lazy as Lazy
import Data.Typeable
import Data.UUID

import Text.Blaze (Markup, ToMarkup(..))

import Web.Scotty (Parsable(..))

import HabitOfFate.TH

data Scale = VeryLow | Low | Medium | High | VeryHigh
  deriving (Bounded,Enum,Eq,Ord,Read,Show)
deriveJSON ''Scale

instance Parsable Scale where
  parseParam p =
    p
    |> unpack
    |> readMaybe
    |> maybe (Left $ "Unrecognized scale \"" ⊕ p ⊕ "\".") Right

instance ToMarkup Scale where
  toMarkup = displayScale >>> toMarkup

showScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ String → α → String
showScale name =
  (^. _Wrapped')
  >>>
  show
  >>>
  (⊕ name)

toMarkupScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ Text → α → Markup
toMarkupScale name =
  (^. _Wrapped')
  >>>
  displayScale
  >>>
  (\scale_str → scale_str ⊕ " " ⊕ name)
  >>>
  toMarkup

readPrecsScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ String → Int → String → [(α, String)]
readPrecsScale name p =
  readsPrec p
  >>>
  \case
    [(scale, rest)]
      | rest == name → [(scale ^. _Unwrapped', "")]
      | otherwise → []
    _ → []

parseParamScale ∷ (Wrapped α, Unwrapped α ~ Scale) ⇒ Lazy.Text → Lazy.Text → Either Lazy.Text α
parseParamScale name p =
  case words p of
    [scale_text, name_]
      | name_ == name →
          parseParam scale_text
          |> bimap
              (\error_message →
                 "Error parsing scale \"" ⊕ scale_text ⊕ "\" in \"" ⊕ p ⊕ "\": " ⊕ error_message)
              (^. _Unwrapped')
      | otherwise → Left $ "Second word in \"%" ⊕ p ⊕ "\" was not " ⊕ name
    _ → Left $ "Wrong number of words in \"" ⊕ p ⊕ "\""

newtype Difficulty = Difficulty { unwrapDifficulty ∷ Scale }
  deriving (Bounded,Enum,Eq,FromJSON,Ord,ToJSON)

instance Wrapped Difficulty where
  type Unwrapped Difficulty = Scale
  _Wrapped' = iso unwrapDifficulty Difficulty

instance Show Difficulty where
  show = showScale "Difficulty"

instance ToMarkup Difficulty where
  toMarkup = toMarkupScale "Difficulty"

instance Read Difficulty where
  readsPrec = readPrecsScale "Difficulty"

instance Parsable Difficulty where
  parseParam = parseParamScale "Difficulty"

newtype Importance = Importance { unwrapImportance ∷ Scale }
  deriving (Bounded,Enum,Eq,FromJSON,Ord,ToJSON)

instance Wrapped Importance where
  type Unwrapped Importance = Scale
  _Wrapped' = iso unwrapImportance Importance

instance Show Importance where
  show = showScale "Importance"

instance ToMarkup Importance where
  toMarkup = toMarkupScale "Importance"

instance Read Importance where
  readsPrec = readPrecsScale "Importance"

instance Parsable Importance where
  parseParam = parseParamScale "Importance"

scales ∷ [Scale]
scales = enumFromTo minBound maxBound

displayScale ∷ Scale → Text
displayScale VeryLow = "Very Low"
displayScale Low = "Low"
displayScale Medium = "Medium"
displayScale High = "High"
displayScale VeryHigh = "Very High"

scaleFactor ∷ Scale → Double
scaleFactor VeryLow = 1/4
scaleFactor Low = 1/2
scaleFactor Medium = 1
scaleFactor High = 2
scaleFactor VeryHigh = 4

data Habit = Habit
  { _name_ ∷ Text
  , _difficulty_ ∷ Difficulty
  , _importance_ ∷ Importance
  } deriving (Eq,Ord,Read,Show)
deriveJSON ''Habit

name ∷ Lens' Habit Text
name = lens _name_ (\old new_name → old { _name_ = new_name })

difficulty ∷ Lens' Habit Scale
difficulty =
  lens
    (_difficulty_ >>> unwrapDifficulty)
    (\old new_scale → old { _difficulty_ = Difficulty new_scale })

importance ∷ Lens' Habit Scale
importance =
  lens
    (_importance_ >>> unwrapImportance)
    (\old new_scale → old { _importance_ = Importance new_scale })

instance Default Habit where
  def = Habit "" (Difficulty Medium) (Importance Medium)

data Habits = Habits
  { _habit_map_ ∷ HashMap UUID Habit
  , _habit_id_seq_ ∷ Seq UUID
  } deriving (Eq,Read,Show,Ord)
deriveJSON ''Habits
makeLenses ''Habits

habit_count ∷ Getter Habits Int
habit_count = habit_id_seq_ . to length

habit_list ∷ Getter Habits [(UUID, Habit)]
habit_list = to $ \habits@(Habits h_map h_id_seq) →
  [ let error_message =
          "Habit id " ⊕ show habit_id ⊕ " was in the sequence but not in the map (habits = " ⊕ show habits ⊕ ")"
    in (habit_id, fromMaybe (error error_message) (lookup habit_id h_map))
  | habit_id ← toList h_id_seq
  ]

instance Default Habits where
  def = Habits mempty mempty

type instance Index Habits = UUID
type instance IxValue Habits = Habit

instance Ixed Habits where
  ix habit_id = habit_map_ . ix habit_id

instance At Habits where
  at habit_id f habits@(Habits h_map h_id_seq) =
    f maybe_old_habit
    <&>
    \case
      Nothing
        | isJust maybe_old_habit →
            Habits (deleteMap habit_id h_map) (delete habit_id h_id_seq)
        | otherwise → habits
      Just new_habit →
        let new_h_map = insertMap habit_id new_habit h_map
            new_h_id_seq -- only append to the sequence if it doesn't exist
              | isNothing maybe_old_habit = h_id_seq `snoc` habit_id
              | otherwise = h_id_seq
        in Habits new_h_map new_h_id_seq
    where
      maybe_old_habit = lookup habit_id h_map
  {-# INLINE at #-}

data ReorderException =
    MismatchedLength
  | IdPositionMismatch
  | DuplicateIds
  | MissingId
  | MissingIndex
  deriving (Eq,Read,Show,Ord,Typeable)

instance Exception ReorderException where

assertValidUUIDSequence ∷ MonadThrow m ⇒ Habits → m Habits
assertValidUUIDSequence habits@(Habits h_map h_id_seq) = do
  let h_id_set = h_id_seq |> toList |> setFromList ∷ HashSet UUID
  unless (length h_id_set == length h_id_seq) $ throwM DuplicateIds
  forM_ h_id_seq $ \habit_id →
    unless (member habit_id h_map) $ throwM MissingId
  pure habits

reorderHabitsByUUID ∷ MonadThrow m ⇒ [UUID] → Habits → m Habits
reorderHabitsByUUID new_h_id_seq (Habits h_map old_h_id_seq) = do
  let number_of_old_ids = length old_h_id_seq
      number_of_new_ids = length new_h_id_seq
  when (number_of_old_ids /= number_of_new_ids) $ throwM MismatchedLength
  assertValidUUIDSequence $ Habits h_map (fromList new_h_id_seq)

reorderHabitsByIndex ∷ MonadThrow m ⇒ [Int] → Habits → m Habits
reorderHabitsByIndex indices habits@(Habits _ h_id_seq) =
  forM indices (\index → maybe (throwM MissingIndex) pure (h_id_seq ^? ix index))
  >>=
  flip reorderHabitsByUUID habits

_moveHabitWithIdFromToIndex ∷ MonadThrow m ⇒ UUID → Int → Int → Habits → m Habits
_moveHabitWithIdFromToIndex habit_id old_index new_index habits@(Habits h_map old_h_id_seq) = do
  unless (old_h_id_seq ^? ix old_index == Just habit_id) $ throwM IdPositionMismatch
  when (new_index < 0 || new_index >= habits ^. habit_count) $ throwM MissingIndex
  assertValidUUIDSequence $
    if old_index < new_index
      then
        old_h_id_seq
        |> deleteAt old_index
        |> insertAt new_index habit_id
        |> Habits h_map
      else
        old_h_id_seq
        |> insertAt new_index habit_id
        |> deleteAt (old_index+1)
        |> Habits h_map

moveHabitWithIdToIndex ∷ MonadThrow m ⇒ UUID → Int → Habits → m Habits
moveHabitWithIdToIndex habit_id new_index habits@(Habits _ h_id_seq) =
  maybe
    (throwM MissingId)
    (\old_index → _moveHabitWithIdFromToIndex habit_id old_index new_index habits)
    (elemIndexL habit_id h_id_seq)

moveHabitWithIndexToIndex ∷ MonadThrow m ⇒ Int → Int → Habits → m Habits
moveHabitWithIndexToIndex old_index new_index habits@(Habits _ h_id_seq) =
  maybe
    (throwM MissingIndex)
    (\habit_id → _moveHabitWithIdFromToIndex habit_id old_index new_index habits)
    (h_id_seq ^? ix old_index)
