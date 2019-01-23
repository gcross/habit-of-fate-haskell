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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Logging where

import HabitOfFate.Prelude

import Data.MonoTraversable (Element, MonoFoldable)
import Data.Text.IO (hPutStrLn)
import Data.Time.Clock
import Data.Time.Format
import System.IO (stderr)
import qualified Data.Vector.Unboxed as UV

ologIO ∷ (MonoFoldable α, Element α ~ Char, MonadIO m) ⇒ α → m ()
ologIO message = liftIO $ do
  current_time ← getCurrentTime
  pack >>> hPutStrLn stderr $
    formatTime defaultTimeLocale "[%F %X] " current_time ⊕ (unpack message)

logIO ∷ MonadIO m ⇒ String → m ()
logIO message = ologIO message
