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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Accounts where

import HabitOfFate.Prelude

import Data.Aeson
import qualified Data.ByteString as BS
import System.Directory
import System.Environment
import System.FilePath

import HabitOfFate.Account
import HabitOfFate.Server

type Accounts = Map Username Account

readAccounts ∷ FilePath → IO Accounts
readAccounts = BS.readFile >=> either error return . decodeEither

writeAccounts ∷ FilePath → Accounts → IO ()
writeAccounts = encodeFile
