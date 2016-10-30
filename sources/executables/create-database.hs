{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad (forM_, when)
import Database.SQLite.Simple (execute_, withConnection)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main = do
    [database_filename] ← getArgs
    file_exists ← doesFileExist database_filename
    when file_exists $ error "That file already exists."
    withConnection database_filename $
        forM_
            ["CREATE TABLE gods (id TEXT PRIMARY KEY, name TEXT UNIQUE)"
            ]
        .
        execute_
