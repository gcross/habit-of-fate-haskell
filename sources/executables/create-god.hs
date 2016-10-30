{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Maybe (maybe)
import Data.UUID (UUID, fromText, toText)
import Database.SQLite.Simple (NamedParam((:=)), executeNamed, withConnection)
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.ToField (ToField(..))
import System.Environment (getArgs)
import System.Random (randomIO)

instance FromField UUID where
    fromField field =
        fromField field
        >>=
        (maybe (returnError ConversionFailed field "invalid UUID string") return) . fromText

instance ToField UUID where
    toField = toField . toText

main = do
    [database_filename, name] ← getArgs
    uuid :: UUID ← randomIO
    withConnection database_filename $ \connection →
        executeNamed connection
            "INSERT INTO gods (id, name) VALUES (:id, :name);"
            [":id" := uuid, ":name" := name]
