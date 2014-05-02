{-# LANGUAGE OverloadedStrings #-}
-- | Main entry point to the application.
module Main where

import Textql.Options
import Textql.Sqlite
import Data.Maybe

import Database.Sqlite

import      System.Environment

import      Data.Text (Text, pack, unpack)


-- | The main entry point.
main :: IO ()
main = do
    options <- getArgs >>= programOptions
    let flags = fst options
    print options
    connection <- open ":memory:"
    let tableName = getTableName flags
        columnNames = getColumnNames flags "col1, col2, col3, kolona"
    maybeTypes <-  getTypes flags "1, 1, something, 1.1"
    let types = catMaybes maybeTypes
    print maybeTypes
    print types
    print tableName
    print columnNames
    withConnection connection $ pack $ schemaQuery tableName (map unpack columnNames) types
    close connection
