{-# LANGUAGE OverloadedStrings #-}
-- | Main entry point to the application.
module Main where

import Textql.Options
import Textql.Sqlite
import Textql.Utils
import Data.Maybe

import Database.Sqlite

import      System.Environment
import      System.IO

import      Data.Text (Text, pack, unpack, replace)
import      Data.List

-- | The main entry point.
main :: IO ()
main = do
    options <- getArgs >>= programOptions

    let flags = fst options

    let inputFileArg = getSource flags
    inputFile <- openFile inputFileArg ReadMode

    let firstRead = hGetLine inputFile
    firstLine <- firstRead

    let tableName = getTableName flags
        columnNames = getColumnNames flags $ pack firstLine

    let secondRead = if (hasHeaderFlag flags) then (hGetLine inputFile) else return firstLine
    secondLine <- secondRead

    maybeTypes <-  getTypes flags secondLine
    let types = catMaybes maybeTypes
        d = getDelimiter flags

    -- ready for inserting data
    connection <- open ":memory:"
    -- create schema
    schemaCreationResponse <- withConnection connection (pack $ schemaQuery tableName (map unpack columnNames) types)
    print schemaCreationResponse
    -- first insert
    firstRowInsertResponse <- withConnection connection (pack $ insertQuery tableName $ words $ clean firstLine d)
    print firstRowInsertResponse
    -- other inserts
    contents <- hGetContents inputFile
    let ls = lines contents
        cleanLines = map (words . (flip clean) d) ls
        batchInsert = batchInsertQuery tableName cleanLines
    -- withConnection connection $ pack $ concat $ intersperse "" insertQueries
    batchInsertResponse <- withConnection connection $ pack $ batchInsert
    print batchInsertResponse
    -- do one select...
    countResponse <- withConnection connection $ pack $ "SELECT count(*) from " ++ tableName ++ ";"
    print countResponse
    -- close resources
    close connection
    hClose inputFile
