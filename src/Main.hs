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

import  qualified       Data.Text as T
import  qualified       Data.Text.IO as TIO
import      Data.List

-- | The main entry point.
main :: IO ()
main = do
    options <- getArgs >>= programOptions

    let flags = fst options

    let inputFileArg = getSource flags
    inputFile <- openFile (T.unpack inputFileArg) ReadMode

    let firstRead = TIO.hGetLine inputFile
    firstLine <- firstRead

    let tableName = getTableName flags
        columnNames = getColumnNames flags firstLine

    let secondRead = if (hasHeaderFlag flags) then (TIO.hGetLine inputFile) else return firstLine
    secondLine <- secondRead

    maybeTypes <-  getTypes flags secondLine
    let types = catMaybes maybeTypes
        d = getDelimiter flags

    -- ready for inserting data
    connection <- open ":memory:"
    -- create schema
    schemaCreationResponse <- withConnection connection (schemaQuery tableName columnNames types)
    print schemaCreationResponse
    -- first insert
    firstRowInsertResponse <- withConnection connection (insertQuery tableName $ T.words $ clean firstLine d)
    print firstRowInsertResponse
    -- other inserts
    contents <- TIO.hGetContents inputFile
    let ls = T.lines contents
        cleanLines = map (T.words . (flip clean) d) ls
        batchInsert = batchInsertQuery tableName cleanLines
    -- withConnection connection $ pack $ concat $ intersperse "" insertQueries
    batchInsertResponse <- withConnection connection batchInsert
    print batchInsertResponse
    -- do one select...
    countResponse <- withConnection connection $ T.concat ["SELECT count(*) from ", tableName, ";"]
    print countResponse
    -- close resources
    close connection
    hClose inputFile
