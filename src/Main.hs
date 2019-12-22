{-# LANGUAGE OverloadedStrings #-}
-- | Main entry point to the application.
module Main where

import              Textql.Options
import              Textql.Sqlite
import              Textql.Utils
import              Data.Maybe

import              Database.Sqlite

import              System.Environment
import              System.IO

import  qualified   Data.Text as T
import  qualified   Data.Text.IO as TIO
import              Data.List

import System.Log.FastLogger

-- | The main entry point.
main :: IO ()
main = do
    options <- getArgs >>= programOptions
    logger <- newStdoutLoggerSet 1024

    let flags = fst options
    let inputFileArg = getSource flags
    inputFile <- openFile (T.unpack inputFileArg) ReadMode

    let firstRead = TIO.hGetLine inputFile
    firstLine <- firstRead

    pushLogStrLn logger (toLogStr $ T.concat ["first line: ", firstLine])

    let tableName = getTableName flags
        columnNames = getColumnNames flags firstLine

    pushLogStrLn logger (toLogStr $ T.concat ["table name: ", tableName])
    pushLogStrLn logger (toLogStr $ T.concat $ "column names: ": columnNames)

    let secondRead = if (hasHeaderFlag flags) then (TIO.hGetLine inputFile) else return firstLine
    secondLine <- secondRead

    maybeTypes <-  getTypes flags secondLine
    let types = catMaybes maybeTypes
        d = getDelimiter flags

    -- ready for inserting data
    connection <- open ":memory:"
    -- create schema
    let createSchemaQuery = schemaQuery tableName columnNames types
    pushLogStrLn logger (toLogStr $ T.concat ["create schema query: ", createSchemaQuery])
    schemaCreationResponse <- withConnection connection createSchemaQuery
    
    -- we already read first and/or second line
    -- so we should insert that one which contains content
    let firstLineValues = insertQuery tableName $ T.words $ clean firstLine d
    pushLogStrLn logger (toLogStr $ T.concat ["first line insert: ", firstLineValues])
    firstRowInsertResponse <- withConnection connection firstLineValues

    -- other values
    contents <- TIO.hGetContents inputFile
    let ls = T.lines contents
        cleanLines = map (T.words . (flip clean) d) ls
        batchInsert = batchInsertQuery tableName cleanLines
    pushLogStrLn logger (toLogStr $ T.concat ["batch insert: ", batchInsert])

    batchInsertResponse <- withConnection connection batchInsert
    -- print countResponse
    queryResponse <- withConnection connection $ getQuery flags
    print queryResponse
    -- close resources
    close connection
    hClose inputFile
