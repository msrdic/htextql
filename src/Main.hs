{-# LANGUAGE OverloadedStrings #-}
-- | Main entry point to the application.
module Main where

import           Data.Maybe
import           Textql.Options
import           Textql.Sqlite
import           Textql.Utils

import           Database.Sqlite

import           System.Environment
import           System.IO

import           Data.List
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           System.Log.FastLogger

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

    let tableName = getTableName flags
        columnNames = getColumnNames flags firstLine

    pushLogStrLn logger (toLogStr $ T.concat ["table name: ", tableName])
    pushLogStrLn logger (toLogStr $ T.concat ["column names: ", T.intercalate ", " columnNames])

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
    pushLogStrLn logger (toLogStr $ T.concat ["schema creation response: ", T.pack $ show schemaCreationResponse])

    -- we already read first and/or second line
    -- so we should insert that one which contains content
    let firstLineValues = insertQuery tableName $ T.words $ clean firstLine d
    firstRowInsertResponse <- withConnection connection firstLineValues

    -- other values
    contents <- TIO.hGetContents inputFile
    let ls = T.lines contents
        cleanLines = map (T.words . (flip clean) d) ls
        batchInsert = batchInsertQuery tableName cleanLines

    batchInsertResponse <- withConnection connection batchInsert
    -- print countResponse
    pushLogStrLn logger (toLogStr $ T.concat ["command line query: ", getQuery flags])
    queryResponse <- withConnection connection $ getQuery flags
    pushLogStrLn logger (toLogStr $ T.concat ["query response: ", T.pack $ show queryResponse])
    print queryResponse
    -- close resources
    close connection
    hClose inputFile
    
