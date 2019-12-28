{-# LANGUAGE OverloadedStrings #-}
module Textql.Sqlite where

-- we need this to avoid qualified Data.Text import
import           Prelude          hiding (concat)

import           Data.Text        (Text, concat, pack, replace)

import           Database.Persist hiding (replace)
import           Database.Sqlite

import           Textql.Types
import           Textql.Utils

type TableName = Text
type ColumnName = Text

createTablePrefix :: Text -> Text
createTablePrefix tableName = joinWithSpace ["CREATE TABLE", tableName]

insertPrefix :: Text -> Text
insertPrefix tableName = joinWithSpace ["INSERT INTO", tableName]

-- Make an Sqlite column definition. Used for constructing
-- a create schema query.
columnDef :: (ColumnName, Type) -> Text
columnDef (colName, colType) = joinWithSpace [colName, pack $ show colType]

schemaQuery :: TableName -> [ColumnName] -> [Type] -> Text
schemaQuery tableName columns types = concat [createTable, columnsDef]
    where createTable = createTablePrefix tableName
          columnsDef = propperValues $ joinWithColon columnDefs
          columnDefs = map columnDef columnsAndTypes
          columnsAndTypes = zip columns types

insertQuery :: TableName -> [Text] -> Text
insertQuery tableName values = joinWithSpace [insertPrefix tableName, "VALUES", values']
    where values' = (propperValues . joinWithColon) quotedValues
          quotedValues = map (quote . replace "'" "''") values

withConnection :: Connection -> Text -> IO [PersistValue]
withConnection connection query = do
    statement   <-  prepare connection query
    result      <-  step statement
    columns     <-  columns statement
    _           <-  finalize statement
    return columns
