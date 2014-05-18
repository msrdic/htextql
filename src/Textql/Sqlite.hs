module Textql.Sqlite where

import      Data.Maybe
import      Data.List
import  qualified   Data.Text as T

import      Database.Sqlite
import      Database.Persist

import      Textql.Types

-- let's write some simple database code
type TableName = T.Text
type ColumnName = T.Text

schemaQuery :: TableName -> [ColumnName] -> [Type] -> T.Text
schemaQuery tableName columns types = T.concat [createTable, fieldDefs]
    where   createTable = createTablePrefix tableName
            fieldDefs = propperValues (T.intercalate ", " $ map columnDef $ zip columns types)

columnDef :: (ColumnName, Type) -> T.Text
columnDef (colName, colType) = T.concat [colName, " ", T.pack $ show colType]

createTablePrefix :: T.Text -> T.Text
createTablePrefix tableName = T.concat ["CREATE TABLE ", tableName]

insertQuery :: TableName -> [T.Text] -> T.Text
insertQuery tableName values = T.concat ["INSERT INTO ", tableName, " VALUES ", values']
    where values' = propperValues $ T.intercalate ", " (map quote values)

batchInsertQuery :: TableName -> [[T.Text]] -> T.Text
batchInsertQuery tableName values = T.concat ["INSERT INTO ", tableName, " VALUES ", values', ";"]
    where values'  = T.intercalate ", " valueStrings
          valueStrings = map generateValueString values

-- take a list of record values and construct a single
-- values part of an insert statement
generateValueString :: [T.Text] -> T.Text
generateValueString = batchValues . (T.intercalate ", ") . (map quote)

quote string = T.concat ["'", string, "'"]
propperValues string = T.concat [(batchValues string), ";"]
batchValues string = T.concat ["(", string, ")"]

withConnection :: Connection -> T.Text -> IO [PersistValue]
withConnection connection query = do
    print query
    statement   <-  prepare connection query
    result      <-  step statement
    columns     <-  columns statement
    _           <-  finalize statement
    return columns