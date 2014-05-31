module Textql.Sqlite where

-- we need this to avoid qualified Data.Text
import              Prelude hiding (concat)

import              Data.Maybe
import              Data.Text (Text, intercalate, concat, pack)

import              Database.Sqlite
import              Database.Persist

import              Textql.Types

type TableName = Text
type ColumnName = Text

joinWithSpace :: [Text] -> Text
joinWithSpace = intercalate " "

joinWithColon :: [Text] -> Text
joinWithColon = intercalate ", "

schemaQuery :: TableName -> [ColumnName] -> [Type] -> Text
schemaQuery tableName columns types = concat [createTable, fieldDefs]
    where createTable = createTablePrefix tableName
          fieldDefs = propperValues (joinWithColon $ map columnDef $ zip columns types)

columnDef :: (ColumnName, Type) -> Text
columnDef (colName, colType) = joinWithSpace [colName, pack $ show colType]

createTablePrefix :: Text -> Text
createTablePrefix tableName = joinWithSpace ["CREATE TABLE", tableName]

insertQuery :: TableName -> [Text] -> Text
insertQuery tableName values = joinWithSpace ["INSERT INTO", tableName, "VALUES", values']
    where values' = propperValues $ joinWithColon (map quote values)

batchInsertQuery :: TableName -> [[Text]] -> Text
batchInsertQuery tableName values = joinWithSpace ["INSERT INTO", tableName, "VALUES", values', ";"]
    where values'  = joinWithColon valueStrings
          valueStrings = map generateValueString values

-- take a list of record values and construct a single
-- values part of an insert statement
generateValueString :: [Text] -> Text
generateValueString = batchValues . joinWithColon . (map quote)

quote string = concat ["'", string, "'"]
propperValues values = concat [batchValues values, ";"]
batchValues values = concat ["(", values, ")"]

withConnection :: Connection -> Text -> IO [PersistValue]
withConnection connection query = do
    statement   <-  prepare connection query
    result      <-  step statement
    columns     <-  columns statement
    _           <-  finalize statement
    return columns