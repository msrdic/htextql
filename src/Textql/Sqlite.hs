module Textql.Sqlite where

import      Data.Maybe
import      Data.List
import      Data.Text (Text, pack, unpack)

import      Database.Sqlite
import      Database.Persist

import      Control.Exception

-- once we have the command line parsing code
-- we can implement some kind of smart data typing
-- see http://www.sqlite.org/datatype3.html for sqlite3 types
data Type = IntegerT | RealT | TextT deriving Eq

instance Show Type where
    show IntegerT   = "INTEGER"
    show RealT      = "REAL"
    show TextT      = "TEXT"

-- a function for deducing a type of a column
-- try parsing a value as an Integer, then as a Real
-- if both fail, declare it a Text
deduceType :: String -> IO (Maybe Type)
deduceType value = do
    parses <- mapM ($ value) [tryInteger, tryReal, tryText]
    return $ head $ dropWhile isNothing parses

-- TODO replace String with Data.Bytestring.Lazy
-- TODO user Data.Bytestring.Lazy.Char8.readInt instead of readIO (?)
tryInteger :: String -> IO (Maybe Type)
tryInteger s = do
    result  <-  try (readIO s :: IO Integer) :: IO (Either IOException Integer)
    return $ case result of
                Left _  ->  Nothing
                Right _ ->  Just IntegerT

tryReal :: String -> IO (Maybe Type)
tryReal s = do
    result  <-  try (readIO s :: IO Double) :: IO (Either IOException Double)
    return $ case result of
                Left _  ->  Nothing
                Right _ ->  Just RealT

tryText :: String -> IO (Maybe Type)
tryText value = return $ Just TextT

-- let's write some simple database code
type TableName = String
type ColumnName = String

schemaQuery :: TableName -> [ColumnName] -> [Type] -> String
schemaQuery tableName columns types = createTable ++ fieldDefs
    where   createTable = "CREATE TABLE " ++ tableName
            fieldDefs = propperValues (intercalate ", " $ map columnDef $ zip columns types)
            columnDef :: (ColumnName, Type) -> String
            columnDef (colName, colType) = colName ++ " " ++ show colType

insertQuery :: TableName -> [String] -> String
insertQuery tableName values = "INSERT INTO " ++ tableName ++ " VALUES " ++ values'
    where values' = propperValues $ intercalate ", " (map quote values)

batchInsertQuery :: TableName -> [[String]] -> String
batchInsertQuery tableName values = "INSERT INTO " ++ tableName ++ " VALUES " ++ values' ++ ";"
    where values'  = intercalate ", " valueStrings
          valueStrings = map generateValueString values

-- take a list of record values and construct a single
-- values part of an insert statement
generateValueString :: [String] -> String
generateValueString = batchValues . (intercalate ", ") . (map quote)

quote string = "'" ++ string ++ "'"
propperValues string = (batchValues string) ++ ";"
batchValues string = "(" ++ string ++ ")"

withConnection :: Connection -> Text -> IO [PersistValue]
withConnection connection query = do
    print query
    statement   <-  prepare connection query
    result      <-  step statement
    columns     <-  columns statement
    _           <-  finalize statement
    return columns