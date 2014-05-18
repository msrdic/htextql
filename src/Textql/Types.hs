module Textql.Types where

import          Data.Maybe
import          Control.Exception

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