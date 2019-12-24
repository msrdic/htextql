{-# LANGUAGE OverloadedStrings #-}
module Textql.Options where

import           Prelude               hiding (words, tail, reverse, head)

import           System.Console.GetOpt

import           Data.List             (find)
import           Data.Maybe
import           Data.Text             (Text, append, concat, pack, replace,
                                        toLower, unpack, words, tail, reverse, head)

import           Textql.Sqlite
import           Textql.Types
import           Textql.Utils

-- | define all usable flags
data Flag = Delimiter String
          | Header String
          | Source String
          | Query String
          | TableName String
          deriving Show

-- if the TableName command-line option isn't supplied,
-- use this as table name
defaultTableName :: Text
defaultTableName = "tbl"

-- command-line options
-- these are just converted to Haskell syntax
-- from [here](https://github.com/dinedal/textql)
-- intentionally skipped some of these
options :: [OptDescr Flag]
options =
    [
        Option ['d'] ["delimiter"]
               (ReqArg Delimiter ",")
               "Delimiter between fields -dlm tab for tab, -dlm 0x## to specify a character code in hex",

        Option ['h'] ["header"]
               (ReqArg Header "false")
               "Treat file as having the first row as a header row",

        Option ['s'] ["source"]
               (ReqArg Source "stdin")
               "Source file to load, or defaults to stdin",

        Option ['q'] ["query"]
               (ReqArg Query "")
               "SQL Command(s) to run on the data",

        Option ['t'] ["table"]
               (ReqArg TableName $ unpack defaultTableName)
               "Override the default table name (tbl)"
    ]

-- parse program command-line options
-- for more details, look [here.](http://hackage.haskell.org/package/base-4.7.0.0/docs/System-Console-GetOpt.html)
programOptions :: [String] -> IO ([Flag], [String])
programOptions argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ constructUserError errs
      where constructUserError errs = Prelude.concat errs ++ usageInfo header options
            header = "Usage: textql [OPTION...]"


isTableNameFlag (TableName _) = True
isTableNameFlag _             = False

isHeaderFlag (Header _) = True
isHeaderFlag _          = False

isSourceFlag (Source _) = True
isSourceFlag _          = False

isDelimiterFlag (Delimiter _) = True
isDelimiterFlag _             = False

isQueryFlag (Query _) = True
isQueryFlag _         = False

getDelimiter :: [Flag] -> Text
getDelimiter flags = case (find isDelimiterFlag flags) of
        Nothing                -> ","
        Just (Delimiter delim) -> pack delim

hasHeaderFlag :: [Flag] -> Bool
hasHeaderFlag flags = case (find isHeaderFlag flags) of
        Nothing         -> False
        Just (Header b) -> if b == "true" then True else False

getSource :: [Flag] -> Text
getSource flags = case (find isSourceFlag flags) of
        Nothing                  -> "stdin"
        Just (Source sourceFile) -> pack sourceFile

getTableName :: [Flag] -> Text
getTableName flags = case (find isTableNameFlag flags) of
        Nothing                    -> defaultTableName
        Just (TableName tableName) -> pack tableName

getQuery :: [Flag] -> Text
getQuery flags = case (find isQueryFlag flags) of
        Nothing        -> ""
        Just (Query q) -> pack q

-- | getColumnNames returns a list of column names that should
-- be used for constructing a database table. If the first line
-- of the input file has no header (supplied via the command line
-- argument), then a default list of column names is constructed
-- and returned (column_1, column_2 etc.). If the first line contains
-- header information, then that line is cleaned up and parsed for
-- column names.
getColumnNames :: [Flag] -> Text -> [Text]
getColumnNames flags firstLine = case firstLineIsHeader of
    False -> standardColumnNames $ (length . words) firstLine
    True  -> uniqueColumnNames $ words $ toLower $ clean firstLine delimiter
    where firstLineIsHeader = hasHeaderFlag flags
          delimiter = getDelimiter flags

-- | Make standard column names.
standardColumnNames :: Int -> [Text]
standardColumnNames count = zipWith (append) (replicate count "column_") $ columnOrds
    where columnOrds = map (pack . show) [1 .. count]

uniqueColumnNames :: [Text] -> [Text]
uniqueColumnNames []   = []
uniqueColumnNames cols = uniqueColumnNames' 1 cols

uniqueColumnNames' :: Int -> [Text] -> [Text]
uniqueColumnNames' _ [] = []
uniqueColumnNames' i (c:cs)  = let duplicate = find (==c) cs in
                             case duplicate of
                               Just _ -> (uniqueColumnName c i) : uniqueColumnNames' (i + 1) cs
                               Nothing -> c:uniqueColumnNames' i cs

uniqueColumnName :: Text -> Int -> Text
uniqueColumnName "" i = Data.Text.concat ["", pack "_", pack $ show i]
uniqueColumnName col i = if charIsQuote (head col)
                              then uniqueColumnNameQuoted col i
                              else uniqueColumnNameUnquoted col i
charIsQuote :: Char -> Bool
charIsQuote '\"' = True
charIsQuote '\'' = True
charIsQuote _ = False

uniqueColumnNameQuoted colName i = quote $ suffixWith i $ unqoute colName
uniqueColumnNameUnquoted colName i = suffixWith i colName
unqoute = tail . reverse . tail . reverse
suffixWith i colName = Data.Text.concat [colName, "_", pack $ show i]

-- | Deduce types of values from a sample line.
getTypes :: [Flag] -> Text -> IO [Maybe Type]
getTypes flags firstLine = mapM deduceType $ textValues
    where textValues = map unpack values
          values = words $ clean firstLine delimiter
          delimiter = getDelimiter flags
