module Textql.Options where

import System.Console.GetOpt
import Data.List
import qualified Data.Text as T
import Textql.Sqlite

-- | define all usable flags
data Flag = Delimiter String
          | Header String
          | Source String
          | Query String
          | TableName String
          deriving Show

defaultTableName = "tbl"

-- command-line options
-- these are just converted to Haskell syntax from here
-- https://github.com/dinedal/textql
-- intentionally skipped some of these
options :: [OptDescr Flag]
options =
    [
        Option ['d'] ["delimiter"]  (ReqArg Delimiter ",")              "Delimiter between fields -dlm tab for tab, -dlm 0x## to specify a character code in hex",
        Option ['h'] ["header"]     (ReqArg Header "false")             "Treat file as having the first row as a header row",
        Option ['s'] ["source"]     (ReqArg Source "stdin")             "Source file to load, or defaults to stdin",
        Option ['q'] ["query"]      (ReqArg Query "")                   "SQL Command(s) to run on the data",
        Option ['t'] ["table"]      (ReqArg TableName defaultTableName) "Override the default table name (tbl)"
    ]

programOptions :: [String] -> IO ([Flag], [String])
programOptions argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: textql [OPTION...]"


isTableNameFlag (TableName _) = True
isTableNameFlag _ = False

isHeaderFlag (Header _) = True
isHeaderFlag _ = False

hasHeaderFlag :: [Flag] -> Bool
hasHeaderFlag flags = case (find isHeaderFlag flags) of
        Nothing -> False
        Just (Header b) -> if b == "true" then True else False

getTableName :: [Flag] -> String
getTableName flags = case (find isTableNameFlag flags) of
        Nothing -> defaultTableName
        Just (TableName tableName) -> tableName

getColumnNames :: [Flag] -> T.Text -> [T.Text]
getColumnNames flags firstLine = case firstLineIsHeader of
    False -> standardColumnNames $ (length . T.words) firstLine
    True -> T.words $ T.replace separator "" firstLine
    where firstLineIsHeader = hasHeaderFlag flags
          separator = ","

standardColumnNames :: Int -> [T.Text]
standardColumnNames count = zipWith (T.append) (replicate count "column_") $ map (T.pack . show) [1 .. count]

getTypes :: [Flag] -> String -> IO [Maybe Type]
getTypes flags firstLine = mapM deduceType $ map T.unpack $ T.words $ T.replace separator "" $ T.pack firstLine
    where separator = ","