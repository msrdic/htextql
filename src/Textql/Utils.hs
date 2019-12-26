{-# LANGUAGE OverloadedStrings #-}
module Textql.Utils where

import           Data.Text (Text, concat, intercalate, replace, head, tail, takeWhile, dropWhile, strip, null)
import           Prelude   hiding (concat, head, tail, takeWhile, dropWhile, null)
import Data.Char (isSpace)

-- clean a line of text by removing delimiters
clean :: Text -> Text -> Text
clean string delim = replace delim " " string

joinWithSpace :: [Text] -> Text
joinWithSpace = intercalate " "

joinWithColon :: [Text] -> Text
joinWithColon = intercalate ", "

quote string = concat ["'", string, "'"]
propperValues values = concat [batchValues values, ";"]
batchValues values = concat ["(", values, ")"]

csvParseLine :: Text -> Char -> [Text]
csvParseLine "" _ = []
csvParseLine line delimiter | isQuote h = quotedToken: csvParseLine rest1 delimiter
                            | otherwise = unquotedToken: csvParseLine rest2 delimiter
                            where h = head line
                                  (quotedToken, rest1) = readQuotedToken line delimiter
                                  (unquotedToken, rest2) = readUnquotedToken line delimiter

isDelim d c = d == c
isQuote c = c == '\"' || c == '\''
readQuotedToken :: Text -> Char -> (Text, Text)
readQuotedToken line delim | null line = ("", "")
                           | otherwise = ( ((takeToQuote . tailSafe . strip) line),
                                            (strip . tailSafe . dropToDelim delim . dropToQuote . tailSafe) line)

readUnquotedToken :: Text -> Char -> (Text, Text)
readUnquotedToken line delim | null line = ("", "")
                             | otherwise =  ( (strip . takeToDelim delim) line,
                                               (strip . tailSafe . dropToDelim delim) line)

tailSafe :: Text -> Text
tailSafe l | null l = ""
           | otherwise = tail l

dropToDelim delim line = dropWhile (not . isDelim delim) line
dropToQuote line = dropWhile (not . isQuote) line

takeToQuote line = takeWhile (not . isQuote) line
takeToDelim delim line = takeWhile (not . isDelim delim) line