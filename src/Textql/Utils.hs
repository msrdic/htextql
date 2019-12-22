{-# LANGUAGE OverloadedStrings #-}
module Textql.Utils where

import              Prelude hiding (concat)
import              Data.Text (Text, replace, intercalate, concat)

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
