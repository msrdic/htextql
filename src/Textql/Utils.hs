module Textql.Utils where

import Data.Text

-- clean a line of text by removing delimiters
clean :: String -> String -> String
clean string delim = unpack $ replace (pack delim) (pack "") (pack string)

removeDelim :: Text -> Text -> Text
removeDelim t delim = replace delim "" t

