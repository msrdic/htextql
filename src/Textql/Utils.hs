module Textql.Utils where

import Data.Text (Text, replace)

-- clean a line of text by removing delimiters
clean :: Text -> Text -> Text
clean string delim = replace delim "" string

removeDelim :: Text -> Text -> Text
removeDelim t delim = replace delim "" t

