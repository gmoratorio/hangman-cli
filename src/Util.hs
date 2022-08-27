module Util
        ( toLowerString
        ) where

import Data.Char (toLower)

toLowerString :: String -> String
toLowerString = fmap toLower