module Lib
    ( 
        boardCharToInt
    ) where

import Data.Char

-- | boardCharToInt takes a char board and returns it as an int board ("1..9" -> 1..9, '_' -> 0)
boardCharToInt :: [Char] -> [Int]
boardCharToInt board = [if x == '_' then 0 else digitToInt x | x <- board ]