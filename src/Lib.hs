module Lib
    ( 
        boardCharToInt,
        boardIntToChar,
        twoLevelInLine
    ) where

import Data.Char


-- | boardCharToInt takes a char board and returns it as an int board ("1..9" -> 1..9, '_' -> 0)
boardCharToInt :: [Char] -> [Int]
boardCharToInt board = [if x == '_' then 0 else digitToInt x | x <- board ]


-- | boardIntToChar takes an int board and returns it as a char board (1..9 -> "1..9", 0 -> '_')
boardIntToChar :: [Int] -> [Char]
boardIntToChar board = [if x == 0 then '_' else chr (48 + x) | x <- board]


-- | twoLevelInLine takes a x larger than 1-leveled array and returns the items inline as an (x - 1) leveled array.
twoLevelInLine :: [[a]] -> [a] -> [a]
twoLevelInLine twoLevelArray output= do
  if length twoLevelArray > 0 then
    twoLevelInLine (drop 1 twoLevelArray) (output ++ twoLevelArray!!0)
  else
    output