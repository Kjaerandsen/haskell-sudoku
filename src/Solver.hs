module Solver
    ( someFunc,
      solve,
      validateBoard,
      validateBoardState
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> buildLineArr [1,2,3]
-- [[1,2,3],[1,2,3]]
--

buildLineArr :: [Int] -> [[Int]]
buildLineArr line = do
    -- Read each individual integer present in the array.

    -- Fill each not-filled array slot with theese possible integers
    -- and the old filled slots with "0"
    [line,line]

solve :: [Char] -> [Char]
solve x = x

-- >>> validateBoard ["x"]
-- (Error while loading modules for evaluation)
-- <BLANKLINE>
-- Failed, no modules loaded.
-- <no location info>: error:
--     module `main:Main' is defined in multiple files: n:\Skole\AdvancedProgramming\haskell-sudoku\app\Main.hs
--                                                      n:\Skole\AdvancedProgramming\haskell-sudoku\test\Spec.hs
--

validateBoard :: [Char] -> Bool
validateBoard x = False

validateBoardState :: [Char] -> Bool
validateBoardState x = False
