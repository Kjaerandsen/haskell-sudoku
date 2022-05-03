module Solver
    ( someFunc,
      solve,
      validateBoard,
      validateBoardState,
      solveHorizontal,
      solveVertical
    ) where

import Data.List

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
solve board = do
  board
  -- Check if the input is validat
  -- validateBoard board
  -- validateBoardState board

  -- Build an array of occupied tiles
  --[if x != "_" then x | ]
  -- List comprehension?


  -- Build an array of possible values horizontally

  -- Vertically

  -- In a box

-- >>> solveHorizontal [0..81] []
-- [[],[],[],[],[],[],[],[9],[1,2,3,4,5,6,7,8]]
--

-- >>> filter (`elem` [1..9]) [4..9]
-- [4,5,6,7,8,9]
--

solveHorizontal :: [Int] -> [[Int]] -> [[Int]]
solveHorizontal array output = do
  -- First take a line if the size is large enough
  if (length array) > 8 then do
    let line = take 9 array
    -- Create a list of possible values for the items in the line
    let items = reverseFilter (`elem` line) [1..9]
    -- recurse
    solveHorizontal (drop 9 array) ((sort [items]) ++ output)
  else do
  -- If the array is empty return the output
    output
    

-- >>> solveVertical "daw" [1,2]
-- [[1,2,3],[1,2,3]]
--
solveVertical :: [Int] -> [[Int]] -> [[Int]]
solveVertical a b = do
  -- First roll the board to the left
  let leftRoll = roll True a
  -- Use the solveHorizontal function and roll the board to the right
  solveHorizontal leftRoll []

-- roll function that rolls the grid the provided direction, left if dir, else right
roll :: Bool -> [Int] -> [Int]
roll _ [] = []
roll dir arr = rollRowsHelper arr dir (reverse [0..8]) -- Rotate the array and return it
  

-- | rollRowsHelper takes a grid and a offset list, returns the grid rotated, left if dir, else right
rollRowsHelper :: [Int] -> Bool -> [Int] -> [Int]
rollRowsHelper arr dir offset = do
    if length offset /= 0 then
        if dir then
            (rollRowLeft 9 (head offset) 0 arr) ++ (rollRowsHelper arr dir (tail offset))
        else
            (rollRowsHelper arr dir (tail offset)) ++ reverse (rollRowLeft 9 (head  offset) 0 arr) 
    else
        []

-- | rollRowLeft recursive function that rolls a single row left
-- takes the row length and the whole grid, a deviation and a recursion counter as variables
rollRowLeft :: Int -> Int -> Int -> [Int] -> [Int]
rollRowLeft rowLen offSet recCount arr = do
    if rowLen == recCount then
        []
    else
        [arr!!((rowLen*recCount)+offSet)] ++ rollRowLeft rowLen offSet (recCount+1) arr

-- >>> filter (`elem` [1,5,7]) [1..9]
-- [1,5,7]
--

-- Using the example written by "https://stackoverflow.com/users/2253286/wit" from 
-- https://stackoverflow.com/questions/22080176/haskell-filtering-but-keeping-the-filtered

-- | reverseFilter takes a filter and returns the opposite of the filter (selects the opposite values)
reverseFilter :: (a -> Bool) -> [a] -> [a]
reverseFilter f = filter (not . f)

-- >>> filter (`elem` [1,5,7]) [1..9]
-- [1,5,7]
--
-- >>> reverseFilter (`elem` [1,5,7]) [1..9]
-- [2,3,4,6,8,9]
--



  -- >>> [ x*y | x <- [2,5,10], y <- [8,10,11]]
  -- [16,20,22,40,50,55,80,100,110]
  --

  -- Build an array of possible values vertically

  -- Build an array of possible values in each 3x3 grid

-- >>> buildOccupied "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" []
-- [3,4,6,9,11,15,16,17,18,21,22,24,27,28,30,32,34,36,37,39,41,43,44,46,48,50,52,53,56,58,59,62,63,64,65,69,71,74,76,77]
--

-- | buildOccupied takes a board and returns a list of occupied slots on the board.
buildOccupied :: [Char] -> [Int] -> [Int]
buildOccupied x y = do
  if (length x) /= 0 then do
    if head x /= '_' then
      buildOccupied (tail x) (y ++ [(81-(length x))])
    else
      buildOccupied (tail x) y
  else
    y

-- >>> validateBoard "x"
-- False
--
validateBoard :: [Char] -> Bool
validateBoard board = False


-- >>> validateBoardState "sa"
-- False
--
validateBoardState :: [Char] -> Bool
validateBoardState board = False
