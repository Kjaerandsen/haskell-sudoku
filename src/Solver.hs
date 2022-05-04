module Solver
    ( someFunc,
      solve,
      validateBoard,
      validateBoardState,
      solveHorizontal,
      solveVertical,
      solveCubic
    ) where

import Data.List
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> buildLineArr [1,2,3]
-- [[1,2,3],[1,2,3]]
--

-- >>> [if x == '_' then 0 else digitToInt x | x <- "___123456789____" ]
-- [0,0,0,1,2,3,4,5,6,7,8,9,0,0,0,0]
--

buildLineArr :: [Int] -> [[Int]]
buildLineArr line = do
    -- Read each individual integer present in the array.
    -- Fill each not-filled array slot with theese possible integers
    -- and the old filled slots with "0"
    [line,line]


-- >>> solve "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
-- *** Exception: Prelude.!!: index too large
-- [[3,8,9],[5,9],[3,8],[7],[6],[1,8],[4],[9],[2],[2],[9],[7],[4],[4],[4],[5],[1],[8],[1],[],[8],[5],[2],[4,7],[3],[8,9],[4,6],[6],[8],
--

-- repeat

solve :: [Char] -> [[Int]]
solve board = do
  -- Board to integer using list comprehension
  let boardInt = [if x == '_' then 0 else digitToInt x | x <- board ]

  let horizontally = solveHorizontal boardInt []
  -- Update the board with the horizontal values if not occupied already
  -- let updatedBoard = [if x /= 0 then horizontally!!0 else [x] | x <- boardInt]

  --let board1 = firstCompare boardInt horizontally 0 []
  let board1 = twoLevelInline [replicate 9 x | x <- horizontally] []

  let vertically = solveVertical boardInt
  -- Create a whole board from vertical values
  let dvertically = vertically ++ vertically ++ vertically ++ vertically ++ vertically ++ vertically ++ vertically ++ vertically ++ vertically

  let verticalAndHorizontal = [filter (`elem` dvertically!!i) (board1!!i) | i <- [0..80]]
  -- [filter (`elem` dvertically!!i) board1!!i | i <- [0..80]]
  -- [filter (`elem` x) y | x <- board1, y <- dvertically]

  let cube = cubicToBoard (solveCubic boardInt) []

  let verticalAndHorizontalAndCube = [filter (`elem` verticalAndHorizontal!!i) (cube!!i) | i <- [0..80]]

  let comparedWithInitial = [if boardInt!!i /= 0 then [boardInt!!i] else verticalAndHorizontalAndCube!!i | i <- [0..80]]

  --cube
  comparedWithInitial
  --verticalAndHorizontalAndCube
  --board

  -- >>> filter (`elem` [[1,2,3,5]]) [[1,2,3]]
  -- []
  --

  -- Check if the input is validat
  -- validateBoard board
  -- validateBoardState board

  -- Build an array of occupied tiles
  --[if x != "_" then x | ]
  -- List comprehension?


  -- Build an array of possible values horizontally

  -- Vertically

  -- In a box

  -- Compare initial array with the horizontal arary

  -- Compare result with the vertical array

  -- Compare result with the cubic array

firstCompare :: [Int] -> [[Int]] -> Int -> [[Int]] -> [[Int]]
firstCompare board moves counter output = do
  if counter < 9 then do
    firstCompare (drop 9 board) moves (counter+1) (output ++ [if x /= 0 then moves!!counter else [x] | x <- (take 9 board)])
  else
    output

-- >>> solveHorizontal [0..81] []
-- [[],[],[],[],[],[],[],[9],[1,2,3,4,5,6,7,8]]
--

-- >>> filter (`elem` [1..9]) [4..9]
-- [4,5,6,7,8,9]
--

-- | cubicToBoard takes the data from solveCubic and converts it to a board for comparisons in the solve function
cubicToBoard :: [[Int]] -> [[Int]] -> [[Int]]
cubicToBoard input output = do
  if length input > 0 then do
    cubicToBoard (drop 3 input) (output ++ twoLevelInline (replicate 3 ((twoLevelInline(replicate 3 [input!!0]) []) ++ (twoLevelInline(replicate 3 [input!!1]) []) ++ (twoLevelInline(replicate 3 [input!!2]) []))) [])
  else
    output

-- | solveCubic takes a board, creates a list of the 3x3 boxes in the board
-- then calculates the list of possible values for each box (not already occupied)
solveCubic :: [Int] -> [[Int]]
solveCubic board =
  solveHorizontal (twoLevelInline (solveCubicHelper board [] 0) []) []

-- | twoLevelInline takes a double-leveled array and returns the items consecutively as a single level array.
twoLevelInline :: [[a]] -> [a] -> [a]
twoLevelInline twoLevelArray output= do
  if length twoLevelArray > 0 then
    twoLevelInline (drop 1 twoLevelArray) (output ++ twoLevelArray!!0)
  else
    output

-- >>> solveCubicHelper [0,0,0,7,6,0,4,0,0,2,0,7,0,0,0,5,1,8,1,0,0,5,2,0,3,0,0,6,8,0,1,0,9,0,4,0,5,4,0,8,0,2,0,6,9,0,7,0,3,0,6,0,2,5,0,0,6,0,1,5,0,0,7,4,3,5,0,0,0,2,0,1,0,0,9,0,8,3,0,0,0] [] 0
-- [[0,0,0,2,0,7,1,0,0],[7,6,0,0,0,0,5,2,0],[4,0,0,5,1,8,3,0,0],[6,8,0,5,4,0,0,7,0],[1,0,9,8,0,2,3,0,6],[0,4,0,0,6,9,0,2,5],[0,0,6,4,3,5,0,0,9],[0,1,5,0,0,0,0,8,3],[0,0,7,2,0,1,0,0,0]]
--

-- | solveCubicHelper takes a board, returns the 3x3 boxes in the board
solveCubicHelper :: [Int] -> [[Int]] -> Int -> [[Int]]
solveCubicHelper arr output counter = do
  if length arr > 0 && counter == 3 then do
    solveCubicHelper (drop 27 arr) (output) 0
  else if length arr > 0 && counter < 3 then do
    let offset = 3*counter
    let addToOutput = [arr!!(0+offset),arr!!(1+offset),arr!!(2+offset),
                       arr!!(9+offset),arr!!(10+offset),arr!!(11+offset),
                       arr!!(18+offset),arr!!(19+offset),arr!!(20+offset)]
    solveCubicHelper arr (output ++ [addToOutput]) (counter+1)
  else
    output

-- >>> solveHorizontal [0,0,0,7,6,0,4,0,0] []
-- [[1,2,3,5,8,9]]
--
solveHorizontal :: [Int] -> [[Int]] -> [[Int]]
solveHorizontal array output = do
  -- First take a line if the size is large enough
  if (length array) > 8 then do
    let line = take 9 array
    -- Create a list of possible values for the items in the line
    let items = reverseFilter (`elem` line) [1..9]
    -- recurse
    solveHorizontal (drop 9 array) (output ++ (sort [items]))
  else do
  -- If the array is empty return the output
    output
    

-- >>> solveVertical "daw" [1,2]
-- [[1,2,3],[1,2,3]]
--

-- | solveVertical takes a board, returns an array of the possible values for each vertical line
solveVertical :: [Int] -> [[Int]]
solveVertical a = do
  -- First roll the board to the left
  let leftRoll = roll True a
  -- Use the solveHorizontal function and roll the board to the right
  reverse (solveHorizontal leftRoll [])


-- Roll functions are slightly modified version of the roll functions used in the haskell tick-tack-roll assignment
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
