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
solve board = do
  board
  -- Check if the input is validat
  -- validateBoard board
  -- validateBoardState board

  -- Build an array of occupied tiles
  --[if x != "_" then x | ]
  -- List comprehension?


  -- Build an array of possible values horizontally

-- >>> filter (`elem` [1,5,7]) [1..9]
-- [1,5,7]
--

-- Using the example written by "https://stackoverflow.com/users/2253286/wit" from 
-- https://stackoverflow.com/questions/22080176/haskell-filtering-but-keeping-the-filtered
-- >>> remove f = filter (not . f)
-- >>> remove (`elem` [1,5,7]) [1..9]
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
