module Solver
    ( solve,
      validateBoard,
      validateBoardState,
      solveHorizontal,
      solveVertical,
      solveCubic,
      solveHelper,
      checkMove,
      checkMoveUN,
      checkWin
    ) where

import GameLogic
import Data.List
import Lib


-- | solveHelper takes a user input board, prints the board solved if possible and valid, else print an error.
solveHelper :: [Char] -> IO()
solveHelper board = do
  if validateBoard board then do
    let solved = solve board
    if length solved /= 81 then do
      putStrLn "Error: Unable to solve the input board. Board might be invalid or too difficult for the solver."
      putStrLn "Use -help for help."
    else do
      printBoard solved
      putStrLn "In the input format:"
      putStrLn solved
  else
    putStrLn "Error: invalid input board. Use -help for help."


-- >>> solve "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
-- "358761492267934518194528376682159743543872169971346825826415937435697281719283654"
--

-- | solve function takes a board and returns it solved if it is solvable with a "single level" ai
solve :: [Char] -> [Char]
solve board = do
  -- First validate the board
  if (validateBoard board) /= True then
    ""
  -- Then validate the board state
  else if (validateBoardState board) /= True then
    ""
  else do
    let answer = solveLoopHelper (boardCharToInt board)
    -- Convert integer board back to char board
    boardIntToChar answer


-- | solveLoopHelper runs the solveLoop for the solve function until complete, or no progress can be made
-- then returns the updated board
solveLoopHelper :: [Int] -> [Int]
solveLoopHelper inputBoard = do
  let outputBoard = solveLoop inputBoard
  if elem 0 outputBoard && outputBoard /= inputBoard then
    solveLoopHelper outputBoard
  else
    outputBoard


-- | solveLoop single level loop for solving sudoku puzzles. Returns the solved board as an integer,
-- or if unable to solve return the progress made.
solveLoop :: [Int] -> [Int]
solveLoop board = do
  let horizontally = solveHorizontal board []
  -- Creates a board with the possible values of each line 
  -- each line has 9 repeats of the possible values as this is the return from the horizontal function
  let board1 = twoLevelInLine [replicate 9 x | x <- horizontally] []
  -- Get the same for vertical
  let vertically = solveVertical board
  -- Create a whole board from vertical values
  let dvertically = twoLevelInLine (replicate 9 vertically) []
  -- Combine vertical and horizontal boards, only keep the cross product (if both contain a digit keep it, else discard it)
  let verticalAndHorizontal = [filter (`elem` dvertically!!i) (board1!!i) | i <- [0..80]]
  -- Create a board for cubic values
  let cube = cubicToBoard (solveCubic board) []
  -- Combine the result with the cubic board
  let verticalAndHorizontalAndCube = [filter (`elem` verticalAndHorizontal!!i) (cube!!i) | i <- [0..80]]
  -- Combine with the initial board (values in the initial board replace the calculated values)
  let comparedWithInitial = [if board!!i /= 0 then [board!!i] else verticalAndHorizontalAndCube!!i | i <- [0..80]]
  -- Finally only keep the calculated values if they only contain a single choice for the slot
  [if length(comparedWithInitial!!i) == 1 then (comparedWithInitial!!i)!!0 else board!!i | i <- [0..80]]


-- >>> solveHorizontal [0,0,0,7,6,0,4,0,0] []
-- [[1,2,3,5,8,9]]
--

-- | solveHorizontal goes through a board and returns the possible values for each group of nine tiles
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
-- uses solveHorizontal for each group
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


-- >>> solveCubicHelper [0,0,0,7,6,0,4,0,0,2,0,7,0,0,0,5,1,8,1,0,0,5,2,0,3,0,0,6,8,0,1,0,9,0,4,0,5,4,0,8,0,2,0,6,9,0,7,0,3,0,6,0,2,5,0,0,6,0,1,5,0,0,7,4,3,5,0,0,0,2,0,1,0,0,9,0,8,3,0,0,0] [] 0
-- [[0,0,0,2,0,7,1,0,0],[7,6,0,0,0,0,5,2,0],[4,0,0,5,1,8,3,0,0],[6,8,0,5,4,0,0,7,0],[1,0,9,8,0,2,3,0,6],[0,4,0,0,6,9,0,2,5],[0,0,6,4,3,5,0,0,9],[0,1,5,0,0,0,0,8,3],[0,0,7,2,0,1,0,0,0]]
--


-- | solveCubic takes a board, creates a list of the 3x3 boxes in the board
-- then calculates the list of possible values for each box (not already occupied)
solveCubic :: [Int] -> [[Int]]
solveCubic board =
  solveHorizontal (twoLevelInLine (solveCubicHelper board [] 0) []) []


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


-- | cubicToBoard takes the data from solveCubic and converts it to a board for comparisons in the solve function
cubicToBoard :: [[Int]] -> [[Int]] -> [[Int]]
cubicToBoard input output = do
  if length input > 0 then do
    cubicToBoard (drop 3 input) (output ++ twoLevelInLine (replicate 3 ((twoLevelInLine(replicate 3 [input!!0]) []) ++ (twoLevelInLine(replicate 3 [input!!1]) []) ++ (twoLevelInLine(replicate 3 [input!!2]) []))) [])
  else
    output


-- Using the example written by "https://stackoverflow.com/users/2253286/wit" from 
-- https://stackoverflow.com/questions/22080176/haskell-filtering-but-keeping-the-filtered
-- | reverseFilter takes a filter and returns the opposite of the filter (selects the opposite values)
reverseFilter :: (a -> Bool) -> [a] -> [a]
reverseFilter f = filter (not . f)


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
-- >>> validateBoard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
-- False
-- True
--

-- | validateBoard takes a board and validates its content charcters and its length
validateBoard :: [Char] -> Bool
validateBoard board = 
  if length (reverseFilter (`elem` "0123456789_") board) /= 0 || length board /= 81 then
    False
  else
    True


-- | Checks if a vertical or horizontal line contains duplicates, as well as 3x3 squares.
-- returns true if no conflicts, false if any conflicts
validateBoardState :: [Char] -> Bool
validateBoardState board = do
  -- Board to integer
  let boardInt = boardCharToInt board
  -- Get, and check the Horizontal lines
  if validateValueLists (validateHorizontal boardInt []) /= True then
    False
  -- Vertical lines
  else if validateValueLists (validateHorizontal (roll True boardInt) []) /= True then
    False
  -- 3x3 grids
  else if validateValueLists (validateHorizontal (twoLevelInLine (solveCubicHelper boardInt [] 0) []) []) /= True then
    False
  -- If no failures return true
  else
    True


-- | validateHorizontal goes through a board and returns the a list of the values for each group of nine tiles, exluding 0's
validateHorizontal :: [Int] -> [[Int]] -> [[Int]]
validateHorizontal array output = do
  -- First take a line if the size is large enough
  if (length array) > 8 then do
    let line = take 9 array
    -- Create a list of possible values for the items in the line
    let items = filter (`elem` [1..9]) line
    -- recurse
    validateHorizontal (drop 9 array) (output ++ (sort [items]))
  else do
  -- If the array is empty return the output
    output


-- | validateValueLists takes a list of the values per line/cube of a board
-- returns a bool corresponding to if there are any conflicts in the input
validateValueLists :: [[Int]] -> Bool
validateValueLists input = do
  -- If there is an element
  if length input > 0 then do
    -- Check it
    -- Based on minMax function from countBirds in haskell assignment 2
    let sortedByValue = map length (group (sort (head input)))
    -- If any duplicate values return false, else recurse
    if (length (filter (>1) sortedByValue)) > 0 then
      False
    else
      validateValueLists (tail input)
  -- If there are no elements left return true
  else
    True


-- | checkMove takes an array slot, a piece, and the inital and winning board states
-- returns 0 if the move is invalid, 1 if valid and 2 if valid, but wrong
checkMove :: Int -> Char -> [Char] -> [Char] -> Int
checkMove move piece boardInitial boardWin = do
  if boardInitial!!move == '_' then
    -- Check if the piece integer is equal to the value occupying the slot in the winning board
    if piece == boardWin!!move then
      -- 2 means the slot is available, and the move is correct
      2
    else do
      -- Check if the piece is a valid piece
      let pieceNum = fromEnum piece
      -- Valid range for number 1 through to 9 is 49-57.
      if pieceNum > 48 && pieceNum < 58  then -- Incorrect move, valid slot and valid piece.
        1
      else -- Not a valid number, invalid response
        0
  else
    -- 0 means the move is invalid, or the slot is occupied
    0


-- | checkMove takes an array slot, a piece, and the inital board and current board states
-- returns 0 if the move is invalid, 1 if it is valid, 2 if it is valid, but incorrect 
-- (using the validateBoardState function)
checkMoveUN :: Int -> Char -> [Char] -> [Char] -> Int
checkMoveUN move piece boardInitial boardCurrent = do
  if boardInitial!!move == '_' then do
    -- Check if the piece is a valid piece
    let pieceNum = fromEnum piece
    -- Valid range for number 1 through to 9 is 49-57.
    if pieceNum > 48 && pieceNum < 58  then do-- valid slot and valid piece.
      -- Create a board with the move performed
      let board = ((take (move) boardCurrent) ++ [piece] ++ (drop (move+1) boardCurrent))
      -- Check it with basic board state validation
      if validateBoardState board then -- Move is valid to a basic test
        2
      else -- Move is valid, but incorrect according to the current board state
        1
    else -- Not a valid number, invalid response
      0
  else
    -- 0 means the move is invalid, or the slot is occupied
    0

-- >>> elem '_' "12323_"
-- True
--

-- | checkWin function, takes a board and checks if all slots are occupied and that the state is valid.
checkWin :: [Char] -> Bool
checkWin board = do
  -- Checks if there are any unoccupied slots
  if elem '_' board then
    False
  else -- if not, validate the board
    if validateBoard board then
      -- If the board is valid validate the board state
      -- returns true if the board state is valid (game win condition), else false
      validateBoardState board
    else
      -- if the board is invalid return false
      False
    