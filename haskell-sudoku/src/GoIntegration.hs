module GoIntegration
    ( 
        solveHelperGo,
        validateBoardStateGo,
        checkWinGo
    ) where
import Solver (solve, validateBoard, validateBoardState, checkWin)

-- | solveHelperGo takes a user input board, prints the board solved if possible and valid, else print an error.
solveHelperGo :: [Char] -> IO()
solveHelperGo board = do
  if validateBoard board then do
    let solved = solve board
    if length solved /= 81 then do
      putStr "Error: unable to solve the board"
    else do
      putStr solved
  else
    putStr "Error: invalid input board"

-- | validateBoardStateGo function for validating the board state from go
validateBoardStateGo :: [Char] -> IO()
validateBoardStateGo board = do
    if validateBoardState board then
        putStr "Valid"
    else
        putStr "inValid"

-- | checkWinGo function for calling by golang to check the current board state for a win
checkWinGo :: [Char] -> IO()
checkWinGo board =
    if checkWin board then
        putStr "Valid"
    else
        putStr "inValid"