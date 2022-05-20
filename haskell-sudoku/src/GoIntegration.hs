module GoIntegration
    ( startGameGo ) where
import Solver (solve, validateBoard)

-- | startGameGo takes a board, solves it and writes the board and the solved board to a file
-- used in the golang frontend. If unable to solve it writes an error to the file instead
startGameGo :: [Char] -> IO()
startGameGo board = do
    -- Check if the board is valid
    if validateBoard board then do
        let boardWin = solve board
        if length boardWin /= 0 then do
            writeFile "gogame.txt" (board ++ "," ++ boardWin)
        else    
            writeFile "gogame.txt" "fail"
    else do
        putStrLn "-board requires a board as the second argument."
        putStrLn "The board should be a string of each horizontal line consequtively"
        putStrLn "The length is 81. Use -help for help."
