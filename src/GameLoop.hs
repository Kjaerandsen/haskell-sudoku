module GameLoop 
    ( 
        gameLoopHard 
    ) where

import Solver
import GameLogic

-- | startGameUN helper function for starting a game with an unknown win state
-- tries to calculate the win state, if unable to starts the game with the unknown win state loops
startHardGameUN :: [Char] -> Int -> IO()
startHardGameUN board difficulty = do
    -- Use the solve function to try to solve the inputBoard
    if length board /= 81 then
        putStrLn "Invalid board, quitting."
    else do
        let boardWin = solve board
        if length boardWin /= 0 then do
            -- Start game on the difficulty with knowledge of the end result
            if difficulty == 0 then
                gameLoopEasy board board boardWin
            else
                gameLoopHard board board boardWin
        else do
            -- Start game on the difficulty without knowing the end result
            putStrLn "Starting game without knowing the end result"
            -- replace theese with functions where boardWin is unknown
            if difficulty == 0 then
                gameLoopEasy board board boardWin
            else
                gameLoopHard board board boardWin

-- | gameLoopEasy Easy gameLoop with known answer / winstate
gameLoopEasy :: [Char] -> [Char] -> [Char] -> IO()
gameLoopEasy boardState boardInitial boardWin = do
    putStrLn "Input: "
    -- Take input
    inputLine <- getLine

    -- Check if input is valid
    -- (Check for commands? -help, -q for quit)
    -- (if the first character is "-", use a helper function)
    let coordinates = coordToArrSlot inputLine
    if coordinates < 0 || (length inputLine) < 4 then do

        -- Check if the input is a parameter
        if inputLine!!0 == '-' then do -- If it is a parameter check the parameter

            -- take the first word from the inputline, check it against a case statement
            case (words inputLine)!!0 of
                "-help" -> do
                    printBoard boardState
                    putStrLn "-help"
                    gameLoopEasy boardState boardInitial boardWin
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopEasy boardInitial boardInitial boardWin
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0)
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopEasy boardState boardInitial boardWin

    else do -- check the piece (1..9)
        putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        putStrLn (show coordinates)
        let validMove = checkMove coordinates moveSlot boardInitial boardWin
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do
            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopEasy boardState boardInitial boardWin
        
        -- Else if the move was incorrect according to the boardWin input tell the player and recurse
        else if validMove == 1 then do
            putStrLn "Error: invalid move, valid placement, but not correct according to the answer."
            gameLoopEasy boardState boardInitial boardWin
        
        else do -- Else perform the move and continue
            -- Update the board
            putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            putStrLn "Successfull"
            
            if board == boardWin then do -- If winning move print the boardState and a win message
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"

            else do -- Else print the board and recurse
                printBoard board
                gameLoopEasy board boardInitial boardWin


-- | gameLoopHard Hard gameLoop with known answer / winstate
gameLoopHard :: [Char] -> [Char] -> [Char] -> IO()
gameLoopHard boardState boardInitial boardWin = do
    putStrLn "Input: "
    -- Take input
    inputLine <- getLine

    -- Check if input is valid
    -- (Check for commands? -help, -q for quit)
    -- (if the first character is "-", use a helper function)
    let coordinates = coordToArrSlot inputLine
    if coordinates < 0 || (length inputLine) < 4 then do

        -- Check if the input is a parameter
        if inputLine!!0 == '-' then do -- If it is a parameter check the parameter

            -- take the first word from the inputline, check it against a case statement
            case (words inputLine)!!0 of
                "-help" -> do
                    printBoard boardState
                    putStrLn "-help"
                    gameLoopHard boardState boardInitial boardWin
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopHard boardInitial boardInitial boardWin
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0)
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopHard boardState boardInitial boardWin

    else do -- check the piece (1..9)
        putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        putStrLn (show coordinates)
        let validMove = checkMove coordinates moveSlot boardInitial boardWin
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do

            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopHard boardState boardInitial boardWin

        
        else do -- Else perform the move and continue
            -- Update the board
            putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            putStrLn "Successfull"
            
            if board == boardWin then do -- If winning move print the boardState and a win message
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"

            else do -- Else print the board and recurse
                printBoard board
                gameLoopHard board boardInitial boardWin


-- | gameLoopHardUN Hard gameLoop with known answer / winstate
gameLoopHardUN :: [Char] -> [Char] -> IO()
gameLoopHardUN boardState boardInitial = do
    putStrLn "Input: "
    -- Take input
    inputLine <- getLine

    -- Check if input is valid
    -- (Check for commands? -help, -q for quit)
    -- (if the first character is "-", use a helper function)
    let coordinates = coordToArrSlot inputLine
    if coordinates < 0 || (length inputLine) < 4 then do

        -- Check if the input is a parameter
        if inputLine!!0 == '-' then do -- If it is a parameter check the parameter

            -- take the first word from the inputline, check it against a case statement
            case (words inputLine)!!0 of
                "-help" -> do
                    printBoard boardState
                    putStrLn "-help"
                    gameLoopHardUN boardState boardInitial
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopHardUN boardInitial boardInitial
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0)
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopHardUN boardState boardInitial

    else do -- check the piece (1..9)
        putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        putStrLn (show coordinates)
        let validMove = checkMoveUN coordinates moveSlot boardInitial boardState
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do
            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopHardUN boardState boardInitial

        else do -- Else perform the move and continue
            -- Update the board
            putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            putStrLn "Successfull"

            -- If the game is won print a good game message and exit
            if checkWin board then do -- If winning move print the boardState and a win message
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"

            else do -- Else print the board and recurse
                printBoard board
                gameLoopHardUN board boardInitial

-- | gameLoopEasyUN Hard gameLoop with known answer / winstate
gameLoopEasyUN :: [Char] -> [Char] -> IO()
gameLoopEasyUN boardState boardInitial = do
    putStrLn "Input: "
    -- Take input
    inputLine <- getLine

    -- Check if input is valid
    -- (Check for commands? -help, -q for quit)
    -- (if the first character is "-", use a helper function)
    let coordinates = coordToArrSlot inputLine
    if coordinates < 0 || (length inputLine) < 4 then do

        -- Check if the input is a parameter
        if inputLine!!0 == '-' then do -- If it is a parameter check the parameter

            -- take the first word from the inputline, check it against a case statement
            case (words inputLine)!!0 of
                "-help" -> do
                    printBoard boardState
                    putStrLn "-help"
                    gameLoopEasyUN boardState boardInitial
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopEasyUN boardInitial boardInitial
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0)
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopEasyUN boardState boardInitial

    else do -- check the piece (1..9)
        putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        putStrLn (show coordinates)
        let validMove = checkMoveUN coordinates moveSlot boardInitial boardState

        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do
            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopEasyUN boardState boardInitial
        
        -- Else if the move was incorrect according to the boardWin input tell the player and recurse
        else if validMove == 1 then do
            putStrLn "Error: invalid move, valid placement, but leads to an invalid board state."
            gameLoopEasyUN boardState boardInitial

        else do -- Else perform the move and continue
            -- Update the board
            putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            putStrLn "Successfull"

            -- If the game is won print a good game message and exit
            if checkWin board then do -- If winning move print the boardState and a win message
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"

            else do -- Else print the board and recurse
                printBoard board
                gameLoopEasyUN board boardInitial