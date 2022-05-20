module GameLoop 
    ( 
        gameLoopHard,
        gameLoopEasy,
        gameLoopHardUN,
        gameLoopEasyUN,
        startGameUN,
        startGameUNHelper,
        startGame
    ) where

import Solver
import GameLogic


-- | startGame function, allows a user to select a stage and a difficulty.
startGame :: IO()
startGame = do
    putStrLn "Sudoku: Which difficulty do you want to play with? 1 for easy, 2 for hard."
    inputLine <- getLine

    if inputLine!!0 /= '1' && inputLine!!0 /= '2' then do
        putStrLn "Invalid input"
        startGame
    else do
        putStrLn "Stage selection: Stages available: 1-easy, 2-medium, 3-hard"
        putStrLn "Input the number corresponding to the stage you want to play:"
        let difficulty = fromEnum (inputLine!!0) - 49
        inputLine <- getLine
        let stage = fromEnum (inputLine!!0) - 48
        if stage > 0 && stage < 4 then do
            
            startGameHelper stage difficulty
        else do
            putStrLn "Invalid input"
            startGame


startGameHelper :: Int -> Int -> IO()
startGameHelper stage difficulty = do
    let easyBoard =   "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
    let mediumBoard = "______427___713__5597_2_____742_5___683___259___6_874_____6_8133__471___216______"
    let hardBoard =   "_143_5762______498__________5_47__23_86___94_34__91_5__________473______5982_617_"
    let easyBoardSolved =   "358761492267934518194528376682159743543872169971346825826415937435697281719283654"
    let mediumBoardSolved = "831956427462713985597824136174295368683147259925638741749562813358471692216389574"
    let hardBoardSolved =   "914385762635712498827964315159478623786523941342691857261847539473159286598236174"
    case stage of
        1 -> do
            printBoard easyBoard
            if difficulty == 0 then
                gameLoopEasy easyBoard 
                             easyBoard 
                             easyBoardSolved
            else
                gameLoopHard easyBoard 
                             easyBoard 
                             easyBoardSolved
        2 -> do
            printBoard mediumBoard
            if difficulty == 0 then
                gameLoopEasy mediumBoard 
                             mediumBoard 
                             mediumBoardSolved
            else
                gameLoopHard mediumBoard 
                             mediumBoard 
                             mediumBoardSolved
        3 -> do
            printBoard hardBoard
            if difficulty == 0 then
                gameLoopEasy hardBoard 
                             hardBoard 
                             hardBoardSolved
            else
                gameLoopHard hardBoard 
                             hardBoard 
                             hardBoardSolved
        _ -> putStrLn "Error: invalid input to startGameHelper"


-- | startGameUNHelper helper function for starting the game with a user input board. Validates the board and
-- allows difficulty selection.
startGameUNHelper :: [Char] -> IO()
startGameUNHelper board = do
    putStrLn "Which difficulty do you want to play with? 1 for easy, 2 for hard."
    inputLine <- getLine

    if inputLine!!0 /= '1' && inputLine!!0 /= '2' then do
        putStrLn "Invalid input"
        startGameUNHelper board
    else do
        -- Convert the input to 0 or 1
        let difficulty = fromEnum (inputLine!!0) - 49

        -- Check if the board is valid
        if validateBoard board then do
            -- Start the game
            printBoard board
            startGameUN board difficulty
        else do
            putStrLn "-board requires a board as the second argument."
            putStrLn "The board should be a string of each horizontal line consequtively"
            putStrLn "The length is 81. Use -help for help."


-- | startGameUN helper function for starting a game with an unknown win state
-- tries to calculate the win state, if unable to starts the game with the unknown win state loops
startGameUN :: [Char] -> Int -> IO()
startGameUN board difficulty = do
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
                gameLoopEasyUN board board
            else
                gameLoopHardUN board board


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
                    putStrLn "Input your move using coordinates. For example A1 3."
                    putStrLn "Use -q to quit, -r to restart to the initial puzzle state and -help for this message."
                    gameLoopEasy boardState boardInitial boardWin
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopEasy boardInitial boardInitial boardWin
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0 ++ ".\nInput -help for help.")
                    gameLoopEasy boardState boardInitial boardWin
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopEasy boardState boardInitial boardWin

    else do -- check the piece (1..9)
        --putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        --putStrLn (show coordinates)
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
            --putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            --putStrLn "Successfull"
            
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
                    putStrLn "Input your move using coordinates. For example A1 3."
                    putStrLn "Use -q to quit, -r to restart to the initial puzzle state and -help for this message."
                    gameLoopHard boardState boardInitial boardWin
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopHard boardInitial boardInitial boardWin
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0 ++ ".\nInput -help for help.")
                    gameLoopHard boardState boardInitial boardWin
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopHard boardState boardInitial boardWin

    else do -- check the piece (1..9)
        --putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        --putStrLn (show coordinates)
        let validMove = checkMove coordinates moveSlot boardInitial boardWin
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do

            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopHard boardState boardInitial boardWin

        
        else do -- Else perform the move and continue
            -- Update the board
            --putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            --putStrLn "Successfull"
            
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
                    putStrLn "Input your move using coordinates. For example A1 3."
                    putStrLn "Use -q to quit, -r to restart to the initial puzzle state and -help for this message."
                    gameLoopHardUN boardState boardInitial
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopHardUN boardInitial boardInitial
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0 ++ ".\nInput -help for help.")
                    gameLoopHardUN boardState boardInitial
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopHardUN boardState boardInitial

    else do -- check the piece (1..9)
        --putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        --putStrLn (show coordinates)
        let validMove = checkMoveUN coordinates moveSlot boardInitial boardState
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do
            putStrLn "Error: invalid move. The slot you tried to occupy is either part of the puzzle start, or the piece is invalid.\nInput '-help' for instructions."
            gameLoopHardUN boardState boardInitial

        else do -- Else perform the move and continue
            -- Update the board
            --putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            --putStrLn "Successfull"

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
                    putStrLn "Input your move using coordinates. For example A1 3."
                    putStrLn "Use -q to quit, -r to restart to the initial puzzle state and -help for this message."
                    gameLoopEasyUN boardState boardInitial
                "-q" -> putStrLn "Exiting the program."
                "-r" -> do
                    putStrLn "Restarting the puzzle."
                    printBoard boardInitial
                    gameLoopEasyUN boardInitial boardInitial
                _ -> do
                    printBoard boardState
                    putStrLn ("Error: Invalid parameter. "++ (words inputLine)!!0 ++ ".\nInput -help for help.")
                    gameLoopEasyUN boardState boardInitial
            
        else do -- else simply recurse
            printBoard boardState
            putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
            gameLoopEasyUN boardState boardInitial

    else do -- check the piece (1..9)
        --putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        -- Check the validity of the move
        --putStrLn (show coordinates)
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
            --putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            --putStrLn "Successfull"

            -- If the game is won print a good game message and exit
            if checkWin board then do -- If winning move print the boardState and a win message
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"

            else do -- Else print the board and recurse
                printBoard board
                gameLoopEasyUN board boardInitial