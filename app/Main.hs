module Main where

-- Local libraries
import Lib
import Solver
import GameLogic

import System.Environment -- For arguments

main :: IO ()
main = do 
    -- Take arguments
    args <- getArgs

    if args /= [] then do
        -- Parse the parameters
        case (args!!0) of
            -- Help should explain the other commands better.
            "-help" -> putStrLn "Available commands are -help, -hard, -easy, -solve"
            -- Hard might be benefitial to have more arguments aftewards.
            "-hard" -> do
                printBoard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
                gameLoopHard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "358761492267934518194528376682159743543872169971346825826415937435697281719283654"
            otherwise -> putStrLn "No valid parameter provided, try running the programs with -help for help.\nExciting the program."
    else do
        putStrLn "No parameters provided, try running the programs with -help for help.\nExciting the program."
        

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