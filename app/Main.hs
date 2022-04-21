module Main where

import Lib
import Solver
import GameLogic

main :: IO ()
main = do 
    printBoard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
    gameLoopHard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "358761492267934518194528376682159743543872169971346825826415937435697281719283654"

gameLoopHard :: [Char] -> [Char] -> [Char] -> IO()
gameLoopHard boardState boardInitial boardWin = do
    putStrLn "Input: "
    -- Take input
    inputLine <- getLine

    -- Check if input is valid
    -- (Check for commands? -help, -q for quit)
    -- (if the first character is "-", use a helper function)
    let coordinates = coordToArrSlot inputLine
    if coordinates < 0 && (length inputLine) < 4 then do
        printBoard boardState
        putStrLn "Error: invalid input. Please input coordinates or a valid command\nInput '-help' for instructions."
        gameLoopHard boardState boardInitial boardWin
    else do
        putStrLn "Trying to read the integer slot"
        let moveSlot = inputLine!!3
        putStrLn "Successfull"
        -- Check the validity of the move
        putStrLn "Validity checking"
        let validMove = checkMove coordinates moveSlot boardInitial boardWin
        putStrLn "Successfull"
        
        -- If the move was invalid write an error message and recurse
        if validMove == 0 then do
            putStrLn "validMove was 0"
            printBoard boardState
            putStrLn "Error: invalid move. The slot you tried to occupy is part of the puzzle start.\nInput '-help' for instructions."
            gameLoopHard boardState boardInitial boardWin

        -- Else perform the move and continue
        else do 
            -- Update the board
            putStrLn "Updating the board"
            let board = ((take (coordinates) boardState) ++ [inputLine!!3] ++ (drop (coordinates+1) boardState))
            putStrLn "Successfull"
            -- If winning move print the boardState and return
            if board == boardWin then do
                printBoard board
                putStrLn "GG, well played! You've beat this puzzle"
                return ()
            else do
                -- Else print the board and recurse
                printBoard board
                gameLoopHard board boardInitial boardWin