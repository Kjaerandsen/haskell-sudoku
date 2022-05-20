module Main where
-- Local libraries
import Lib
import Solver
import GameLogic
import GameLoop
import GoIntegration

import System.Environment -- For arguments

main :: IO ()
main = do 
    -- Take arguments
    args <- getArgs

    if args /= [] then do
        -- Parse the parameters
        --putStrLn (twoLevelInLine args [])
        case (args!!0) of
            -- Help should explain the other commands better.
            "-help" -> do
                putStrLn "Available commands are -help, -play, -board and -solve"
                putStrLn "-help shows this help message."
                putStrLn "-play opens the local play menu which allows the user to select a stage and difficulty."
                putStrLn "-board allows a user to input their own board as a second argument example:"
                putStrLn "-board ___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
                putStrLn "The user can then select a difficulty and play with the board."
                putStrLn "-solve also takes a user input which is a board. Returns the board solved."
            -- Hard might be benefitial to have more arguments aftewards.
            "-play" -> do
                startGame
                --printBoard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
                --gameLoopHard "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___" "358761492267934518194528376682159743543872169971346825826415937435697281719283654"
            "-board" -> do
                if length args > 1 then
                    if length args > 2 && args!!1 == "-go" then
                        startGameGo (args!!2)
                    else
                        startGameUNHelper (args!!1)
                else do
                    putStrLn "-board requires a board as the second argument."
                    putStrLn "The board should be a string of each horizontal line consequtively"
                    putStrLn "The length is 81. Use -help for help."
            "-solve" -> do
                if length args > 1 then do
                    solveHelper (args!!1)
                else do
                    putStrLn "-solve requires a board as the second argument."
                    putStrLn "The board should be a string of each horizontal line consequtively"
                    putStrLn "The length is 81. Use -help for help."
            otherwise -> putStrLn "No valid parameter provided, try running the programs with -help for help.\nExciting the program."
    else do
        putStrLn "No parameters provided, try running the programs with -help for help.\nExciting the program."