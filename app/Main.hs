module Main where

-- Local libraries
import Lib
import Solver
import GameLogic
import GameLoop

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
        