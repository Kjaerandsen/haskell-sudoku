module GameLogic
    ( someFunc,
      printBoard,
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- | printBoard takes a board and prints it to the screen
-- uses the helper function printLineByLine to print each line of the array
printBoard :: [Char] -> IO()
printBoard arr = do
    putStrLn "    A B C   D E F   G H I\n   -----------------------"
    printLineByLine arr 1

-- | printLineByLine takes a line length and a line list and prints the list line by line
-- uses the printLine helper function to print each line
printLineByLine :: [Char] -> Int -> IO()
printLineByLine arr ctr = do
    let len = 9
    if length arr >= len then do
        putStr ((show ctr) ++ " | ")
        printLine (take len arr)
        if ctr `mod` 3 == 0 then do
            putStrLn "   -----------------------"
            printLineByLine (drop len arr) (ctr+1)
        else do
            printLineByLine (drop len arr) (ctr+1)
    else
        return ()

-- | printLine takes an array of integers and prints them one by one separated by a single space
-- when done also print a line break
printLine :: [Char] -> IO()
printLine arr = do
    let len = 3
    if (length arr) >= len then do
        printTile (take len arr)
        printLine (drop len arr)
    else
        putStr "\n"

-- | Prints the three chars in a tile
-- called from printLine
printTile :: [Char] -> IO()
printTile i = do
    if (length i) > 0 then do
        putChar (head i)
        putChar ' '
        printTile (tail i)
    else
        putStr "| "