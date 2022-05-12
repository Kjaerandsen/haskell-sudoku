module GameLogic
  ( printBoard,
    coordToArrSlot
  ) where

import Solver

-- | printBoard takes a board and prints it to the screen
-- uses the helper function printLineByLine to print each line of the array
printBoard :: [Char] -> IO()
printBoard arr = do
  putStrLn "    A B C   D E F   G H I\n   ------- ------- -------"
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
      putStrLn "   ------- ------- -------"
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


-- >>> read "1"::Int
-- >>> fromEnum 'a'
-- >>> fromEnum 'I'
-- >>> fromEnum '1'
-- >>> fromEnum '9'
-- 1
-- 97
-- 73
-- 49
-- 57
--

-- >>> coordToArrSlot "B5"
-- >>> coordToArrSlot "Z2"
-- >>> coordToArrSlot "b5"
-- >>> coordToArrSlot "i9"
-- >>> coordToArrSlot "A1"
-- >>> coordToArrSlot "a"
-- 37
-- -1
-- 37
-- 80
-- 0
-- -1
--


-- | coordToArrSlot takes player coordinates from input and returns the array slot
-- to be occupied. Returns -1 if the input is invalid.
coordToArrSlot :: [Char] -> Int
coordToArrSlot move = do
  let len = 9
  let character = (fromEnum (move!!0))

  -- Check that the input move is long enough
  if length move < 2 then -- Default to -1 if invalid
    (-1)
  else do
    -- Check if the integer is valid
    let integer = ((fromEnum (move!!1)) - 49)
    if integer < 9 && integer > (-1) then do
      -- Check if the char is valid, convert 'a'/'A' to 1, 'b'/'B' to 2, etc.
      -- Lowercase valid characters are in the range 97-105
      if character > 96 && character < 106 then do
        ((len * integer) + character-97)
      else
        -- Uppercase valid characters are in the range 65-73
        if character > 64 && character < 74 then do
          ((len * integer) + character-65)
        else -- Default to -1 if invalid
        (-1)
    else -- Default to -1 if invalid
      (-1)


-- The convertion used in checkMove, from integer to char
-- >>> toEnum 48::Char
-- '0'
--