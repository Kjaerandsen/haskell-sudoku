# Haskell-Sudoku

A sudoku game created in Haskell, with a frontend written in Golang.

# How to play

The goal of the game is to fill each row, column and 3 by 3 box with the numbers 1 through to 9.

This means that each row needs to contain the numbers 1 through to 9.

||||  Solved ->  ||||
| - | - | - | - | - | - | - |
|---|76-|4--||358|761|492|

Same for each column and 3 by 3 square

||  Solved ->  ||
| - | - | - |
|4--||492|
|518||518|
|3--||376|

## Haskell version
To play the premade levels start the program with the "-play" argument.

To play a self-selected level start the program with the "-board" argument and a board as the seconds argument.

The board should be in the format of a sudoku board as text, where each line is written out left-to-right consecutively. Where empty slots are written as "_".

For example: `___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___`

Inputting your move is done through typing out the coordinates to the slot you want to occupy,
and the piece or number you want in that slot.

For example: `A1 5` would place the number five in the first slot to the top left of the board.

## Golang version
The golang version can only be played with the "-board" argument. 

This is done the same way as with the haskell version described above.

For move selection in the golang version you instead have a moving cursor.
The cursor can be moved up, down, left and right using the arrow keys, or the "wasd" keys.
A piece can be put into the selected slot using the corresponding number (0-9).

# Haskell program
The Haskell program is a fully featured implementation of sudoku in haskell with two different difficulty levels, three stages and five main arguments.

The arguments for the Haskell program are:

- "-help": Shows help information, instructions to run the program.
- "-play": Starts a menu for playing the game.
- "-board": Allows inputting a board to play.
- "-solve": Allows inputting a board to receive it back solved if possible to solve.
- "-validate": Subfunctions for the Golang frontend, allows the frontend to validate and check boards for win conditions. As well as solving boards.

All commands assume you are in the `haskell-sudoku` sub-folder in the project.
There is an automated test suite that can be run using the `stack test` command.

To compile the program run `stack build`, and find the executable to run it.
The executable needs to be run in a terminal, such as "Windows Terminal" on Windows or "Bash" on Linux.
In order to use the Haskell program as a backend for the Golang frontend it needs to be in the same directory as the Golang frontend.

# Golang frontend

The golang frontend uses the bubble tea library found at https://github.com/charmbracelet/bubbletea for fancier terminal usage.

The code is based on the "Bubble Tea basics" tutorial found at https://github.com/charmbracelet/bubbletea/tree/master/tutorials/basics

The Golang frontend has two valid arguments:

- "-board": Works the same way as the corresponding argument in the Haskell program.
- "-help": Works the same way as the corresponding argument in the Haskell program.

The Golang frontend is a less feature rich implementation of the sudoku game. It uses the haskell program as a backend which it runs with parameters to run the different relevant functions in Haskell (the functions "solve", "validateBoardState" and "checkWin").

In order to use the Haskell program as a backend for the Golang frontend it needs to be in the same directory as the Golang frontend, and the Golang program needs to be run from that directory. The name of the executable should be "haskell-sudoku-exe". To run the frontend use the command `go run main.go` from the base folder of the project, and provide an argument. A terminal is required to run the frontend.

The Golang frontend is tested in the "Windows Terminal", with both the standard windows "CMD" shell and "Powershell".

## Time tracking:
1h planning the basic outline of the first version of the project. An auto-solver and a basic gameplay loop.
30m planning game loop structure.