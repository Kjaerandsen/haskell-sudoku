# haskell-sudoku

sudoku game created in haskell

## Time tracking:
1h planning the basic outline of the first version of the project. An auto-solver and a basic gameplay loop.
30m planning game loop structure.

## How to play

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

# Golang frontend

The golang frontend uses the bubble tea library found at https://github.com/charmbracelet/bubbletea for fancier terminal usage.

The code is based on the "Bubble Tea basics" tutorial found at https://github.com/charmbracelet/bubbletea/tree/master/tutorials/basics