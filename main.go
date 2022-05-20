package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"os/exec"

	tea "github.com/charmbracelet/bubbletea"
)

var Board string = "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
var BoardInitial string
var BoardWin string
var KnownEndGame = true
var Message = "Awaiting move"

type model struct {
	cursor int
}

// This "tea program" is based on the "bubble tea basics tutorial" found at:
// https://pkg.go.dev/github.com/charmbracelet/bubbletea@v0.20.0#readme-tutorial
func main() {
	// Take arguments
	help := flag.Bool("help", false, "Print out the available functions.")

	// For the generate flag
	var boardValue string
	flag.StringVar(&boardValue, "board", "", "Start the game with a board")

	// Parse the flags
	flag.Parse()

	if *help {
		fmt.Println(
			"This program uses flags to run the different commands, the following flags are availablle:\n" +
				"-help  : Outputs this message\n" +
				"-board=x : Where x is the board you want to play on. Read left to right, line by line. With '_' for empty slots.\n" +
				"For example: board=___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___")
	}

	if boardValue == "" {
		fmt.Println("No valid argument provided, use -help for help.\nQuitting")
		return
	} else {
		// Validate the board state, and start the game.
		_, validBoard, err := haskellCall(2, boardValue)
		if err != nil {
			fmt.Println("Error calling haskell backend: ", err, ". Quitting.")
			return
		}
		// Quit if invalid board
		if !validBoard {
			fmt.Println("Invalid board parameter, Quitting")
			return
		}

		// Execute the haskell program to get a solved state of the board
		solvedBoard, _, err := haskellCall(1, boardValue)
		if err != nil {
			fmt.Println("Error calling haskell backend: ", err)
			return
		}
		// Start the game with an unknown end state if unable to solve it
		if solvedBoard == "" {
			KnownEndGame = false
		}
		// Set the three boards
		BoardWin = solvedBoard
		Board = boardValue
		BoardInitial = boardValue
		// Start the game
		p := tea.NewProgram(initialModel())
		if err := p.Start(); err != nil {
			fmt.Printf("An error has occured: %v", err)
			os.Exit(1)
		}
		fmt.Println("\nQuitting")
	}
}

/*
   haskellCall function takes a function selection integer and a board
   returns the board solved or a statecheck of the board or a wincheck of the board.
   Code to run other program from go retrieved from Mariusz's comment at the gitlab issue 62 found at:
   https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2022/-/issues/62#note_54012
*/
func haskellCall(function int, board string) (string, bool, error) {
	var args = []string{"-solve", "",
		board}
	switch function {
	case 1:
		args[1] = "-go"
	case 2:
		args[0] = "-validate"
		args[1] = "-state"
	case 3:
		args[0] = "-validate"
		args[1] = "-win"
	}
	output, err := exec.Command("./haskell-sudoku-exe", args...).Output()
	if err != nil {
		fmt.Println(err.Error())
		return "", false, errors.New("error running the haskell backend")
	}
	// Take the response, convert it to a string
	responseData := string(output)
	// Check the response and return the connected response
	if len(responseData) == 81 {
		return responseData, false, nil
	} else if responseData == "Valid" {
		return "", true, nil
	} else if responseData == "inValid" {
		return "", false, nil
	} else if responseData == "" {
		return "", false, nil
	}
	return "", false, errors.New("error invalid response from the haskell backend")
}

func initialModel() model {
	return model{}
}

func (m model) Init() tea.Cmd {
	// Just return `nil`, which means "no I/O right now, please."
	return nil
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {

	// Is it a key press?
	case tea.KeyMsg:

		// Cool, what was the actual key pressed?
		switch msg.String() {

		// These keys should exit the program.
		case "ctrl+c", "q":
			return m, tea.Quit

		// The "up" and "w" keys move the cursor up
		case "up", "w":
			if m.cursor > 8 {
				m.cursor -= 9
			} else {
				m.cursor += 72
			}

		// The "down" and "s" keys move the cursor down
		case "down", "s":
			if m.cursor < 72 {
				m.cursor += 9
			} else {
				m.cursor = m.cursor % 9
			}

		case "right", "d":
			if m.cursor%9 < 8 {
				m.cursor++
			} else {
				m.cursor -= 8
			}

		case "left", "a":
			if m.cursor%9 > 0 {
				m.cursor--
			} else {
				m.cursor += 8
			}

		case "r":
			Board = BoardInitial
			Message = "Reset the board to the initial board state"

		case "1":
			replaceValue("1", m.cursor)

		case "2":
			replaceValue("2", m.cursor)

		case "3":
			replaceValue("3", m.cursor)

		case "4":
			replaceValue("4", m.cursor)

		case "5":
			replaceValue("5", m.cursor)

		case "6":
			replaceValue("6", m.cursor)

		case "7":
			replaceValue("7", m.cursor)

		case "8":
			replaceValue("8", m.cursor)

		case "9":
			replaceValue("9", m.cursor)

		case "0":
			replaceValue("_", m.cursor)

		}
	}

	// Check if the game is won
	if KnownEndGame {
		if Board == BoardWin {
			fmt.Println("You've won the game. Well playeed!")
			return m, tea.Quit
		}
	} else {
		// If the end game state is unknown check through a call to the haskell backend
		_, gameWin, err := haskellCall(3, Board)
		if err != nil {
			fmt.Println("Error checking for win: ", err)
			return m, tea.Quit
		}
		if gameWin {
			fmt.Println("You've won the game. Well playeed!")
			return m, tea.Quit
		}
	}

	// Return the updated model to the Bubble Tea runtime for processing.
	// Note that we're not returning a command.
	return m, nil
}

func replaceValue(character string, position int) {
	if KnownEndGame {
		if BoardWin[position:position+1] == character {
			Message = "Performed move."
			if position == 0 {
				Board = character + Board[position+1:]
			} else if position == 80 {
				Board = Board[0:position] + character
			} else {
				Board = Board[0:position] + character + Board[position+1:]
			}
		} else {
			Message = "Wrong move, try again."
		}
	} else {
		// Do the same with a check instead
		boardCopy := Board
		if BoardInitial[position:position+1] == "_" {
			if position == 0 {
				Board = character + Board[position+1:]
			} else if position == 80 {
				Board = Board[0:position] + character
			} else {
				Board = Board[0:position] + character + Board[position+1:]
			}
		} else {
			Message = "Wrong move, try again."
			return
		}
		// Check if the boardstate is valid
		_, validMove, err := haskellCall(2, Board)
		if err != nil {
			fmt.Println("Error checking boardstate: ", err)
			Message = "Error checking move validity. Game is invalid, please restart the program."
		}
		if validMove {
			Message = "Performed move."
		} else {
			Board = boardCopy
			Message = "Wrong move, try again."
		}
	}
}

func (m model) View() string {
	// The header
	s := "      A   B   C   D   E   F   G   H   I   J\n"

	line := 1
	// Iterate over the board items
	for i := 0; i < 81; i++ {
		// Is the cursor pointing at this choice?
		cursor := " " // no cursor
		if m.cursor == i {
			cursor = ">" // cursor!
		}

		if i%27 == 0 {
			s += "     -----------   -----------   -----------"
		}

		if i%9 == 0 {
			s += fmt.Sprintf("  |\n%d | %s[%s]", line, cursor, Board[i:i+1])
			line++
		} else if i%9-8 == 0 {
			s += fmt.Sprintf("%s[%s]\n", cursor, Board[i:i+1])
		} else if i%3 == 0 {
			s += fmt.Sprintf(" |%s[%s]", cursor, Board[i:i+1])
		} else {
			s += fmt.Sprintf("%s[%s]", cursor, Board[i:i+1])
		}

	}
	// The footer
	s += "     -----------   -----------   -----------  |\n\nPress q to quit, press r to reset the board."
	// Print the message
	s += fmt.Sprintf("\n %s", Message)
	// Reset the message
	Message = "Awaiting move"
	// Send the UI for rendering
	return s
}
