package main

import (
	"fmt"
	"os"

	tea "github.com/charmbracelet/bubbletea"
)

var Board string = "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"
var BoardInitial string = "___76_4__2_7___5181__52_3__68_1_9_4_54_8_2_69_7_3_6_25__6_15__7435___2_1__9_83___"

type model struct {
	cursor int
}

// This product is based on the "Bubble Tea basics" tutorial found at
// https://github.com/charmbracelet/bubbletea/tree/master/tutorials/basics

func main() {
	p := tea.NewProgram(initialModel())
	if err := p.Start(); err != nil {
		fmt.Printf("An error has occured: %v", err)
		os.Exit(1)
	}
	fmt.Println("Game won!")
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
			if m.cursor > 7 {
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
	// Return the updated model to the Bubble Tea runtime for processing.
	// Note that we're not returning a command.
	return m, nil
}

func replaceValue(character string, position int) {
	if BoardInitial[position:position+1] == "_" {
		if position == 0 {
			Board = character + Board[position+1:]
		} else if position == 80 {
			Board = Board[0:position] + character
		} else {
			Board = Board[0:position] + character + Board[position+1:]
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
	s += "     -----------   -----------   -----------  |\n\nPress q to quit.\n"

	// Send the UI for rendering
	return s
}
