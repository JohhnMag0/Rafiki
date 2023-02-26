// Read Eval Print Loop
package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/JohhnMag0/Rafiki/lexer"
	"github.com/JohhnMag0/Rafiki/parser"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Printf(PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}
		line := scanner.Text()
		lex := lexer.New(line)
		par := parser.New(lex)

		program := par.ParseProgram()
		if len(par.Errors()) != 0 {
			printParserErrors(out, par.Errors())
			continue
		}

		io.WriteString(out, program.String())
		io.WriteString(out, "\n")
	}
}

const MONKEY = `
     __
wc  (..)o    (
 \__(-)     __)
     /\    (
    /(_)___)
    w /|
     | \
     m  m
`

func printParserErrors(out io.Writer, errors []string) {
	io.WriteString(out, MONKEY)
	io.WriteString(out, "parser errors:\n")
	for _, msg := range errors {
		io.WriteString(out, msg+"\n")
	}
}
