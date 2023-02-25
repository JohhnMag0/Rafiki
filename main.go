package main

import (
	"fmt"
	"os"
	"os/user"

	"./repl"
)

func main() {
	user, err := user.Current()

	if err != nil {
		panic(err)
	}
	fmt.Printf("Rafiki Interpreter [%s]", user.Username)
	repl.Start(os.Stdin, os.Stdout)
}
