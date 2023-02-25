package main

import (
	"fmt"
	"os"
	"os/user"

	"github.com/JohhnMag0/Rafiki/repl"
)

func main() {
	user, err := user.Current()

	if err != nil {
		panic(err)
	}
	fmt.Printf("Rafiki Interpreter [%s]\n", user.Username)
	repl.Start(os.Stdin, os.Stdout)
}
