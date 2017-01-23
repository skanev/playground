// Exercise 1.1: Modify the echo program to also print os.Args[0], the name of
// the command that invoked it.

package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

var out io.Writer = os.Stdout

func main() {
	echo(os.Args)
}

func echo(args []string) {
	fmt.Fprintln(out, strings.Join(args, " "))
}
