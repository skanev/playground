// Exercise 1.2: Modify the echo program to print the index and value of each
// of its arguments, one per line.

package main

import (
	"fmt"
	"io"
	"os"
)

var out io.Writer = os.Stdout

func main() {
	echo(os.Args)
}

func echo(args []string) {
	for i, arg := range args {
		fmt.Fprintf(out, "%d %s\n", i, arg)
	}
}
