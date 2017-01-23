// Exercise 1.2: Modify the echo program to print the index and value of each
// of its arguments, one per line.

package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

func main() {
	echo1(os.Stdout, os.Args[1:])
}

func echo1(out io.Writer, args []string) {
	var s, sep string
	for i := 0; i < len(args); i++ {
		s += sep + args[i]
		sep = " "
	}
	fmt.Fprintln(out, s)
}

func echo2(out io.Writer, args []string) {
	var s, sep string
	for _, arg := range args {
		s += sep + arg
		sep = " "
	}
	fmt.Fprintln(out, s)
}

func echo3(out io.Writer, args []string) {
	fmt.Fprintln(out, strings.Join(args, " "))
}
