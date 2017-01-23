// Exercise 1.3: Experiment to measure the difference in running time between
// our potentially inefficient versions and the one that users strings.Join
// (Section 1.6 illustrates part of the time package, and Section 11.4 shows
// how to write benchmark tests for systematic performance evaluation.)

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
