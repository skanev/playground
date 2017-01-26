// Exercise 1.4: Modify dup2 to print the names of all files in which each
// duplicated line occurs.

package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

var out io.Writer = os.Stdout

func main() {
	dup2(os.Stdout, os.Args[1:])
}

func dup2(out io.Writer, files []string) {
	counts := make(map[string]int)
	locations := make(map[string]map[string]bool)

	if len(files) == 0 {
		countLines(os.Stdin, "<stdout>", counts, locations)
	} else {
		for _, filename := range files {
			f, err := os.Open(filename)
			if err != nil {
				fmt.Fprintf(os.Stderr, "dup: %v\n", err)
			}
			countLines(f, filename, counts, locations)
			f.Close()
		}
	}
	for line, n := range counts {
		if n > 1 {
			fmt.Fprintf(out, "%d\t%s", n, line)

			for location, _ := range locations[line] {
				fmt.Fprintf(out, " %s", location)
			}

			fmt.Fprintf(out, "\n")
		}
	}
}

func countLines(f *os.File, filename string, counts map[string]int, locations map[string]map[string]bool) {
	input := bufio.NewScanner(f)
	for input.Scan() {
		text := input.Text()
		counts[text]++

		if locations[text] == nil {
			locations[text] = make(map[string]bool)
		}

		locations[text][filename] = true
	}
}
