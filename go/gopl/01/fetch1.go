package main

import (
	"fmt"
	"net/http"
	"io"
	"os"
	"strings"
)

func main() {
	for _, url := range os.Args[1:] {
		if !strings.HasPrefix(url, "http://") {
			url = "http://" + url
		}
		resp, err := http.Get(url)
		if err != nil {
			fmt.Fprintf(os.Stderr, "fetch: %v\n", err)
			os.Exit(1)
		}
		bytes, err := io.Copy(os.Stdout, resp.Body)
		fmt.Printf("%v\n", resp.Status)
		resp.Body.Close()
		if err != nil {
			fmt.Fprintf(os.Stderr, "fetch: copying %s: %v\n", url, err)
			os.Exit(1)
		}
		fmt.Printf("Read %d bytes\n", bytes)
	}
}
