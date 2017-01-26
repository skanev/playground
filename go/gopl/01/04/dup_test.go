package main

import (
	"bytes"
	"sort"
	"strings"
	"testing"
)

func TestDup(t *testing.T) {
	out := new(bytes.Buffer)

	args := []string{"fixtures/first", "fixtures/second", "fixtures/third"}
	want := []string{"", "2\tthree fixtures/first fixtures/second", "2\tfive fixtures/second fixtures/third", "2\tseven fixtures/third"}

	dup2(out, args)

	got := strings.Split(out.String(), "\n")

	sort.Strings(want)
	sort.Strings(got)

	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Errorf("result = %q\nwant %q", got, want)
	}
}
