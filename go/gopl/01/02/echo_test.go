package main

import (
	"bytes"
	"testing"
)

func TestEcho(t *testing.T) {
	out = new(bytes.Buffer)

	args := []string{"ls", "-la", "whatever"}
	want := "0 ls\n1 -la\n2 whatever\n"

	echo(args)
	got := out.(*bytes.Buffer).String()

	if got != want {
		t.Errorf("echo(%q) = %q, want %q", args, got, want)
	}
}
