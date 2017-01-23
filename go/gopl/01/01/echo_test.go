package main

import (
	"bytes"
	"testing"
)

func TestEcho(t *testing.T) {
	out = new(bytes.Buffer)

	args := []string{"ls", "-la", "whatever"}
	want := "ls -la whatever\n"

	echo(args)
	got := out.(*bytes.Buffer).String()

	if got != want {
		t.Errorf("echo(%q) = %q, want %q", args, got, want)
	}
}
