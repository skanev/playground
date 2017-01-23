package main

import (
	"bytes"
	"strings"
	"testing"
)

var testInput = strings.Split(`
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean dictum lacinia
purus, at rutrum erat vulputate id. Vestibulum vel elementum metus, in
vehicula sem. Nullam vitae magna hendrerit, sodales erat vel, placerat sapien.
Integer pharetra, lectus a scelerisque imperdiet, tortor leo sagittis nisi, in
pellentesque quam ipsum a turpis. Morbi nec neque porta, dapibus velit at,
mattis elit. Praesent elementum nisl est, in facilisis lacus ullamcorper eu.
Praesent sed eros a nisi vulputate viverra nec et tortor. Maecenas volutpat
sed purus et fermentum. Sed pulvinar dolor ut diam imperdiet dictum. Nullam
venenatis odio nisi, vestibulum dignissim mauris auctor id. Mauris sagittis
neque interdum dignissim vulputate. Curabitur congue tempus lectus.
Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac
turpis egestas. Ut consequat convallis urna, et venenatis nisl laoreet at.
Proin id porta odio, nec luctus justo.
`, " ")

func TestEcho1(t *testing.T) {
	out := new(bytes.Buffer)

	args := []string{"ls", "-la", "whatever"}
	want := "ls -la whatever\n"

	echo1(out, args)

	got := out.String()

	if got != want {
		t.Errorf("echo(%q) = %q, want %q", args, got, want)
	}
}

func TestEcho2(t *testing.T) {
	out := new(bytes.Buffer)

	args := []string{"ls", "-la", "whatever"}
	want := "ls -la whatever\n"

	echo2(out, args)

	got := out.String()

	if got != want {
		t.Errorf("echo(%q) = %q, want %q", args, got, want)
	}
}

func TestEcho3(t *testing.T) {
	out := new(bytes.Buffer)

	args := []string{"ls", "-la", "whatever"}
	want := "ls -la whatever\n"

	echo3(out, args)

	got := out.String()

	if got != want {
		t.Errorf("echo(%q) = %q, want %q", args, got, want)
	}
}

func BenchmarkEcho1(b *testing.B) {
	out := new(bytes.Buffer)
	for i := 0; i < b.N; i++ {
		echo1(out, testInput)
	}
}

func BenchmarkEcho2(b *testing.B) {
	out := new(bytes.Buffer)
	for i := 0; i < b.N; i++ {
		echo2(out, testInput)
	}
}

func BenchmarkEcho3(b *testing.B) {
	out := new(bytes.Buffer)
	for i := 0; i < b.N; i++ {
		echo3(out, testInput)
	}
}
