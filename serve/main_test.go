package main

import (
	"testing"
)

func TestRandString(t *testing.T) {
	s := randHex(5)
	if have, want := len(s), 2*5; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
}
