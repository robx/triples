package main

import (
	"testing"
)

func TestBlob(t *testing.T) {
	b := Blob{
		UserID:          1234,
		InlineMessageID: "I'm a rather less regular ID than will be in there.",
	}
	k := genKey()

	if bb, err := decode(encode(b, k), k); err != nil {
		t.Fatal(err)
	} else if b != bb {
		t.Errorf("have %v, want %v", bb, b)
	}
}

func TestBlobShort(t *testing.T) {
	k := genKey()
	_, err := decode("short", k)
	if err == nil {
		t.Error("expected error")
	}
}
