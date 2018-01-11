package main

import (
	"testing"
)

func TestBlob(t *testing.T) {
	b := Blob{
		UserID:          1234,
		InlineMessageID: "I'm a rather less regular ID than will be in there.",
	}

	if bb, err := decode(encode(b)); err != nil {
		t.Fatal(err)
	} else if b != bb {
		t.Errorf("have %v, want %v", bb, b)
	}
}

func TestBlobShort(t *testing.T) {
	_, err := decode("short")
	if err == nil {
		t.Error("expected error")
	}
}
