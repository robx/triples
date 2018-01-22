package main

import (
	"testing"
)

func TestDealMatches(t *testing.T) {
	g := newGame()
	if have, want := len(g.Deck), 81; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 0; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), 4; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.deal()
	if have, want := len(g.Deck), 69; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 12; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), 4; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.dealMore()
	if have, want := len(g.Deck), 66; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 15; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), 5; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.dealMore()
	if have, want := len(g.Deck), 63; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 18; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), 6; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	for i := 0; i < 27-6; i++ {
		g.dealMore()
	}
	if have, want := len(g.Deck), 0; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 81; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), 27; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.countMatches(), 1080; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
}

func TestCompact(t *testing.T) {
	g := newGame()
	g.deal()
	g.dealMore()
	g.dealMore()
	if have, want := len(g.listCards()), 18; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	{
		var (
			p1 = Position{X: 0, Y: 0}
			p2 = Position{X: 3, Y: 2}
			p3 = Position{X: 5, Y: 1}
			q1 = Position{X: 5, Y: 2}
			q2 = Position{X: 5, Y: 0}
			v1 = g.Cards[q1]
			v2 = g.Cards[q2]
		)
		delete(g.Cards, p1)
		delete(g.Cards, p2)
		delete(g.Cards, p3)
		g.compact()
		if have, want := g.Cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p2], v2; have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p3], 0; have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.Cards[q1], 0; have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
		if have, want := g.Cards[q2], 0; have != want {
			t.Errorf("q2: have %v, want %v", have, want)
		}
	}
	{
		var (
			p1 = Position{X: 0, Y: 0}
			p2 = Position{X: 4, Y: 0}
			p3 = Position{X: 4, Y: 2}
			q1 = Position{X: 4, Y: 1}
			v1 = g.Cards[q1]
		)
		delete(g.Cards, p1)
		delete(g.Cards, p2)
		delete(g.Cards, p3)
		g.compact()
		if have, want := g.Cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p2], 0; have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p3], 0; have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.Cards[q1], 0; have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
	}
	{
		var (
			p1 = Position{X: 2, Y: 1}
			p2 = Position{X: 4, Y: 0}
			p3 = Position{X: 4, Y: 1}
			q1 = Position{X: 4, Y: 2}
			v1 = g.Cards[q1]
		)
		delete(g.Cards, p1)
		delete(g.Cards, p2)
		delete(g.Cards, p3)
		g.compact()
		if have, want := g.Cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p2], 0; have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.Cards[p3], 0; have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.Cards[q1], 0; have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
	}
}
