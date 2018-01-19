package main

import (
	"testing"
)

func TestDealMatches(t *testing.T) {
	g := newGame()
	if have, want := len(g.deck), 81; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 0; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), uint32(4); have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.deal()
	if have, want := len(g.deck), 69; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 12; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), uint32(4); have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.dealMore()
	if have, want := len(g.deck), 66; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 15; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), uint32(5); have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	g.dealMore()
	if have, want := len(g.deck), 63; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 18; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), uint32(6); have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	for i := 0; i < 27-6; i++ {
		g.dealMore()
	}
	if have, want := len(g.deck), 0; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := len(g.listCards()), 81; have != want {
		t.Errorf("have %v, want %v", have, want)
	}
	if have, want := g.columns(), uint32(27); have != want {
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
			p1 = pos{x: 0, y: 0}
			p2 = pos{x: 3, y: 2}
			p3 = pos{x: 5, y: 1}
			q1 = pos{x: 5, y: 2}
			q2 = pos{x: 5, y: 0}
			v1 = g.cards[q1]
			v2 = g.cards[q2]
		)
		delete(g.cards, p1)
		delete(g.cards, p2)
		delete(g.cards, p3)
		g.compact()
		if have, want := g.cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.cards[p2], v2; have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.cards[p3], uint32(0); have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.cards[q1], uint32(0); have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
		if have, want := g.cards[q2], uint32(0); have != want {
			t.Errorf("q2: have %v, want %v", have, want)
		}
	}
	{
		var (
			p1 = pos{x: 0, y: 0}
			p2 = pos{x: 4, y: 0}
			p3 = pos{x: 4, y: 2}
			q1 = pos{x: 4, y: 1}
			v1 = g.cards[q1]
		)
		delete(g.cards, p1)
		delete(g.cards, p2)
		delete(g.cards, p3)
		g.compact()
		if have, want := g.cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.cards[p2], uint32(0); have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.cards[p3], uint32(0); have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.cards[q1], uint32(0); have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
	}
	{
		var (
			p1 = pos{x: 2, y: 1}
			p2 = pos{x: 4, y: 0}
			p3 = pos{x: 4, y: 1}
			q1 = pos{x: 4, y: 2}
			v1 = g.cards[q1]
		)
		delete(g.cards, p1)
		delete(g.cards, p2)
		delete(g.cards, p3)
		g.compact()
		if have, want := g.cards[p1], v1; have != want {
			t.Errorf("p1: have %v, want %v", have, want)
		}
		if have, want := g.cards[p2], uint32(0); have != want {
			t.Errorf("p2: have %v, want %v", have, want)
		}
		if have, want := g.cards[p3], uint32(0); have != want {
			t.Errorf("p3: have %v, want %v", have, want)
		}
		if have, want := g.cards[q1], uint32(0); have != want {
			t.Errorf("q1: have %v, want %v", have, want)
		}
	}
}
