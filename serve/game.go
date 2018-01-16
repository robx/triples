package main

import (
	"encoding/json"
	"log"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
)

type Action struct {
}

type Update struct {
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type Games struct {
	mu    sync.Mutex
	games map[string]*Game
}

func newGames() *Games {
	return &Games{
		games: map[string]*Game{},
	}
}

func (g *Games) Get(blob Blob) *Game {
	key := blob.ChatInstance
	g.mu.Lock()
	defer g.mu.Unlock()
	game := g.games[key]
	if game != nil {
		return game
	}
	g.games[key] = newGame(blob)
	return g.games[key]
}

type Game struct {
	creator Blob
}

func newGame(blob Blob) *Game {
	g := &Game{
		creator: blob,
	}
	go g.loop()
	return g
}

func (g *Game) loop() {
	log.Printf("starting new game: %s %s", g.creator.Game, g.creator.FirstName)
}

func (g *Game) join(b Blob) (Update, <-chan Update) {
	log.Printf("player joining: %s", b.FirstName)
	return Update{}, make(chan Update)
}

func (g *Game) action(a Action) {
}

func (g *Game) Serve(b Blob, w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	actions := make(chan Action)
	go func() {
		defer close(actions)
		for {
			t, m, err := conn.NextReader()
			if err != nil {
				log.Printf("conn err: %s", err)
				return
			}
			switch t {
			case websocket.TextMessage:
				action := Action{}
				if err := json.NewDecoder(m).Decode(&action); err != nil {
					log.Printf("json err: %s", err)
					return
				}
				actions <- action
			default:
				log.Printf("ignoring message type: %d", t)
			}
		}
	}()

	writeUpdate := func(u Update) error {
		w, err := conn.NextWriter(websocket.TextMessage)
		if err != nil {
			return err
		}
		defer w.Close()
		return json.NewEncoder(w).Encode(u)
	}

	welcome, updates := g.join(b)
	if err := writeUpdate(welcome); err != nil {
		log.Print(err)
		return
	}
	for {
		select {
		case u := <-updates:
			if err := writeUpdate(u); err != nil {
				log.Print(err)
				return
			}
		case a := <-actions:
			g.action(a)
		}
	}
}
