package main

import (
	"log"
	"math/rand"
	"net/http"
	"sync"

	"github.com/golang/protobuf/jsonpb"
	"github.com/gorilla/websocket"

	pb "gitlab.com/rrrob/triples/serve/proto"
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type Rooms struct {
	mu    sync.Mutex
	rooms map[string]*Room
}

func newRooms() *Rooms {
	return &Rooms{
		rooms: map[string]*Room{},
	}
}

func (rs *Rooms) Get(blob Blob) *Room {
	key := blob.ChatInstance
	rs.mu.Lock()
	defer rs.mu.Unlock()
	room := rs.rooms[key]
	if room != nil {
		return room
	}
	rs.rooms[key] = newRoom(blob)
	return rs.rooms[key]
}

type Room struct {
	creator  Blob
	connects chan *client
	claims   chan *claim
}

type claim struct {
	clientId int
	claim    *pb.Claim
}

type client struct {
	blob    Blob
	updates chan<- *pb.Update
	sendId  chan<- int
}

func newRoom(blob Blob) *Room {
	r := &Room{
		creator:  blob,
		connects: make(chan *client),
		claims:   make(chan *claim),
	}
	go r.loop()
	return r
}

type game struct {
	deck   []uint32
	cards  map[pos]uint32
	scores map[string]score
}

func newGame() *game {
	var (
		deck []uint32
		d    = rand.Perm(81)
	)
	for _, i := range d {
		deck = append(deck, uint32(i))
	}
	return &game{
		deck:   deck,
		cards:  map[pos]uint32{},
		scores: map[string]score{},
	}
}

func (g *game) deckSize() uint32 {
	return uint32(len(g.deck))
}

func (g *game) add(player string) {
	s := g.scores[player]
	g.scores[player] = s
}

func (r *Room) loop() {
	log.Printf("starting new room: %s %s", r.creator.Game, r.creator.FirstName)
	var (
		clientId int
		clients  = map[int]*client{}
		present  = map[string]struct{}{}
		g        *game
	)
	for {
		var update *pb.Update
		select {
		case c := <-r.connects:
			c.sendId <- clientId
			clients[clientId] = c
			clientId++
			present[c.blob.FirstName] = struct{}{}
			if g != nil {
				g.add(c.blob.FirstName)
			}
			c.updates <- makeFull(g, present)
			update = makeJoin(c.blob)
		case cl := <-r.claims:
			if cl.claim == nil {
				log.Printf("removing client %d", cl.clientId)
				c := clients[cl.clientId]
				close(c.updates)
				delete(clients, cl.clientId)
			} else {
				log.Print("received claim")
			}
		}
		if update != nil {
			log.Printf("sending message to %d clients", len(clients))
			for _, c := range clients {
				c.updates <- update
			}
		}
	}
}

func makeJoin(b Blob) *pb.Update {
	return &pb.Update{
		UpdateOneof: &pb.Update_Event{
			Event: &pb.UpdateEvent{
				EventOneof: &pb.UpdateEvent_Join{
					Join: &pb.UpdateEvent_EventJoin{
						Name: b.FirstName,
					},
				},
			},
		},
	}
}

type pos struct {
	x uint32
	y uint32
}

type score struct {
	match        uint32
	matchwrong   uint32
	nomatch      uint32
	nomatchwrong uint32
}

func toPbCards(cards map[pos]uint32) []*pb.PlacedCard {
	var out []*pb.PlacedCard
	for p, c := range cards {
		out = append(out, &pb.PlacedCard{
			Position: &pb.Position{
				X: p.x,
				Y: p.y,
			},
			Card: c,
		})
	}
	return out
}

func toPbPlayerScores(scores map[string]score) []*pb.UpdateFull_PlayerScore {
	var out []*pb.UpdateFull_PlayerScore
	for p, s := range scores {
		out = append(out, &pb.UpdateFull_PlayerScore{
			Name:  p,
			Score: toPbScore(s),
		})
	}
	return out
}

func toZeroPbPlayerScores(scores map[string]struct{}) []*pb.UpdateFull_PlayerScore {
	var out []*pb.UpdateFull_PlayerScore
	for p := range scores {
		out = append(out, &pb.UpdateFull_PlayerScore{
			Name:  p,
			Score: &pb.Score{},
		})
	}
	return out
}

func toPbScore(s score) *pb.Score {
	return &pb.Score{
		Match:        s.match,
		Matchwrong:   s.matchwrong,
		Nomatch:      s.nomatch,
		Nomatchwrong: s.nomatchwrong,
	}
}

func makeFull(g *game, present map[string]struct{}) *pb.Update {
	var (
		deckSize uint32 = 81
		cards    []*pb.PlacedCard
		scores   = toZeroPbPlayerScores(present)
	)
	if g != nil {
		deckSize = g.deckSize()
		scores = toPbPlayerScores(g.scores)
		cards = toPbCards(g.cards)
	}
	return &pb.Update{
		UpdateOneof: &pb.Update_Full{
			Full: &pb.UpdateFull{
				Cols:      4,
				Rows:      3,
				MatchSize: 3,
				DeckSize:  deckSize,
				Cards:     cards,
				Scores:    scores,
			},
		},
	}
}

func (r *Room) connect(b Blob) (<-chan *pb.Update, chan<- *claim, <-chan int) {
	log.Printf("player connecting: %s", b.FirstName)
	updates := make(chan *pb.Update)
	sendId := make(chan int)
	r.connects <- &client{
		blob:    b,
		updates: updates,
		sendId:  sendId,
	}
	return updates, r.claims, sendId
}

func (r *Room) Serve(b Blob, w http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	updates, claims, getId := r.connect(b)

	go func() {
		clientId := <-getId
		defer func() {
			claims <- &claim{
				clientId: clientId,
				claim:    nil,
			}
		}()
		for {
			t, m, err := conn.NextReader()
			if err != nil {
				log.Printf("conn err: %s", err)
				return
			}
			switch t {
			case websocket.TextMessage:
				log.Printf("receiving message from %s", b.FirstName)
				cl := &pb.Claim{}
				if err := jsonpb.Unmarshal(m, cl); err != nil {
					log.Printf("proto err: %s", err)
					return
				}
				claims <- &claim{
					clientId: clientId,
					claim:    cl,
				}
			default:
				log.Printf("ignoring message type: %d", t)
			}
		}
	}()

	for u := range updates {
		log.Printf("sending message to %s", b.FirstName)
		if err := writeUpdate(conn, u); err != nil {
			log.Print(err)
		}
	}
	log.Print("left room")
}

func writeUpdate(conn *websocket.Conn, u *pb.Update) error {
	w, err := conn.NextWriter(websocket.TextMessage)
	if err != nil {
		return err
	}
	defer w.Close()
	m := jsonpb.Marshaler{}
	return m.Marshal(w, u)
}
