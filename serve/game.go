package main

import (
	"log"
	"math/rand"
	"net/http"
	"sort"
	"sync"
	"time"

	"github.com/golang/protobuf/jsonpb"
	"github.com/gorilla/websocket"

	pb "gitlab.com/rrrob/triples/serve/proto"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

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
		//		d    = rand.Perm(81)
		d = rand.Perm(21)
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

func (g *game) findCard(c uint32) (pos, bool) {
	for p, cc := range g.cards {
		if cc == c {
			return p, true
		}
	}
	return pos{}, false
}

const (
	LATE  = 0
	RIGHT = 1
	WRONG = -1
)

func isMatch(x, y, z uint32) bool {
	check := func(a, b, c uint32) bool {
		return a == b && b == c || a != b && b != c && a != c
	}
	for i := 0; i < 4; i++ {
		if !check(x%3, y%3, z%3) {
			return false
		}
		x /= 3
		y /= 3
		z /= 3
	}
	return true
}

func (g *game) listCards() []uint32 {
	var cs []uint32
	for _, c := range g.cards {
		cs = append(cs, c)
	}
	return cs
}

func (g *game) countMatches() int {
	var (
		count = 0
		cards = g.listCards()
	)
	for i := 0; i < len(cards); i++ {
		for j := i + 1; j < len(cards); j++ {
			for k := j + 1; k < len(cards); k++ {
				if isMatch(cards[i], cards[j], cards[k]) {
					count++
				}
			}
		}
	}
	return count
}

func (g *game) gameover() bool {
	if len(g.deck) > 0 {
		return false
	}
	return g.countMatches() == 0
}

func (g *game) claimMatch(name string, cards []uint32) (int, score, *pb.Update) {
	var ps []pos
	if g.gameover() {
		return LATE, g.scores[name], nil
	}
	for _, c := range cards {
		if p, ok := g.findCard(c); !ok {
			return LATE, g.scores[name], nil
		} else {
			ps = append(ps, p)
		}
	}
	if len(cards) == 3 && isMatch(cards[0], cards[1], cards[2]) {
		for _, p := range ps {
			delete(g.cards, p)
		}
		s := g.scores[name]
		s.match += 1
		g.scores[name] = s
		return RIGHT, s, &pb.Update{
			UpdateOneof: &pb.Update_Change{
				Change: &pb.UpdateChange{
					ChangeOneof: &pb.UpdateChange_Match{
						Match: &pb.UpdateChange_ChangeMatch{
							Positions: toPbPositions(ps),
						},
					},
				},
			},
		}
	}
	s := g.scores[name]
	s.matchwrong += 1
	g.scores[name] = s
	return WRONG, s, nil
}

func (g *game) claimNomatch(name string, cards []uint32) (int, score, *pb.Update) {
	if g.gameover() || len(cards) < 12 {
		return LATE, g.scores[name], nil
	}
	cs := g.listCards()
	equal := func(as, bs []uint32) bool {
		if len(as) != len(bs) {
			return false
		}
		var (
			xs []int
			ys []int
		)
		for _, a := range as {
			xs = append(xs, int(a))
		}
		for _, b := range bs {
			ys = append(ys, int(b))
		}
		sort.Ints(xs)
		sort.Ints(ys)
		for i := 0; i < len(xs); i++ {
			if xs[i] != ys[i] {
				return false
			}
		}
		return true
	}
	if !equal(cs, cards) {
		return LATE, g.scores[name], nil
	}
	c := g.countMatches()
	if c == 0 {
		s := g.scores[name]
		s.nomatch += 1
		g.scores[name] = s
		return RIGHT, s, g.dealMore()
	}
	log.Printf("wrong nomatch claim, %d matches, these cards %+v", c, cards)
	s := g.scores[name]
	s.nomatchwrong += 1
	g.scores[name] = s
	return WRONG, s, makeRevealCount(c)
}

func (g *game) dealMore() *pb.Update {
	cs := map[pos]uint32{}
	x := g.columns()
	for y := uint32(0); y < 3; y++ {
		p := pos{x: x, y: y}
		if len(g.deck) > 0 {
			c := g.deck[0]
			g.deck = g.deck[1:]
			g.cards[p] = c
			cs[p] = c
		}
	}
	return makeDeal(cs)
}

func (g *game) columns() uint32 {
	var m uint32
	for p := range g.cards {
		if p.x+1 > m {
			m = p.x + 1
		}
	}
	if m < 4 {
		return 4
	}
	return m
}

func (g *game) empty(p pos) bool {
	_, ok := g.cards[p]
	return !ok
}

func (g *game) compact() *pb.Update {
	cols := g.columns()
	up := func(p pos) pos {
		if p.y == 2 {
			return pos{x: p.x + 1, y: 0}
		} else {
			return pos{x: p.x, y: p.y + 1}
		}
	}
	down := func(p pos) pos {
		if p.y == 0 {
			return pos{x: p.x - 1, y: 2}
		} else {
			return pos{x: p.x, y: p.y - 1}
		}
	}
	l := pos{x: 0, y: 0}
	h := pos{x: cols - 1, y: 2}
	var moves []*pb.UpdateChange_ChangeMove_MoveOne
	for {
		for ; !g.empty(l) && l.x < cols; l = up(l) {
		}
		for ; g.empty(h) && h.x > l.x && h.x >= 4; h = down(h) {
		}
		if g.empty(l) && !g.empty(h) && h.x > l.x && h.x >= 4 {
			g.cards[l] = g.cards[h]
			delete(g.cards, h)
			moves = append(moves, &pb.UpdateChange_ChangeMove_MoveOne{
				From: toPbPosition(h),
				To:   toPbPosition(l),
			})
		} else {
			break
		}
	}
	if len(moves) == 0 {
		return nil
	}
	return &pb.Update{
		UpdateOneof: &pb.Update_Change{
			Change: &pb.UpdateChange{
				ChangeOneof: &pb.UpdateChange_Move{
					Move: &pb.UpdateChange_ChangeMove{
						Moves: moves,
					},
				},
			},
		},
	}
}

func (g *game) deal() *pb.Update {
	d := map[pos]uint32{}
	for x := 0; x < 4; x++ {
		for y := 0; y < 3; y++ {
			p := pos{x: uint32(x), y: uint32(y)}
			if _, ok := g.cards[p]; !ok {
				if len(g.deck) > 0 {
					c := g.deck[0]
					g.deck = g.deck[1:]
					g.cards[p] = c
					d[p] = c
				}
			}
		}
	}
	log.Printf("dealing %d cards", len(d))
	return makeDeal(d)
}

func makeRevealCount(count int) *pb.Update {
	return nil
}

func makeDeal(cs map[pos]uint32) *pb.Update {
	if len(cs) == 0 {
		return nil
	}
	return &pb.Update{
		UpdateOneof: &pb.Update_Change{
			Change: &pb.UpdateChange{
				ChangeOneof: &pb.UpdateChange_Deal{
					Deal: &pb.UpdateChange_ChangeDeal{
						Cards: toPbCards(cs),
					},
				},
			},
		},
	}
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
		var updates []*pb.Update
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
			updates = append(updates, makeJoin(c.blob))
		case cl := <-r.claims:
			c := clients[cl.clientId]
			if cl.claim == nil {
				log.Printf("removing client %d", cl.clientId)
				close(c.updates)
				delete(clients, cl.clientId)
				// todo: clean-up present?
			} else {
				log.Print("received claim")
				if g == nil || g.gameover() {
					if cl.claim.Type == pb.ClaimType_CLAIM_NOMATCH && len(cl.claim.Cards) == 0 {
						log.Printf("starting game on behalf of %s", c.blob.FirstName)
						g = newGame()
						for p := range present {
							g.add(p)
						}
						updates = append(updates, makeFull(g, present), g.deal())
					} else {
						log.Printf("out of game claim: %+v", cl.claim)
					}
				} else {
					switch cl.claim.Type {
					case pb.ClaimType_CLAIM_MATCH:
						res, score, up := g.claimMatch(c.blob.FirstName, cl.claim.Cards)
						updates = append(updates,
							up,
							g.compact(),
							g.deal(),
							makeClaimed(c.blob.FirstName, cl.claim.Type, res, score))
						if g.gameover() {
							log.Printf("game over")
							h := &game{
								scores: g.scores,
							}
							g = nil
							updates = append(updates, makeFull(h, present))
						}
					case pb.ClaimType_CLAIM_NOMATCH:
						res, score, up := g.claimNomatch(c.blob.FirstName, cl.claim.Cards)
						updates = append(updates,
							up,
							makeClaimed(c.blob.FirstName, cl.claim.Type, res, score))
					default:
						log.Printf("ignoring claim: %+v", cl.claim)
					}
				}
			}
		}
		count := 0
		for _, update := range updates {
			if update == nil {
				continue
			}
			for _, c := range clients {
				c.updates <- update
			}
		}
		log.Printf("sent %d messages to %d clients", count, len(clients))
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

func toPbResult(r int) pb.UpdateEvent_EventClaimed_Result {
	switch r {
	case RIGHT:
		return pb.UpdateEvent_EventClaimed_CORRECT
	case WRONG:
		return pb.UpdateEvent_EventClaimed_WRONG
	default:
		return pb.UpdateEvent_EventClaimed_LATE
	}
}

func makeClaimed(name string, typ pb.ClaimType, res int, s score) *pb.Update {
	return &pb.Update{
		UpdateOneof: &pb.Update_Event{
			Event: &pb.UpdateEvent{
				EventOneof: &pb.UpdateEvent_Claimed{
					Claimed: &pb.UpdateEvent_EventClaimed{
						Name:   name,
						Type:   typ,
						Result: toPbResult(res),
						Score:  toPbScore(s),
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

func toPbPosition(p pos) *pb.Position {
	return &pb.Position{
		X: p.x,
		Y: p.y,
	}
}

func toPbPositions(ps []pos) []*pb.Position {
	var out []*pb.Position
	for _, p := range ps {
		out = append(out, toPbPosition(p))
	}
	return out
}

func toPbCards(cards map[pos]uint32) []*pb.PlacedCard {
	var out []*pb.PlacedCard
	for p, c := range cards {
		out = append(out, &pb.PlacedCard{
			Position: toPbPosition(p),
			Card:     c,
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
		deckSize uint32 = 0
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
