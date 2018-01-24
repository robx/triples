package main

import (
	"log"
	"math/rand"
	"net/http"
	"sort"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	"gopkg.in/edn.v1"
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
	cmds     chan *cmd
}

type cmd struct {
	clientId int
	command  Command
}

type client struct {
	blob    Blob
	updates chan<- Update
	sendId  chan<- int
}

func newRoom(blob Blob) *Room {
	r := &Room{
		creator:  blob,
		connects: make(chan *client),
		cmds:     make(chan *cmd),
	}
	go r.loop()
	return r
}

type Game struct {
	Deck   []int
	Cards  map[Position]int
	Scores map[string]Score
}

func newGame() *Game {
	return &Game{
		Deck:   rand.Perm(81),
		Cards:  map[Position]int{},
		Scores: map[string]Score{},
	}
}

func (g *Game) deckSize() int {
	return len(g.Deck)
}

func (g *Game) add(player string) {
	s := g.Scores[player]
	g.Scores[player] = s
}

func (g *Game) findCard(c int) (Position, bool) {
	for p, cc := range g.Cards {
		if cc == c {
			return p, true
		}
	}
	return Position{}, false
}

func isMatch(x, y, z int) bool {
	check := func(a, b, c int) bool {
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

func (g *Game) listCards() []int {
	var cs []int
	for _, c := range g.Cards {
		cs = append(cs, c)
	}
	return cs
}

func (g *Game) countMatches() int {
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

func (g *Game) gameover() bool {
	if len(g.Deck) > 0 {
		return false
	}
	return g.countMatches() == 0
}

func (g *Game) claimMatch(name string, cards []int) (ResultType, Score, Update) {
	var ps []Position
	if g.gameover() {
		return ResultLate, g.Scores[name], nil
	}
	for _, c := range cards {
		if p, ok := g.findCard(c); !ok {
			return ResultLate, g.Scores[name], nil
		} else {
			ps = append(ps, p)
		}
	}
	if len(cards) == 3 && isMatch(cards[0], cards[1], cards[2]) {
		for _, p := range ps {
			delete(g.Cards, p)
		}
		s := g.Scores[name]
		s.Match += 1
		g.Scores[name] = s
		return ResultCorrect, s, ChangeMatch(ps)
	}
	s := g.Scores[name]
	s.MatchWrong += 1
	g.Scores[name] = s
	return ResultWrong, s, nil
}

func (g *Game) claimNomatch(name string, cards []int) (ResultType, Score, Update) {
	if g.gameover() || len(cards) < 12 {
		return ResultLate, g.Scores[name], nil
	}
	cs := g.listCards()
	equal := func(as, bs []int) bool {
		if len(as) != len(bs) {
			return false
		}
		sort.Ints(as)
		sort.Ints(bs)
		for i := 0; i < len(as); i++ {
			if as[i] != bs[i] {
				return false
			}
		}
		return true
	}
	if !equal(cs, cards) {
		return ResultLate, g.Scores[name], nil
	}
	c := g.countMatches()
	if c == 0 {
		s := g.Scores[name]
		s.NoMatch += 1
		g.Scores[name] = s
		return ResultCorrect, s, g.dealMore()
	}
	log.Printf("wrong nomatch claim, %d matches, these cards %+v", c, cards)
	s := g.Scores[name]
	s.NoMatchWrong += 1
	g.Scores[name] = s
	return ResultWrong, s, makeRevealCount(c)
}

func (g *Game) dealMore() Update {
	var cs []PlacedCard
	x := g.columns()
	for y := 0; y < 3; y++ {
		p := Position{X: x, Y: y}
		if len(g.Deck) > 0 {
			c := g.Deck[0]
			g.Deck = g.Deck[1:]
			g.Cards[p] = c
			cs = append(cs, PlacedCard{p, c})
		}
	}
	return ChangeDeal(cs)
}

func (g *Game) columns() int {
	m := -1
	for p := range g.Cards {
		if p.X+1 > m {
			m = p.X + 1
		}
	}
	if m < 4 {
		return 4
	}
	return m
}

func (g *Game) empty(p Position) bool {
	_, ok := g.Cards[p]
	return !ok
}

func (g *Game) compact() Update {
	cols := g.columns()
	up := func(p Position) Position {
		if p.Y == 2 {
			return Position{X: p.X + 1, Y: 0}
		} else {
			return Position{X: p.X, Y: p.Y + 1}
		}
	}
	down := func(p Position) Position {
		if p.Y == 0 {
			return Position{X: p.X - 1, Y: 2}
		} else {
			return Position{X: p.X, Y: p.Y - 1}
		}
	}
	l := Position{X: 0, Y: 0}
	h := Position{X: cols - 1, Y: 2}
	var moves []Move
	for {
		for ; !g.empty(l) && l.X < cols; l = up(l) {
		}
		for ; g.empty(h) && h.X > l.X && h.X >= 4; h = down(h) {
		}
		if g.empty(l) && !g.empty(h) && h.X > l.X && h.X >= 4 {
			g.Cards[l] = g.Cards[h]
			delete(g.Cards, h)
			moves = append(moves, Move{
				From: h,
				To:   l,
			})
		} else {
			break
		}
	}
	if len(moves) == 0 {
		return nil
	}
	return ChangeMove(moves)
}

func (g *Game) deal() Update {
	var cs []PlacedCard
	for x := 0; x < 4; x++ {
		for y := 0; y < 3; y++ {
			p := Position{X: x, Y: y}
			if _, ok := g.Cards[p]; !ok {
				if len(g.Deck) > 0 {
					c := g.Deck[0]
					g.Deck = g.Deck[1:]
					g.Cards[p] = c
					cs = append(cs, PlacedCard{p, c})
				}
			}
		}
	}
	if len(cs) == 0 {
		return nil
	}
	log.Printf("dealing %d cards", len(cs))
	return ChangeDeal(cs)
}

func makeRevealCount(count int) Update {
	return nil
}

func (r *Room) loop() {
	log.Printf("starting new room: %s %s", r.creator.Game, r.creator.FirstName)
	var (
		clientId int
		clients  = map[int]*client{}
		present  = map[string]struct{}{}
		g        *Game
	)
	for {
		var updates []Update
		select {
		case cl := <-r.connects:
			cl.sendId <- clientId
			clients[clientId] = cl
			clientId++
			present[cl.blob.FirstName] = struct{}{}
			if g != nil {
				g.add(cl.blob.FirstName)
			}
			cl.updates <- makeFull(g, present)
			updates = append(updates, EventJoin{Name: cl.blob.FirstName})
		case c := <-r.cmds:
			cl := clients[c.clientId]
			switch cmd := c.command.(type) {
			case CmdDisconnect:
				log.Printf("removing client %d", c.clientId)
				close(cl.updates)
				delete(clients, c.clientId)
				// todo: clean-up present?
			case CmdStart:
				if g != nil && !g.gameover() {
					log.Printf("game in progress, ignoring start message")
					break
				}
				log.Printf("starting game on behalf of %s", cl.blob.FirstName)
				g = newGame()
				for p := range present {
					g.add(p)
				}
				updates = append(updates, makeFull(g, present), g.deal())
			case CmdClaim:
				if g == nil || g.gameover() {
					log.Printf("out of game claim: %+v", cmd)
					break
				}
				switch cmd.Type {
				case ClaimMatch:
					res, score, up := g.claimMatch(cl.blob.FirstName, cmd.Cards)
					updates = append(updates,
						up,
						g.compact(),
						g.deal(),
						EventClaimed{
							Name:   cl.blob.FirstName,
							Type:   cmd.Type,
							Result: res,
							Score:  score,
						})
					if g.gameover() {
						log.Printf("game over")
						h := &Game{
							Scores: g.Scores,
							Cards: map[Position]int{},
						}
						g = nil
						updates = append(updates, makeFull(h, present))
					}
				case ClaimNoMatch:
					res, score, up := g.claimNomatch(cl.blob.FirstName, cmd.Cards)
					updates = append(updates,
						up,
						EventClaimed{
							Name:   cl.blob.FirstName,
							Type:   cmd.Type,
							Result: res,
							Score:  score,
						})
				default:
					log.Printf("unknown claim type: %s", cmd.Type)
				}
			default:
				log.Printf("unknown command: %+v", cmd)
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

type Position struct {
	X int
	Y int
}

type Score struct {
	Match        int
	MatchWrong   int
	NoMatch      int
	NoMatchWrong int
}

type ClaimType string

const (
	ClaimMatch   ClaimType = "match"
	ClaimNoMatch ClaimType = "nomatch"
)

type ResultType string

const (
	ResultCorrect = "correct"
	ResultWrong   = "wrong"
	ResultLate    = "late"
)

type Command interface {
	isCommand()
}

type CmdDisconnect struct{}        //synthetic
func (c CmdDisconnect) isCommand() {}

type CmdStart struct{}

func (c CmdStart) isCommand() {}

type CmdClaim struct {
	Type  ClaimType
	Cards []int
}

func (c CmdClaim) isCommand() {}

type EventJoin struct {
	Name string
}

func (u EventJoin) isUpdate()   {}
func (u EventJoin) tag() string { return "eventJoin" }

type EventClaimed struct {
	Name   string
	Type   ClaimType
	Result ResultType
	Score  Score
}

func (u EventClaimed) isUpdate()   {}
func (u EventClaimed) tag() string { return "eventClaimed" }

type ChangeMatch []Position

func (u ChangeMatch) isUpdate()   {}
func (u ChangeMatch) tag() string { return "changeMatch" }

type PlacedCard struct {
	Position Position
	Card     int
}

type ChangeDeal []PlacedCard

func (u ChangeDeal) isUpdate()   {}
func (u ChangeDeal) tag() string { return "changeDeal" }

type Move struct {
	From Position
	To   Position
}

type ChangeMove []Move

func (u ChangeMove) isUpdate()   {}
func (u ChangeMove) tag() string { return "changeMove" }

type Full struct {
	Cols      int
	Rows      int
	MatchSize int
	DeckSize  int
	Cards     map[Position]int
	Scores    map[string]Score
}

func (u Full) isUpdate()   {}
func (u Full) tag() string { return "full" }

type Update interface {
	isUpdate()
	tag() string
}

func makeFull(g *Game, present map[string]struct{}) Update {
	var (
		deckSize = 0
		cards    = map[Position]int{}
		scores   = map[string]Score{}
	)
	if g == nil {
		for p := range present {
			scores[p] = Score{}
		}
	} else {
		deckSize = g.deckSize()
		scores = g.Scores
		cards = g.Cards
	}
	return Full{
		Cols:      4,
		Rows:      3,
		MatchSize: 3,
		DeckSize:  deckSize,
		Cards:     cards,
		Scores:    scores,
	}
}

func (r *Room) connect(b Blob) (<-chan Update, chan<- *cmd, <-chan int) {
	log.Printf("player connecting: %s", b.FirstName)
	updates := make(chan Update)
	sendId := make(chan int)
	r.connects <- &client{
		blob:    b,
		updates: updates,
		sendId:  sendId,
	}
	return updates, r.cmds, sendId
}

var commandTagMap edn.TagMap

func init() {
	if err := commandTagMap.AddTagStruct("triples/claim", CmdClaim{}); err != nil {
		panic(err)
	}
	if err := commandTagMap.AddTagStruct("triples/start", CmdStart{}); err != nil {
		panic(err)
	}
}

func (r *Room) Serve(b Blob, w http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	updates, cmds, getId := r.connect(b)

	go func() {
		clientId := <-getId
		defer func() {
			cmds <- &cmd{
				clientId: clientId,
				command:  CmdDisconnect{},
			}
		}()
		for {
			t, r, err := conn.NextReader()
			if err != nil {
				log.Printf("conn err: %s", err)
				return
			}
			switch t {
			case websocket.TextMessage:
				log.Printf("receiving message from %s", b.FirstName)
				d := edn.NewDecoder(r)
				d.UseTagMap(&commandTagMap)
				var c Command
				if err := d.Decode(&c); err != nil {
					log.Printf("decode err: %s", err)
					return
				}
				cmds <- &cmd{
					clientId: clientId,
					command:  c,
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

func writeUpdate(conn *websocket.Conn, u Update) error {
	w, err := conn.NextWriter(websocket.TextMessage)
	if err != nil {
		return err
	}
	defer w.Close()
	return edn.NewEncoder(w).Encode(edn.Tag{Tagname: "triples/" + u.tag(), Value: u})
}
