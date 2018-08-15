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

const (
	closeDelay = 30 * time.Second
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
	rooms map[[2]string]*Room
}

func newRooms() *Rooms {
	return &Rooms{
		rooms: map[[2]string]*Room{},
	}
}

func (rs *Rooms) Serve(game, room, name string, w http.ResponseWriter, req *http.Request) {
	r := rs.get(game, room)
	r.Serve(name, w, req)
	rs.release(game, room)
}

func (rs *Rooms) get(game, room string) *Room {
	key := [2]string{game, room}
	rs.mu.Lock()
	defer rs.mu.Unlock()
	if _, ok := rs.rooms[key]; !ok {
		rs.rooms[key] = newRoom(game, room)
	}
	rs.rooms[key].count += 1
	return rs.rooms[key]
}

func (rs *Rooms) release(game, room string) {
	key := [2]string{game, room}
	rs.mu.Lock()
	defer rs.mu.Unlock()
	rm := rs.rooms[key]
	if rm == nil {
		return
	}
	rm.count -= 1
	if rm.count <= 0 {
		go rs.maybeClose(key)
	}
}

// maybeClose closes an empty room after a delay,
// assuming it is still empty (or accidentally again empty).
func (rs *Rooms) maybeClose(key [2]string) {
	time.Sleep(closeDelay)
	rs.mu.Lock()
	defer rs.mu.Unlock()
	rm := rs.rooms[key]
	if rm == nil {
		return
	}
	if rm.count > 0 {
		return
	}
	delete(rs.rooms, key)
	rm.close()
}

type Room struct {
	game     int
	room     string
	quit     chan struct{}
	connects chan *client
	cmds     chan *cmd
	count    int
}

type cmd struct {
	clientId int
	command  Command
}

type client struct {
	name    string
	updates chan<- Update
	sendId  chan<- int
}

func (c client) Name() string {
	return c.name
}

func newRoom(game, room string) *Room {
	gm := GameTriples
	if game == "quadruplesmulti" {
		gm = GameQuadruples
	}
	r := &Room{
		game:     gm,
		room:     room,
		quit:     make(chan struct{}),
		connects: make(chan *client),
		cmds:     make(chan *cmd),
	}
	go r.loop()
	return r
}

const (
	GameTriples    = 0
	GameQuadruples = 1
)

var (
	defaultColumns = [2]int{4, 3}
	matchSize      = [2]int{3, 4}
)

type Game struct {
	Type           int
	DefaultColumns int
	Deck           []int
	Cards          map[Position]int
	Scores         map[string]int
	ClaimedNoMatch bool
}

func newGame(gameType int) *Game {
	return &Game{
		Type:           gameType,
		DefaultColumns: defaultColumns[gameType],
		Deck:           rand.Perm(81),
		Cards:          map[Position]int{},
		Scores:         map[string]int{},
	}
}

func (g *Game) deckSize() int {
	return len(g.Deck)
}

func (g *Game) add(player string) {
	g.Scores[player] = g.Scores[player]
}

func (g *Game) findCard(c int) (Position, bool) {
	for p, cc := range g.Cards {
		if cc == c {
			return p, true
		}
	}
	return Position{}, false
}

func isTriple(x, y, z int) bool {
	for i := 0; i < 4; i++ {
		if (x+y+z)%3 != 0 {
			return false
		}
		x /= 3
		y /= 3
		z /= 3
	}
	return true
}

func isQuadruple(x, y, z, w int) bool {
	missing := func(a, b int) int {
		c := 0
		f := 1
		for i := 0; i < 4; i++ {
			c += f * ((-a - b) % 3)
			f *= 3
		}
		return c
	}
	return missing(x, y) == missing(z, w) || missing(x, z) == missing(y, w) || missing(x, w) == missing(y, z)
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
	switch g.Type {
	case GameQuadruples:
		for i := 0; i < len(cards); i++ {
			for j := i + 1; j < len(cards); j++ {
				for k := j + 1; k < len(cards); k++ {
					for l := k + 1; l < len(cards); l++ {
						if isQuadruple(cards[i], cards[j], cards[k], cards[l]) {
							count++
						}
					}
				}
			}
		}
	default:
		for i := 0; i < len(cards); i++ {
			for j := i + 1; j < len(cards); j++ {
				for k := j + 1; k < len(cards); k++ {
					if isTriple(cards[i], cards[j], cards[k]) {
						count++
					}
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

func (g *Game) isMatch(cards []int) bool {
	switch g.Type {
	case GameQuadruples:
		return len(cards) == 4 && isQuadruple(cards[0], cards[1], cards[2], cards[3])
	default:
		return len(cards) == 3 && isTriple(cards[0], cards[1], cards[2])
	}
}

func (g *Game) claimMatch(name string, cards []int) (ResultType, int, Update) {
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
	if g.isMatch(cards) {
		for _, p := range ps {
			delete(g.Cards, p)
		}
		g.Scores[name] += 1
		return ResultCorrect, g.Scores[name], ChangeMatch(ps)
	}
	g.Scores[name] -= 1
	return ResultWrong, g.Scores[name], nil
}

func (g *Game) claimNomatch(name string, cards []int) (ResultType, int, Update) {
	if g.gameover() || len(cards) < 3*g.DefaultColumns || g.ClaimedNoMatch {
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
		g.Scores[name] += 1
		return ResultCorrect, g.Scores[name], g.dealMore()
	}
	log.Printf("wrong nomatch claim, %d matches, these cards %+v", c, cards)
	g.ClaimedNoMatch = true
	g.Scores[name] -= 1
	return ResultWrong, g.Scores[name], makeRevealCount(c)
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
	g.ClaimedNoMatch = false
	return ChangeDeal(cs)
}

func (g *Game) columns() int {
	m := -1
	for p := range g.Cards {
		if p.X+1 > m {
			m = p.X + 1
		}
	}
	if dc := g.DefaultColumns; m < dc {
		return dc
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
		for ; g.empty(h) && h.X > l.X && h.X >= g.DefaultColumns; h = down(h) {
		}
		if g.empty(l) && !g.empty(h) && h.X > l.X && h.X >= g.DefaultColumns {
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
	for x := 0; x < g.DefaultColumns; x++ {
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
	g.ClaimedNoMatch = false
	return ChangeDeal(cs)
}

func makeRevealCount(count int) Update {
	return nil
}

func (r *Room) loop() {
	log.Printf("starting new room: %d %s", r.game, r.room)
	var (
		clientId int
		clients  = map[int]*client{}
		g        *Game
	)
	present := func() map[string]struct{} {
		p := map[string]struct{}{}
		for _, c := range clients {
			p[c.Name()] = struct{}{}
		}
		return p
	}
	sendAfter := func(u Update, after time.Duration) {
		if u == nil {
			return
		}
		time.Sleep(after)
		for _, c := range clients {
			c.updates <- u
		}
	}
	send := func(u Update) {
		sendAfter(u, 0)
	}
	for {
		select {
		case <-r.quit:
			for _, cl := range clients {
				close(cl.updates)
			}
			return
		case cl := <-r.connects:
			cl.sendId <- clientId
			_, alreadyThere := present()[cl.Name()]
			clients[clientId] = cl
			clientId++
			if g != nil {
				g.add(cl.Name())
			}
			cl.updates <- makeFull(g, r.game, present())
			if !alreadyThere {
				send(EventOnline{Name: cl.Name(), Present: true})
			}
		case c := <-r.cmds:
			cl := clients[c.clientId]
			switch cmd := c.command.(type) {
			case CmdDisconnect:
				log.Printf("removing client %d", c.clientId)
				close(cl.updates)
				delete(clients, c.clientId)
				if _, ok := present()[cl.Name()]; !ok {
					send(EventOnline{Name: cl.Name(), Present: false})
				}
			case CmdStart:
				if g != nil && !g.gameover() {
					log.Printf("game in progress, ignoring start message")
					break
				}
				log.Printf("starting game on behalf of %s", cl.Name())
				g = newGame(r.game)
				ps := present()
				for p := range ps {
					g.add(p)
				}
				send(makeFull(g, r.game, ps))
				sendAfter(g.deal(), 250*time.Millisecond)
			case CmdClaim:
				if g == nil || g.gameover() {
					log.Printf("out of game claim: %+v", cmd)
					break
				}
				switch cmd.Type {
				case ClaimMatch:
					res, score, up := g.claimMatch(cl.Name(), cmd.Cards)
					send(up)
					send(EventClaimed{
						Name:   cl.Name(),
						Type:   cmd.Type,
						Result: res,
						Score:  score,
					})
					gameover := func() {
						log.Printf("game over")
						h := &Game{
							Scores: g.Scores,
							Cards:  map[Position]int{},
						}
						g = nil
						sendAfter(makeFull(h, r.game, present()), 250*time.Millisecond)
					}
					if g.gameover() {
						gameover()
					} else {
						sendAfter(g.compact(), 250*time.Millisecond)
						sendAfter(g.deal(), 250*time.Millisecond)
						if g.gameover() {
							gameover()
						}
					}
				case ClaimNoMatch:
					res, score, upd := g.claimNomatch(cl.Name(), cmd.Cards)
					send(upd)
					send(EventClaimed{
						Name:   cl.Name(),
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
	}
}

type Position struct {
	X int
	Y int
}

type Status struct {
	Present bool
	Score   int
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

type EventOnline struct {
	Present bool
	Name    string
}

func (u EventOnline) isUpdate()   {}
func (u EventOnline) tag() string { return "eventOnline" }

type EventClaimed struct {
	Name   string
	Type   ClaimType
	Result ResultType
	Score  int
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
	Players   map[string]Status
}

func (u Full) isUpdate()   {}
func (u Full) tag() string { return "full" }

type Update interface {
	isUpdate()
	tag() string
}

func makeFull(g *Game, typ int, present map[string]struct{}) Update {
	var (
		deckSize = 0
		cards    = map[Position]int{}
		players  = map[string]Status{}
	)
	if g == nil {
		for p := range present {
			players[p] = Status{Present: true}
		}
	} else {
		deckSize = g.deckSize()
		for p, s := range g.Scores {
			_, ok := present[p]
			players[p] = Status{Present: ok, Score: s}
		}
		cards = g.Cards
		typ = g.Type
	}
	return Full{
		Cols:      defaultColumns[typ],
		Rows:      3,
		MatchSize: matchSize[typ],
		DeckSize:  deckSize,
		Cards:     cards,
		Players:   players,
	}
}

func (r *Room) close() {
	log.Printf("closing room: %s", r.room)
	close(r.quit)
}

func (r *Room) connect(name string) (<-chan Update, chan<- *cmd, <-chan int) {
	log.Printf("player connecting: %s", name)
	updates := make(chan Update)
	sendId := make(chan int)
	r.connects <- &client{
		name:    name,
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

func (r *Room) Serve(name string, w http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	updates, cmds, getId := r.connect(name)

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
				log.Printf("receiving message from %s", name)
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
		log.Printf("sending message to %s", name)
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
