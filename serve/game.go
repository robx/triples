package main

import (
	"log"
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
}

type client struct {
	blob    Blob
	updates chan<- *pb.Update
	claims  <-chan *pb.Claim
}

func newRoom(blob Blob) *Room {
	r := &Room{
		creator:  blob,
		connects: make(chan *client),
	}
	go r.loop()
	return r
}

func (r *Room) loop() {
	log.Printf("starting new room: %s %s", r.creator.Game, r.creator.FirstName)
	var (
		clients []*client
		msgID   uint32
	)
	for {
		var update *pb.Update
		select {
		case c := <-r.connects:
			clients = append(clients, c)
			update = makeJoin(c.blob, msgID)
			msgID += 1
		}
		if update != nil {
			log.Printf("sending message to %d clients", len(clients))
			for _, c := range clients {
				c.updates <- update
			}
		}
	}
}

func makeJoin(b Blob, msgID uint32) *pb.Update {
	return &pb.Update{
		Msgid: msgID,
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

func (r *Room) connect(b Blob) (<-chan *pb.Update, chan<- *pb.Claim) {
	log.Printf("player connecting: %s", b.FirstName)
	updates := make(chan *pb.Update)
	claims := make(chan *pb.Claim)
	r.connects <- &client{
		blob:    b,
		updates: updates,
		claims:  claims,
	}
	return updates, claims
}

func (r *Room) Serve(b Blob, w http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	updates, claims := r.connect(b)

	go func() {
		defer close(claims)
		for {
			t, m, err := conn.NextReader()
			if err != nil {
				log.Printf("conn err: %s", err)
				return
			}
			switch t {
			case websocket.TextMessage:
				log.Printf("receiving message from %s", b.FirstName)
				claim := &pb.Claim{}
				if err := jsonpb.Unmarshal(m, claim); err != nil {
					log.Printf("proto err: %s", err)
					return
				}
				claims <- claim
			default:
				log.Printf("ignoring message type: %d", t)
			}
		}
	}()

	for u := range updates {
		log.Printf("sending message to %s", b.FirstName)
		if err := writeUpdate(conn, u); err != nil {
			log.Print(err)
			return
		}
	}
	log.Print("room over")
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
