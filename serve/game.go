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
	creator Blob
}

func newRoom(blob Blob) *Room {
	r := &Room{
		creator: blob,
	}
	go r.loop()
	return r
}

func (r *Room) loop() {
	log.Printf("starting new room: %s %s", r.creator.Game, r.creator.FirstName)
}

func (r *Room) connect(b Blob) (*pb.Update, <-chan *pb.Update) {
	log.Printf("player connecting: %s", b.FirstName)
	u := &pb.Update{
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
	return u, make(chan *pb.Update)
}

func (r *Room) claim(c pb.Claim) {
	log.Printf("received claim: %+v", c)
}

func (r *Room) Serve(b Blob, w http.ResponseWriter, req *http.Request) {
	conn, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Printf("websocket upgrade: %s", err)
		return
	}
	defer conn.Close()

	claims := make(chan pb.Claim)
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
				claim := pb.Claim{}
				if err := jsonpb.Unmarshal(m, &claim); err != nil {
					log.Printf("proto err: %s", err)
					return
				}
				claims <- claim
			default:
				log.Printf("ignoring message type: %d", t)
			}
		}
	}()

	writeUpdate := func(u *pb.Update) error {
		w, err := conn.NextWriter(websocket.TextMessage)
		if err != nil {
			return err
		}
		defer w.Close()
		m := jsonpb.Marshaler{
			EmitDefaults: true,
		}
		return m.Marshal(w, u)
	}

	welcome, updates := r.connect(b)
	if err := writeUpdate(welcome); err != nil {
		log.Print(err)
		return
	}
	for {
		select {
		case u, ok := <-updates:
			if !ok {
				log.Print("game over")
				return
			}
			if err := writeUpdate(u); err != nil {
				log.Print(err)
				return
			}
		case c, ok := <-claims:
			if !ok {
				log.Print("lost connection")
				return
			}
			r.claim(c)
		}
	}
}
