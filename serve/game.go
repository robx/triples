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

func (r *Room) loop() {
	log.Printf("starting new room: %s %s", r.creator.Game, r.creator.FirstName)
	var (
		clientId int
		clients  = map[int]*client{}
		msgId    int
	)
	for {
		var update *pb.Update
		select {
		case c := <-r.connects:
			c.sendId <- clientId
			clients[clientId] = c
			clientId++
			update = makeJoin(c.blob, msgId)
			msgId += 1
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

func makeJoin(b Blob, msgId int) *pb.Update {
	return &pb.Update{
		Msgid: int32(msgId),
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
