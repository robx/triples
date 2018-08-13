package main

import (
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"

	"github.com/julienschmidt/httprouter"
	"github.com/robx/telegram-bot-api"
	"golang.org/x/crypto/nacl/secretbox"
)

var (
	listen   = flag.String("listen", ":8080", "http listen")
	static   = flag.String("static", "./static/", "points to static/")
	debugbot = flag.Bool("debugbot", false, "debug logs for the Telegram bot")
	baseURL  = flag.String("base", "https://arp.vllmrt.net/triples", "http base URL")
	bot      = flag.Bool("bot", true, "run the telegram bot")
)

var (
	games = []string{
		"triples",
		"quadruples",
		"triplessprint",
		"quadruplessprint",
		"triplesmulti",
		"quadruplesmulti",
	}
)

func main() {
	flag.Parse()

	actions := make(chan BotAction)

	if *bot {
		var callbacks []CallbackHandler
		for _, g := range games {
			callbacks = append(callbacks, handleGame(g, *baseURL+"?scored=1&game="+g))
		}
		go runBot(os.Getenv("TELEGRAM_TOKEN"), callbacks, actions)
	}

	log.Printf("listening on %s...\n", *listen)
	log.Fatal(http.ListenAndServe(*listen, mux(actions, *static)))
}

func mux(actions chan<- BotAction, static string) *httprouter.Router {
	r := httprouter.New()
	if static != "" {
		r.GET("/", func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
			http.ServeFile(w, r, static+"index.html")
		})
		r.ServeFiles("/static/*filepath", http.Dir(static))
	}
	r.GET("/api/win", winHandler(actions))
	r.GET("/api/join", multiHandler(newRooms()))
	r.GET("/api/new", newHandler())
	return r
}

func runBot(
	token string,
	callbacks []CallbackHandler,
	actions <-chan BotAction,
) {
	bot, err := tgbotapi.NewBotAPI(token)
	if err != nil {
		log.Fatalf("creating bot: %s", err)
	}

	bot.Debug = *debugbot

	log.Printf("Authorized on account %s", bot.Self.UserName)

	u := tgbotapi.NewUpdate(0)
	u.Timeout = 60

	updates, err := bot.GetUpdatesChan(u)

	for {
		select {
		case update := <-updates:
			handleUpdate(bot, callbacks, update)
		case action := <-actions:
			action(bot)
		}
	}
}

type BotAction func(*tgbotapi.BotAPI)

func handleUpdate(bot *tgbotapi.BotAPI, callbacks []CallbackHandler, update tgbotapi.Update) {
	if m := update.Message; m != nil {
		if t := m.Text; len(t) > 0 && t[0] == '/' {
			words := strings.Fields(t)
			if len(words) == 2 && words[0] == "/send" {
				log.Printf("answering /send: %s", words[1])
				found := false
				for _, g := range games {
					if words[1] == g {
						cfg := tgbotapi.GameConfig{
							BaseChat: tgbotapi.BaseChat{
								ChatID:           m.Chat.ID,
								ReplyToMessageID: 0,
							},
							GameShortName: words[1],
						}
						bot.Send(cfg)
						found = true
						break
					}
				}
				if !found {
					msg := tgbotapi.NewMessage(m.Chat.ID, "I don't know that game")
					bot.Send(msg)
				}
			}
		} else {
			log.Printf("ignoring non-command message: %s", m.Text)
		}
	}
	if q := update.InlineQuery; q != nil {
		log.Printf("answering inline query: %+v", games)
		var results []interface{}
		for i, g := range games {
			results = append(results,
				tgbotapi.InlineQueryResultGame{
					Type:          "game",
					ID:            strconv.Itoa(i),
					GameShortName: g,
				})
		}
		ic := tgbotapi.InlineConfig{
			InlineQueryID: q.ID,
			Results:       results,
		}
		_, err := bot.AnswerInlineQuery(ic)
		if err != nil {
			log.Printf("error answering inline query: %v", err)
		}
	}
	if q := update.CallbackQuery; q != nil {
		for _, c := range callbacks {
			if cc := c(q); cc != nil {
				if _, err := bot.AnswerCallbackQuery(*cc); err != nil {
					log.Printf("answer callback: %s", err)
				}
				break
			}
		}
	}
}

type CallbackHandler func(*tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig

type Blob struct {
	Game            string `json:"g,omitempty"`
	UserID          int    `json:"uid,omitempty"`
	FirstName       string `json:"fst,omitempty"`
	ChatInstance    string `json:"cin,omitempty"`
	ChatID          int64  `json:"cid,omitempty"`
	MessageID       int    `json:"mid,omitempty"`
	InlineMessageID string `json:"iid,omitempty"`
}

var (
	key = [32]byte{0x49, 0xf3, 0xae, 0x3f, 0x82, 0x26, 0x72, 0x6d, 0xf4, 0x5c, 0xf4, 0x3c, 0x36, 0x66, 0x12, 0xdf, 0x8a, 0xc1, 0x2b, 0xe9, 0x94, 0x87, 0x92, 0x47, 0x8e, 0xfa, 0xcf, 0xb9, 0xcc, 0x77, 0xf7, 0x3d}
)

func encode(b Blob) string {
	js, err := json.Marshal(b)
	if err != nil {
		panic(err)
	}
	var (
		nonce [24]byte
	)
	_, err = rand.Read(nonce[:])
	if err != nil {
		panic(err)
	}
	bs := secretbox.Seal(nonce[:], js, &nonce, &key)
	return base64.RawURLEncoding.EncodeToString(bs)
}

func decode(s string) (Blob, error) {
	bs, err := base64.RawURLEncoding.DecodeString(s)
	if err != nil {
		return Blob{}, err
	}
	var nonce [24]byte
	copy(nonce[:], bs)
	box := bs[24:]
	var out []byte
	js, ok := secretbox.Open(out, box, &nonce, &key)
	if !ok {
		return Blob{}, fmt.Errorf("bad blob")
	}
	var b Blob
	return b, json.Unmarshal(js, &b)
}

func handleGame(shortname, u string) CallbackHandler {
	return func(q *tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig {
		if g := q.GameShortName; g != shortname {
			return nil
		}

		b := Blob{
			Game:            shortname,
			UserID:          q.From.ID,
			FirstName:       q.From.FirstName,
			InlineMessageID: q.InlineMessageID,
			ChatInstance:    q.ChatInstance,
		}
		if msg := q.Message; msg != nil {
			b.MessageID = msg.MessageID
			b.ChatID = msg.Chat.ID
		}
		key := encode(b)
		var v = url.Values{}
		v.Add("key", key)
		v.Add("name", q.From.FirstName)
		log.Printf("game callback: %s: %+v", shortname, b)
		return &tgbotapi.CallbackConfig{
			CallbackQueryID: q.ID,
			URL:             u + "&" + v.Encode(),
		}
	}
}

func sendScore(blob Blob, score int) BotAction {
	sc := tgbotapi.SetGameScoreConfig{
		UserID:          blob.UserID,
		Score:           score,
		ChatID:          blob.ChatID,
		MessageID:       blob.MessageID,
		InlineMessageID: blob.InlineMessageID,
	}
	return func(bot *tgbotapi.BotAPI) {
		if _, err := bot.Send(sc); err != nil {
			log.Printf("send score %s=%d: %s", blob.FirstName, score, err)
		} else {
			log.Printf("sent score %s=%d", blob.FirstName, score)
		}
	}
}

func winHandler(actions chan<- BotAction) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
		key := r.FormValue("key")
		if key == "" {
			http.Error(w, "missing parameter `key`", http.StatusBadRequest)
			return
		}
		s, err := strconv.Atoi(r.FormValue("score"))
		if err != nil {
			http.Error(w, "missing/bad parameter `score`", http.StatusBadRequest)
			return
		}
		blob, err := decode(key)
		if err != nil {
			log.Printf("decoding blob %q: %s", key, err)
			http.Error(w, "bad key", http.StatusBadRequest)
			return
		}
		actions <- sendScore(blob, s)
	}
}

func multiHandler(rooms *Rooms) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
		key := r.FormValue("key")
		if key == "" {
			http.Error(w, "missing parameter `key`", http.StatusBadRequest)
			return
		}
		name := r.FormValue("name")
		if name == "" {
			http.Error(w, "missing parameter `name`", http.StatusBadRequest)
			return
		}
		blob, err := decode(key)
		if err != nil {
			log.Printf("decoding blob %q: %s", key, err)
			http.Error(w, "bad key", http.StatusBadRequest)
			return
		}
		rooms.Get(blob).Serve(blob, name, w, r)
	}
}

func randHex(n int) string {
	bs := make([]byte, n)
	_, err := rand.Read(bs)
	if err != nil {
		panic(err)
	}
	var s string
	for _, b := range bs {
		s += fmt.Sprintf("%02x", b)
	}
	return s
}

func newHandler() httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
		game := r.FormValue("game")
		if game == "" {
			http.Error(w, "missing parameter `game`", http.StatusBadRequest)
			return
		}
		b := Blob{
			Game:         game,
			ChatInstance: randHex(50),
		}
		key := encode(b)
		io.WriteString(w, key)
	}
}
