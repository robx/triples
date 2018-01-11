package main

import (
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"os"

	"github.com/julienschmidt/httprouter"
	"golang.org/x/crypto/nacl/secretbox"
	"gopkg.in/telegram-bot-api.v4"
)

var (
	baseURL = flag.String("base", "https://arp.vllmrt.net/triples", "base url")
	game    = flag.String("game", "triples", "Telegram game shortname")
	listen  = flag.String("listen", ":8080", "http listen")
	static  = flag.String("static", "./static/", "points to static/")
)

func main() {
	flag.Parse()

	go runBot(os.Getenv("TELEGRAM_TOKEN"), handleGame(*game, *baseURL))

	log.Printf("listening on %s...\n", *listen)
	log.Fatal(http.ListenAndServe(*listen, mux(*static)))
}

func mux(static string) *httprouter.Router {
	r := httprouter.New()
	if static != "" {
		r.GET("/", func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
			http.ServeFile(w, r, static+"index.html")
		})
		r.ServeFiles("/static/*filepath", http.Dir(static))
	}
	return r
}

func runBot(
	token string,
	callback CallbackHandler,
) {
	bot, err := tgbotapi.NewBotAPI(token)
	if err != nil {
		log.Fatalf("creating bot: %s", err)
	}

	bot.Debug = true

	log.Printf("Authorized on account %s", bot.Self.UserName)

	u := tgbotapi.NewUpdate(0)
	u.Timeout = 60

	updates, err := bot.GetUpdatesChan(u)

	for update := range updates {
		if msg := update.Message; msg != nil {
			log.Printf("message: [%s] %s", msg.From.FirstName, msg.Text)
			game := tgbotapi.GameConfig{
				BaseChat: tgbotapi.BaseChat{
					ChatID:           msg.Chat.ID,
					ReplyToMessageID: 0,
				},
				GameShortName: "triples",
			}
			bot.Send(game)
		}
		if q := update.CallbackQuery; q != nil {
			if cc := callback(q); cc != nil {
				if _, err := bot.AnswerCallbackQuery(*cc); err != nil {
					log.Printf("answer callback: %s", err)
				}
			}
		}
	}
}

type CallbackHandler func(*tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig

type Blob struct {
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
	return base64.URLEncoding.EncodeToString(bs)
}

func decode(s string) (Blob, error) {
	bs, err := base64.URLEncoding.DecodeString(s)
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

func handleGame(shortname, url string) CallbackHandler {
	return func(q *tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig {
		if g := q.GameShortName; g == shortname {
			b := Blob{
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
			return &tgbotapi.CallbackConfig{
				CallbackQueryID: q.ID,
				URL:             url + "?key=" + key,
			}
		}
		return nil
	}
}
