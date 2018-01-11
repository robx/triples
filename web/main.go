package main

import (
	"flag"
	"log"
	"net/http"
	"os"

	"github.com/julienschmidt/httprouter"
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

func handleGame(shortname, url string) CallbackHandler {
	return func(q *tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig {
		if g := q.GameShortName; g == shortname {
			return &tgbotapi.CallbackConfig{
				CallbackQueryID: q.ID,
				URL:             url,
			}
		}
		return nil
	}
}
