package main

import (
	"flag"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"

	"github.com/julienschmidt/httprouter"
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
	}
	multigames = []string{
		"triplesmulti",
		"quadruplesmulti",
	}
)

func main() {
	flag.Parse()

	var score ScoreHandler
	if *bot {
		score = startBot(os.Getenv("TELEGRAM_TOKEN"))
	}

	log.Printf("listening on %s...\n", *listen)
	log.Fatal(http.ListenAndServe(*listen, mux(*static, score)))
}

func mux(static string, score ScoreHandler) *httprouter.Router {
	r := httprouter.New()
	if static != "" {
		if !strings.HasSuffix(static, "/") {
			static += "/"
		}
		r.GET("/", func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
			http.ServeFile(w, r, static+"index.html")
		})
		r.ServeFiles("/static/*filepath", http.Dir(static))
	}
	if score != nil {
		r.GET("/api/win", winHandler(score))
	}
	r.GET("/api/join", multiHandler(newRooms()))
	return r
}

func winHandler(handleScore ScoreHandler) httprouter.Handle {
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
		if err := handleScore(key, s); err != nil {
			log.Print(err)
			http.Error(w, "bad key", http.StatusBadRequest)
		}

	}
}

func multiHandler(rooms *Rooms) httprouter.Handle {
	return func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
		room := r.FormValue("room")
		if room == "" {
			http.Error(w, "missing parameter `room`", http.StatusBadRequest)
			return
		}
		game := r.FormValue("game")
		if game == "" {
			http.Error(w, "missing parameter `game`", http.StatusBadRequest)
			return
		}
		name := r.FormValue("name")
		if name == "" {
			http.Error(w, "missing parameter `name`", http.StatusBadRequest)
			return
		}
		rooms.Serve(game, room, name, w, r)
	}
}
