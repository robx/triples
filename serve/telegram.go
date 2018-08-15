package main

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/url"
	"strconv"
	"strings"

	"github.com/robx/telegram-bot-api"
	"golang.org/x/crypto/nacl/secretbox"
)

func startBot(token string) ScoreHandler {
	var (
		blobKey   = genKey()
		actions   = make(chan BotAction)
		callbacks []CallbackHandler
	)

	for _, g := range games {
		callbacks = append(callbacks, handleGame(g, *baseURL, blobKey))
	}
	for _, g := range multigames {
		callbacks = append(callbacks, handleMultiGame(g, *baseURL))
	}
	go runBot(token, callbacks, actions)

	return handleScore(actions, blobKey)
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

func encode(b Blob, key [32]byte) string {
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

func decode(s string, key [32]byte) (Blob, error) {
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

func genKey() [32]byte {
	var key [32]byte
	_, err := rand.Read(key[:])
	if err != nil {
		panic(err)
	}
	return key
}

func handleGame(shortname, u string, key [32]byte) CallbackHandler {
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
		key := encode(b, key)
		var v = url.Values{}
		v.Add("game", shortname)
		v.Add("scored", "1")
		v.Add("key", key)
		v.Add("name", q.From.FirstName)
		log.Printf("game callback: %s: %+v", shortname, b)
		return &tgbotapi.CallbackConfig{
			CallbackQueryID: q.ID,
			URL:             u + "?" + v.Encode(),
		}
	}
}

func handleMultiGame(shortname, u string) CallbackHandler {
	return func(q *tgbotapi.CallbackQuery) *tgbotapi.CallbackConfig {
		if g := q.GameShortName; g != shortname {
			return nil
		}

		room := base64.RawURLEncoding.EncodeToString([]byte(q.ChatInstance))

		var v = url.Values{}
		v.Add("game", shortname)
		v.Add("room", room)
		v.Add("name", q.From.FirstName)
		log.Printf("multi game callback: %s", shortname)
		return &tgbotapi.CallbackConfig{
			CallbackQueryID: q.ID,
			URL:             u + "?" + v.Encode(),
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

type ScoreHandler func(string, int) error

func handleScore(actions chan<- BotAction, blobKey [32]byte) ScoreHandler {
	return func(key string, score int) error {
		blob, err := decode(key, blobKey)
		if err != nil {
			return fmt.Errorf("decoding blob %q: %s", blob, err)
		}
		actions <- sendScore(blob, score)
		return nil
	}
}
