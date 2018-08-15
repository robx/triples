# Triples (and Quadruples)

Triples is a card game where you try to spot triples of
cards as quickly as possible.

Play it live at [triples.vllmrt.net](https://triples.vllmrt.net)
or via Telegram [@TriplesBot](https://telegram.me/TriplesBot).

The frontend is built using Elm and SVG. It should work both
on mobile and desktop. Single player works fully client-side.

There is a Go backend included that supports multiplayer games
and the Telegram integration.

## Game rules

Every card has four properties: color, count, shape, filling.

Three cards form a *triple* if, for each property by itself,
all three cards are the same or all three cards are different.
(If this seems confusing, just try hitting the hint button
at the top right.)

In the game, keep collecting triples, and more cards
will be dealt automatically. It's possible that there are no
triples to be found, in which case you can ask for extra cards
using the button at the top right. If you missed a triple, this
button will reveal the number of triples instead, and offer hints
afterwards.

If this gets boring, you can move on to quadruples. For those games,
four cards ABCD form a *quadruple* if there exists a fifth card X
such that ABX and CDX are triples.

## Deploying

Here's an nginx config fragment to make things work for the backend.

    location /triples/api/join {
        proxy_pass http://127.0.0.1:8080/api/join;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";

        # go websocket origin check
        proxy_set_header Host $host;
    }

    location /triples/ {
        proxy_pass http://127.0.0.1:8080/;
    }
