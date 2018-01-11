.PHONY: all build build-elm build-web format test deploy

all: build

build-elm:
	elm-make src/Main.elm --yes --output ./static/Main.js

build-web:
	make -C web build-linux

build: build-elm build-web

format:
	elm-format --yes src tests
	make -C web format

test:
	elm test
	make -C web test


DEPLOY_DEST ?= arp:triples/

deploy:
	rsync -z web/web $(DEPLOY_DEST)
	rsync -az static $(DEPLOY_DEST)
