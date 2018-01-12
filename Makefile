.PHONY: all build build-elm build-web format test deploy deploy-elm deploy-web

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

deploy-elm:
	rsync -az static $(DEPLOY_DEST)

deploy-web:
	rsync -z web/web $(DEPLOY_DEST)

deploy: deploy-elm deploy-web
