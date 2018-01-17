.PHONY: all build format test protoc deploy

all: build

build:
	make -C client build
	make -C serve build-linux

format:
	make -C client format
	make -C serve format

test:
	make -C client test
	make -C serve test

protoc:
	protoc --elm_out=client/src proto/triples.proto
	protoc --go_out=serve proto/triples.proto

DEPLOY_DEST ?= arp:triples/

deploy:
	rsync -az static $(DEPLOY_DEST)
	rsync -z serve/serve $(DEPLOY_DEST)
