.PHONY: all test build build-linux format

all: test build

test:
	go test

build:
	go build

build-linux:
	GOOS=linux CGO_ENABLED=0 go build

format:
	gofmt -w .
