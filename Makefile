DEFAULTDIR    ?=    examples/
FILENAME      ?=    basic.asm
PATH          = $(DEFAULTDIR)$(FILENAME)

all: build

compile:
	./laser-comp -a $(PATH)

build:
	cargo build

run:
	cargo run

test:
	cargo test

clean:
	./laser-comp -c examples/*

.PHONY: run build test clean
