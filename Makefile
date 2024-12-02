DEFAULTDIR    ?=    examples/
FILENAME      ?=    basic.asm
PATH          = $(DEFAULTDIR)$(FILENAME)

DEBUGMODE     ?= t

all: build

compile:
	./laser-comp -a $(PATH)

build:
	make -C laser/src
	cargo build

run: build
	cargo run $(FILENAME) $(DEBUGMODE)

test:
	cargo test

clean:
	./laser-comp -c examples/*

.PHONY: run build test clean
