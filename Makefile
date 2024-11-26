FILENAME      ?=    examples/basic.asm

all: build

compile:
	./laser-comp -a $(FILENAME)

build:
	cargo build

run:
	cargo run

test:
	cargo test

clean:
	./laser-comp -c *

.PHONY: run build test clean
