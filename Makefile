dump:
	cargo run -- dump --dump-type=ir ./examples/main.cyr ./tmp/main

run:
	cargo run -- run ./examples/main.cyr

build: 
	cargo run -- build ./examples/main.cyr ./tmp/program;