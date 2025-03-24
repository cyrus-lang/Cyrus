dump:
	cargo run -- dump ./examples/main.cyr ir ./tmp/main.ll

run:
	cargo run -- run ./examples/main.cyr

build: 
	cargo run -- build ./examples/main.cyr ./tmp/program;