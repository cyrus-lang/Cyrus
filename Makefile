dump:
	cargo run -- dump --dump-type=ir ./examples/main.cy ./tmp/main

run:
	cargo run -- run ./examples/main.cy