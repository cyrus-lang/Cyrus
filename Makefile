default: run

static-analyzer:
	cargo run -j24 -p static_analyzer -- ./examples/main.cyr $(ARGS)

emit-llvm:
	cargo run -j24 -- emit-llvm ./examples/main.cyr -o ./tmp/llvmir/ --stdlib=./stdlib $(ARGS)

parser:
	cargo run -j24 -p parser -- ./examples/main.cyr --stdlib=./stdlib $(ARGS)

run:
	cargo run -j24 -p cli -- run ./examples/main.cyr --stdlib=./stdlib --disable-warnings $(ARGS)

build:
	cargo build -j24 $(ARGS)

test: 
	cargo test -j24 --all $(ARGS)