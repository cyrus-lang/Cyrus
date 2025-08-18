default: run

static-analyzer:
	cargo run -j24 -p static_analyzer -- ./examples/main.cyr 
	
emit-llvm:
	cargo run -j24 -- emit-llvm ./examples/main.cyr -o ./tmp/llvmir/ --stdlib=./stdlib --disable-modulefs-cache

parser:
	cargo run -j24 -p parser -- ./examples/main.cyr  --stdlib=./stdlib

run:
	cargo run -j24 -p cli -- run ./examples/main.cyr --stdlib=./stdlib

test: 
	cargo test -j24 --all