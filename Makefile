default: run

emit-llvm:
	cargo run -j24 -- emit-llvm ./examples/main.cyr -o ./tmp/main.ll --stdlib=./stdlib

parser:
	cargo run -j24 -p parser -- ./examples/main.cyr  --stdlib=./stdlib

run:
	cargo run -j24 -- run ./examples/main.cyr --stdlib=./stdlib

test: 
	cargo test -j24 --all