default: run

emit-llvm:
	cargo run -j24 -- emit-llvm ./examples/main.cyr -o ./tmp/main.ll

parser:
	cargo run -j24 -p parser -- ./examples/main.cyr 

run:
	cargo run -j24 -- run ./examples/main.cyr

test: 
	cargo test -j24 --all