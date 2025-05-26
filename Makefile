default: run

emit-llvm:
	cargo run --jobs 16 -- emit-llvm ./examples/main.cyr -o ./tmp/main.ll

run:
	cargo run --jobs 16 -- run ./examples/main.cyr

test: 
	cargo test --jobs 16 --all