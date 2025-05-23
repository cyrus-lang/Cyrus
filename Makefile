default: run

emit-llvm:
	cargo run --jobs 16 -- emit-llvm ./examples/main.cyr -o ./tmp/main.ll

run:
	cargo run --jobs 16 -- run ./examples/main.cyr

build: 
	cargo run -- build ./examples/main.cyr ./tmp/program;

generate-sample-llvm-ir:
	clang -S -emit-llvm -O0 -Xclang -disable-O0-optnone -fno-inline ./tmp/sample.c -o ./tmp/sample.ll 

test: 
	cargo test --all

generate-internals:
	gcc ./internals/tests.c ./internals/internal_funcs.c -o ./tmp/internals_tests;