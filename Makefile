dump:
	cargo run -- dump ./examples/main.cyr ir ./tmp/main.ll

run:
	cargo run -- run ./examples/main.cyr

build: 
	cargo run -- build ./examples/main.cyr ./tmp/program;

generate-sample-llvm-ir:
	clang -S -emit-llvm -O0 -Xclang -disable-O0-optnone -fno-inline ./tmp/sample.c -o ./tmp/sample.ll 