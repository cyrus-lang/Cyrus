default: run

cir:
	cargo run -j24 -p cyrusc_cir -- ./tmp/main.cyrus --disable-warnings $(ARGS)

sema:
	cargo run -j24 -p cyrusc_sema -- ./tmp/main.cyrus --disable-warnings $(ARGS)

resolver:
	cargo run -j24 -p cyrusc_resolver -- ./tmp/main.cyrus --disable-warnings $(ARGS)

emit-llvm:
	cargo run -j24 -- emit-llvm ./tmp/main.cyrus -o ./tmp/llvmir/ --stdlib=./stdlib $(ARGS)

parser:
	cargo run -j24 -p cyrusc_parser -- ./tmp/main.cyrus --stdlib=./stdlib $(ARGS)

run:
	cargo run -j24 -p cyrusc_cli -- run ./tmp/main.cyrus --stdlib=./stdlib --disable-warnings $(ARGS)

build:
	cargo build -j24 $(ARGS)

test: 
	cargo test -j24 --all $(ARGS)

testsuite:
	python3 ./test/test_suite.py -d test --output ./tmp/tests --compiler ./target/debug/cyrus --flags "--stdlib=./stdlib --quiet"