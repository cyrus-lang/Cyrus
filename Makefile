default: run

static-analyzer:
	cargo run -j24 -p static_analyzer -- ./tmp/main.cyr $(ARGS)

emit-llvm:
	cargo run -j24 -- emit-llvm ./tmp/main.cyr -o ./tmp/llvmir/ --stdlib=./stdlib $(ARGS)

parser:
	cargo run -j24 -p parser -- ./tmp/main.cyr --stdlib=./stdlib $(ARGS)

run:
	cargo run -j24 -p cli -- run ./tmp/main.cyr --stdlib=./stdlib --disable-warnings $(ARGS)

build:
	cargo build -j24 $(ARGS)

test: 
	cargo test -j24 --all $(ARGS)

testsuite:
	python3 ./test/test_suite.py -d test --output ./tmp/tests --compiler ./target/debug/cyrus --flags "--stdlib=./stdlib --quiet"