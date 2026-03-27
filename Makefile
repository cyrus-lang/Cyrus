.DEFAULT_GOAL := run

JOBS     ?= 24
PROFILE  ?= debug

INPUT    ?= ./tmp/main.cyrus
STDLIB   ?= ./stdlib
LLVM_OUT ?= ./tmp/llvmir
ASM_OUT ?= ./tmp/asm
ARGS     ?=

TARGET_DIR = ./target/$(PROFILE)
COMPILER   = $(TARGET_DIR)/cyrus

ifeq ($(PROFILE),release)
	CARGO_PROFILE_FLAG = --release
else
	CARGO_PROFILE_FLAG =
endif

CARGO_RUN   = cargo run -j$(JOBS) $(CARGO_PROFILE_FLAG)
CARGO_BUILD = cargo build -j$(JOBS) $(CARGO_PROFILE_FLAG)
CARGO_TEST  = cargo test -j$(JOBS) $(CARGO_PROFILE_FLAG)

COMMON_FLAGS = --disable-warnings $(ARGS)

.PHONY: run build test testsuite \
        cir_walk analyzer resolver parser emit-llvm

resolver:
	$(CARGO_RUN) -p cyrusc_resolver --bin cyrusc_resolver -- $(INPUT) $(COMMON_FLAGS) --stdlib=$(STDLIB)

resolver_dump_global_symbols:
	$(CARGO_RUN) -p cyrusc_resolver --bin cyrusc_resolver_debugger -- $(INPUT) ./tmp/global_symbols_dump $(COMMON_FLAGS) --stdlib=$(STDLIB) && code ./tmp/global_symbols_dump

cir_walk analyzer parser lexer:
	$(CARGO_RUN) -p cyrusc_$@ -- $(INPUT) $(COMMON_FLAGS) --stdlib=$(STDLIB)

emit-llvm:
	$(CARGO_RUN) -- emit-llvm $(INPUT) -o $(LLVM_OUT) --stdlib=$(STDLIB) $(ARGS)

emit-asm:
	$(CARGO_RUN) -- emit-asm $(INPUT) -o $(ASM_OUT) --stdlib=$(STDLIB) $(ARGS)

run:
	$(CARGO_RUN) -- run $(INPUT) --stdlib=$(STDLIB) $(COMMON_FLAGS)

sanitizer:
	$(CARGO_RUN) -- run $(INPUT) --sanitize=address --stdlib=$(STDLIB) $(COMMON_FLAGS)

build:
	$(CARGO_BUILD) $(ARGS)

test: build testsuite
	$(CARGO_TEST) --all $(ARGS)

testsuite:
	python3 ./tests/test_suite.py \
		-d tests \
		--output ./tmp/tests \
		--compiler $(COMPILER) \
		--flags "--stdlib=$(STDLIB) --quiet"