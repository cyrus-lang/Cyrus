.DEFAULT_GOAL := run

JOBS     ?= 24
PROFILE  ?= debug

ifeq ($(OS),Windows_NT)
	EXE = .exe
	PY_CMD = python
else
	EXE = 
	PY_CMD = python3
endif

INPUT    ?= ./tmp/main.cyrus
STDLIB   ?= ./stdlib
LLVM_OUT ?= ./tmp/llvmir
ASM_OUT ?= ./tmp/asm
CIR_DUMP_OUT ?= ./tmp/cir_dump
ARGS     ?=

TARGET_DIR = ./target/$(PROFILE)
COMPILER   = $(TARGET_DIR)/cyrus$(EXE)

ifeq ($(PROFILE),release)
	CARGO_PROFILE_FLAG = --release
else
	CARGO_PROFILE_FLAG =
endif

CARGO_RUN   = cargo run -j$(JOBS) $(CARGO_PROFILE_FLAG)
CARGO_BUILD = cargo build -j$(JOBS) $(CARGO_PROFILE_FLAG)
CARGO_TEST  = cargo test -j$(JOBS) $(CARGO_PROFILE_FLAG)

COMMON_FLAGS = $(ARGS)

.PHONY: run build test testsuite \
        cir_walk analyzer resolver parser emit-llvm

resolver:
	$(CARGO_RUN) -p cyrusc_resolver --bin cyrusc_resolver -- $(INPUT) $(COMMON_FLAGS) --stdlib=$(STDLIB)

resolver-dump:
	$(CARGO_RUN) -p cyrusc_resolver --bin cyrusc_resolver_dump -- $(INPUT) ./tmp/global_symbols_dump $(COMMON_FLAGS) --stdlib=$(STDLIB) && code ./tmp/global_symbols_dump

cir_walk parser lexer:
	$(CARGO_RUN) -p cyrusc_$@ -- $(INPUT) $(COMMON_FLAGS) --stdlib=$(STDLIB)

semantic-only:
	$(CARGO_RUN) -- semantic-only $(INPUT) --stdlib=$(STDLIB) $(ARGS)

emit-llvm:
	$(CARGO_RUN) -- emit-llvm $(INPUT) -o $(LLVM_OUT) --stdlib=$(STDLIB) $(ARGS)

emit-asm:
	$(CARGO_RUN) -- emit-asm $(INPUT) -o $(ASM_OUT) --stdlib=$(STDLIB) $(ARGS)

emit-cir-dump:
	$(CARGO_RUN) -- emit-cir-dump $(INPUT) -o $(CIR_DUMP_OUT) --stdlib=$(STDLIB) $(ARGS)

run:
	$(CARGO_RUN) -- run $(INPUT) --stdlib=$(STDLIB) $(COMMON_FLAGS)

sanitizer:
	$(CARGO_RUN) -- run $(INPUT) --sanitize=address --stdlib=$(STDLIB) $(COMMON_FLAGS)

build:
	$(CARGO_BUILD) $(ARGS)

test: build testsuite
	$(CARGO_TEST) --all $(ARGS)

testsuite: build
	$(PY_CMD) ./tests/test_suite.py \
		-d tests \
		--output ./tmp/tests \
		--compiler $(COMPILER) \
		--flags "--stdlib=$(STDLIB) --quiet --jobs 1"

testsuite-fail: build
	$(PY_CMD) ./tests/test_suite.py \
		-d tests \
		--output ./tmp/tests \
		--compiler $(COMPILER) \
		--fail \
		--flags "--stdlib=$(STDLIB) --quiet --jobs 1"