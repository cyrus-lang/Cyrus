.DEFAULT_GOAL := run

JOBS     ?= 24
PROFILE  ?= debug

INPUT    ?= ./tmp/main.cyrus
STDLIB   ?= ./stdlib
LLVM_OUT ?= ./tmp/llvmir
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
        cir sema resolver parser emit-llvm

cir sema resolver parser:
	$(CARGO_RUN) -p cyrusc_$@ -- $(INPUT) $(COMMON_FLAGS) --stdlib=$(STDLIB)

emit-llvm:
	$(CARGO_RUN) -- emit-llvm $(INPUT) -o $(LLVM_OUT) --stdlib=$(STDLIB) $(ARGS)

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