# Thin wrapper kept for muscle memory — the real driver is ../x.py at the repo root.
# Prefer: ./x.py build --stage stage1   (from the repository root)

COMPILER ?= ../stage0/target/debug/cyrus
STDLIB   ?= ../../lib/std

release:
	$(COMPILER) build --profile=release \
					--stdlib=$(STDLIB) \
					--linker=clang \
					--optimize=o2 \
					-z="-flto" \
					--module-merge-mode=separate --disable-modulefs-cache

debug:
	$(COMPILER) build --profile=debug \
					--sanitize=address -g \
					--stdlib=$(STDLIB) \
					--linker=clang \
					--optimize=o0 \
					--module-merge-mode=separate --disable-modulefs-cache

clean:
	rm -rf ./build

.PHONY: release debug clean
