COMPILER ?= ../target/debug/cyrus
STDLIB ?= ../stdlib

release:
	$(COMPILER) build --profile=release \
						--stdlib=$(STDLIB) \
						--linker=clang \
					    --optimize=o2 \
						-z="-flto" \
						--jobs 1

debug:
	$(COMPILER) build --profile=debug \
						--sanitize=address -g \
						--stdlib=$(STDLIB) \
						--linker=clang \
					    --optimize=o0 \
						--jobs 1
	