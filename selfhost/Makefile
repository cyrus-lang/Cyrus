COMPILER ?= ../target/debug/cyrus
STDLIB ?= ../stdlib

release:
	$(COMPILER) build --profile=release \
						--stdlib=$(STDLIB) \
						./src/main.cyrus \
						--linker=clang \
					    --optimize=o2 \
						-z="-flto" \
						-o ./build/cyrus
					
dev:
	$(COMPILER) build -g --profile=debug \
						--stdlib=$(STDLIB) \
						--linker=clang \
					    --optimize=o0 \
						./src/main.cyrus
					
	