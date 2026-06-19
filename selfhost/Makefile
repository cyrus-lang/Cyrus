COMPILER ?= ../target/debug/cyrus
STDLIB ?= ../stdlib

release:
	$(COMPILER) build --profile=release \
					    --optimize=o2 \
						--stdlib=$(STDLIB) \
						./src/main.cyrus \
						-o ./build/cyrus
					
dev:
	$(COMPILER) build -g --profile=debug \
					    --optimize=o0 \
						--stdlib=$(STDLIB) \
						./src/main.cyrus
					
	