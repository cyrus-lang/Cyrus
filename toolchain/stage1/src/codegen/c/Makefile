OUTDIR ?= ./build
CC ?= cc
AR ?= ar
CFLAGS ?= -O2 -Wall -Wextra

SRCS := int128.c float16.c panic.c
OBJS := $(patsubst %.c,$(OUTDIR)/%.o,$(SRCS))
LIB := $(OUTDIR)/libcyrus_rt.a

.PHONY: all clean

all: $(LIB)

$(OUTDIR)/%.o: %.c runtime.h
	@mkdir -p $(OUTDIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(LIB): $(OBJS)
	$(AR) rcs $@ $^

clean:
	rm -rf $(OUTDIR)
