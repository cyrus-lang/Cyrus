FROM alpine:latest

RUN apk add --no-cache rust cargo gcc make libc-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

RUN apk add --no-cache libgcc libstdc++ musl-dev

RUN ln -sf /usr/bin/gcc /usr/bin/cc && \
    ln -sf /usr/bin/gcc /usr/bin/gcc && \
    ln -sf /usr/bin/g++ /usr/bin/g++

RUN export CC=gcc

RUN ~/.cargo/bin/rustup target add x86_64-unknown-linux-musl

WORKDIR /app

COPY . .

RUN mkdir -p tmp && touch tmp/main

RUN make

CMD ["/bin/sh"]
