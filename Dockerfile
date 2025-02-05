FROM rust:1.84-alpine3.20

ENV CARGO_HOME=/root/.cargo
ENV RUSTUP_HOME=/root/.rustup

RUN apk update && apk add --no-cache \
    gcc \
    make \
    musl-dev \
    libc-dev \
    curl \
    bash \
    binutils-gold \
    clang \
    libgcc \
    libstdc++ \
    zlib-dev

WORKDIR /app

COPY . .

RUN mkdir -p tmp && touch tmp/main

RUN rustup default stable

RUN make

ENTRYPOINT ["/bin/sh"]
