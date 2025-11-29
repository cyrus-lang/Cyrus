FROM debian:bookworm AS builder

ENV DEBIAN_FRONTEND=noninteractive
ENV RUSTUP_HOME=/usr/local/rustup
ENV CARGO_HOME=/usr/local/cargo
ENV PATH=$CARGO_HOME/bin:$PATH

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        llvm-18 llvm-18-dev clang \
        zlib1g zlib1g-dev libpolly-18-dev \
        curl git unzip zip ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain stable
ENV PATH=$CARGO_HOME/bin:$PATH

WORKDIR /usr/src/cyrus

COPY . .

RUN cargo build --release --verbose

FROM debian:bookworm AS runtime

RUN apt-get update && \
    apt-get install -y --no-install-recommends zlib1g bash ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /cyrus

COPY --from=builder /usr/src/cyrus/target/release/cyrus .

ENV PATH=/cyrus:$PATH

CMD ["bash"]