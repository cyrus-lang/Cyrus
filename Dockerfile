FROM rust:1.84

RUN apt install -y build-essentials software-properties-common llvm-19 llvm-19-dev clang zlib1g zlib1g-dev

WORKDIR /build

COPY . /build

RUN cargo build --release && \
    mkdir cyrus && \
    cp ./target/release/cyrus ./cyrus && \
    cp -r ./stdlib ./cyrus
