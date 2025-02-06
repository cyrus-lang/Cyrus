FROM rust:1.84.1-bookworm

ENV DEBIAN_FRONTEND=noninteractive 

RUN apt update && apt install -y \
    build-essential \
    gcc \
    libgccjit-12-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY ../ .

RUN cargo build --release

CMD ["./target/release/cyrus"]
