FROM rust:1.84.1-bookworm

RUN echo "deb http://deb.debian.org/debian testing main" | tee /etc/apt/sources.list.d/testing.list

RUN apt update && apt install -y \
    build-essential \
    gcc \
    make \
    git \
    wget \
    gcc-14 \
    libgccjit-14-dev

WORKDIR /app

COPY . .

RUN mkdir -p tmp
RUN echo 'alias cyrus="cargo run --"' >> ~/.bashrc

VOLUME [ "/app" ]

CMD ["/usr/bin/bash"]
