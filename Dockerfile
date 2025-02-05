FROM alpine:latest

RUN apk update && apk add --no-cache \
    build-base \
    gcc \
    g++ \
    make \
    git \
    curl \
    tar \
    xz \
    bash \
    sudo

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

RUN apk add --no-cache gcc12 gcc12-libs gcc12-static libgccjit

RUN ln -sf /usr/bin/gcc-12 /usr/bin/gcc && \
    ln -sf /usr/bin/g++-12 /usr/bin/g++

WORKDIR /app

COPY . .

RUN make

CMD ["/bin/sh"]
