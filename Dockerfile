FROM alpine:latest

RUN apk add --no-cache rust cargo gcc make

RUN ln -sf /usr/bin/gcc /usr/bin/gcc && \
    ln -sf /usr/bin/g++ /usr/bin/g++

WORKDIR /app

COPY . .

RUN mkdir -p tmp && touch tmp/main

RUN make

CMD ["/bin/sh"]
