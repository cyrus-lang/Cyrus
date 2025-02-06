FROM rust:1.84.1-alpine3.21

RUN apk add --no-cache build-base make libgccjit libgccjit-dev

WORKDIR /app

COPY . .

CMD ["make", "run"]
