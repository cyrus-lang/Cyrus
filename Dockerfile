FROM rust:1.84.1-bullseye

RUN apt update && apt install -y build-essential gcc libgccjit-12-dev

WORKDIR /app

COPY . .

CMD ["make", "run"]
