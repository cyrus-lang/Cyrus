FROM archlinux:latest

RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm base-devel curl git

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"

RUN rustc --version && cargo --version

# WORKDIR /app

# COPY . .

# RUN mkdir -p tmp
# RUN echo 'alias cyrus="cargo run --"' >> ~/.bashrc

# VOLUME [ "/app" ]

# CMD ["/usr/bin/bash"]
