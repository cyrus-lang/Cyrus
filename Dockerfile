# Use Rust 1.84.1 with Debian Bookworm for better package availability
FROM rust:1.84.1-bookworm

# Set non-interactive mode to prevent prompts during installation
ENV DEBIAN_FRONTEND=noninteractive 

# Update package list and install dependencies
RUN apt update && apt install -y \
    build-essential \
    gcc \
    libgccjit-12-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy source files into the container
COPY . .

# Build the Rust project
RUN cargo build --release

# Set the entrypoint for the container
CMD ["./target/release/my-cyrus-lang"]
