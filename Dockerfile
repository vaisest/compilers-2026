FROM rust:slim AS builder
WORKDIR /app
COPY src ./src
COPY Cargo.toml .
COPY Cargo.lock .
RUN cargo install --path .

FROM debian:bookworm-slim
RUN apt-get update && apt-get upgrade && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/cargo/bin/compilers-2026 /usr/local/bin/compilers-2026
COPY --from=builder /app/src .
EXPOSE 3000
CMD ["compilers-2026", "serve"]
