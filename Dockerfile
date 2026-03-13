FROM rust:slim AS builder
WORKDIR /app
COPY src ./src
COPY Cargo.toml .
COPY Cargo.lock .
RUN cargo install --path .

# in case the container fails due to GLIBC version problems, it probably means
# this needs to be the same debian version as the build
FROM python:slim-trixie
RUN apt-get update && apt-get upgrade && apt-get install gcc -y && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/cargo/bin/compilers-2026 /usr/local/bin/compilers-2026
COPY --from=builder /app/src .
COPY assembler.py .
EXPOSE 3000
CMD ["compilers-2026", "serve"]
