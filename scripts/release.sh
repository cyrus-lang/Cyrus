#!/usr/bin/env bash
# Build a release stage0 compiler and compress it with upx.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

./x.py build --stage stage0 --profile release --jobs "${JOBS:-24}"

BIN="$ROOT/toolchain/stage0/target/release/cyrus"
if command -v upx >/dev/null 2>&1; then
    upx --best --lzma "$BIN"
else
    echo "upx not found; skipping compression" >&2
fi

echo "release binary: $BIN"
