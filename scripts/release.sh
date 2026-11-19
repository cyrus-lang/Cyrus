#!/usr/bin/env bash
# Build a release stage0 compiler.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

./x.py build --stage stage0 --profile release --jobs "${JOBS:-24}"

BIN="$ROOT/toolchain/stage0/target/release/cyrus"
echo "release binary: $BIN"
