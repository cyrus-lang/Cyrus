#!/usr/bin/env bash

cargo build --release -j24

upx --best --lzma ./target/release/cyrus