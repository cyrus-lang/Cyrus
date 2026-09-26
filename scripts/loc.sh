#!/usr/bin/env bash
#
# loc.sh
# Recursively count all lines in .cyrus source files.
#
# Usage:
#   ./loc.sh [directory]
#
# If no directory is given, the current working directory is used.

set -euo pipefail

# Configuration
TARGET_DIR="${1:-.}"
EXTENSION="cyrus"

# Validate input
if [[ ! -d "$TARGET_DIR" ]]; then
    echo "Error: '$TARGET_DIR' is not a directory." >&2
    exit 1
fi

# Collect files
mapfile -d '' files < <(
    find "$TARGET_DIR" -type f -name "*.${EXTENSION}" -print0
)

if [[ ${#files[@]} -eq 0 ]]; then
    echo "No .${EXTENSION} files found in '$TARGET_DIR'."
    exit 0
fi

# Count lines
total=$(printf '%s\0' "${files[@]}" | xargs -0 wc -l | tail -n 1 | awk '{print $1}')

# --- Report ------------------------------------------------------------------
printf 'Directory     : %s\n' "$TARGET_DIR"
printf 'File type     : *.%s\n' "$EXTENSION"
printf 'Files matched : %d\n' "${#files[@]}"
printf 'Total lines   : %d\n' "$total"