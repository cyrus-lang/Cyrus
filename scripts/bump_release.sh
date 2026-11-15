#!/usr/bin/env bash
# Bump the compiler patch version by one unit, 
# commit, tag, and push to trigger GitHub artifact publishing.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
fi

VERSION_FILE="$ROOT/toolchain/stage0/VERSION"
PROJECT_TOML="$ROOT/toolchain/stage1/Project.toml"
VERSION_CYRUS="$ROOT/toolchain/stage1/src/version.cyrus"

CURRENT_VERSION="$(tr -d '[:space:]' < "$VERSION_FILE")"
IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"
if [[ -z "${MAJOR:-}" || -z "${MINOR:-}" || -z "${PATCH:-}" ]]; then
    echo "error: invalid version '$CURRENT_VERSION' in $VERSION_FILE" >&2
    exit 1
fi

NEW_VERSION="${MAJOR}.${MINOR}.$((PATCH + 1))"
TAG="v${NEW_VERSION}"

echo "Current version: $CURRENT_VERSION"
echo "New version:     $NEW_VERSION (tag: $TAG)"

if $DRY_RUN; then
    echo "[dry-run] would update $VERSION_FILE, $PROJECT_TOML, $VERSION_CYRUS"
    echo "[dry-run] would commit, tag $TAG, and push to origin"
    exit 0
fi

printf '%s\n' "$NEW_VERSION" > "$VERSION_FILE"
sed -i "s/^version = \".*\"/version = \"${NEW_VERSION}\"/" "$PROJECT_TOML"
sed -i "s/pub const COMPILER_VERSION = \".*\";/pub const COMPILER_VERSION = \"${NEW_VERSION}-selfhost\";/" "$VERSION_CYRUS"

git add "$VERSION_FILE" "$PROJECT_TOML" "$VERSION_CYRUS"
git commit -m "chore: bump version to ${TAG}"
git tag -a "$TAG" -m "Release ${TAG}"
git push origin HEAD
git push origin "$TAG"

echo "Pushed ${TAG}; GitHub Actions will build and publish release artifacts."
