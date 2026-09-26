#!/usr/bin/env bash
# Build the pinned Aiken fork: the one compiler this repository compiles,
# formats and tests with (stock v1.1.22 compiles unsound expect-decoders).
#
# The recipe used to live inline in `.github/workflows/aiken-ci.yml`; both
# workflows and local setup now call this script instead, so there is one way
# to produce the compiler. The pin itself is read from the workflows through
# `onchain/aiken/scripts/pinned-compiler.mjs --print-pin`, which refuses to
# print anything if the workflows disagree.
#
# The binary lands in <prefix>/bin/aiken. Nothing is written to ~/.cargo/bin:
# `cargo install --root <prefix>` is the only install step. (cargo's download
# cache under ~/.cargo/registry and the rustup toolchain are still shared, as
# they are for any cargo build.)
#
# Exit codes: 0 built or already present and correct; 1 the build failed or the
# built binary does not report the pin; 2 usage error.

set -euo pipefail

# The fork's own toolchain, selected with `cargo +<toolchain>` rather than by
# changing the default, so other Rust builds on the same machine are untouched.
rust_toolchain="1.94.1"
prefix=""
print_pin=0

usage() {
  cat <<'EOF'
Usage: scripts/ci/build-aiken-fork.sh --prefix DIR [--toolchain VERSION]
       scripts/ci/build-aiken-fork.sh --print-pin
       scripts/ci/build-aiken-fork.sh --help

Builds the Aiken fork pinned by AIKEN_FORK_* in the workflows into DIR/bin/aiken
and asserts it reports AIKEN_FORK_VERSION. A DIR that already holds the pinned
binary is left as is, so a restored CI cache costs nothing.

  --prefix DIR         install root (required); e.g. ~/.aiken-fork
  --toolchain VERSION  rustup toolchain for the build (default 1.94.1)
  --print-pin          print the pinned repo/tag/rev/version and exit

Point MIDGARD_AIKEN_BIN at DIR/bin/aiken (or put DIR/bin on PATH) afterwards.
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --prefix)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      prefix="$2"
      shift 2
      ;;
    --toolchain)
      [ "$#" -ge 2 ] || { usage >&2; exit 2; }
      rust_toolchain="$2"
      shift 2
      ;;
    --print-pin)
      print_pin=1
      shift
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      echo "build-aiken-fork: unknown argument '$1'" >&2
      usage >&2
      exit 2
      ;;
  esac
done

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
pin="$(node "$repository_root/onchain/aiken/scripts/pinned-compiler.mjs" --print-pin)"

if [ "$print_pin" -eq 1 ]; then
  printf '%s\n' "$pin"
  exit 0
fi

if [ -z "$prefix" ]; then
  echo "build-aiken-fork: --prefix is required (the script never installs into ~/.cargo/bin)" >&2
  usage >&2
  exit 2
fi

AIKEN_FORK_REPO="" AIKEN_FORK_REV="" AIKEN_FORK_VERSION=""
while IFS='=' read -r key value; do
  case "$key" in
    AIKEN_FORK_REPO) AIKEN_FORK_REPO="$value" ;;
    AIKEN_FORK_REV) AIKEN_FORK_REV="$value" ;;
    AIKEN_FORK_VERSION) AIKEN_FORK_VERSION="$value" ;;
  esac
done <<<"$pin"
for required in AIKEN_FORK_REPO AIKEN_FORK_REV AIKEN_FORK_VERSION; do
  if [ -z "${!required}" ]; then
    echo "build-aiken-fork: pinned-compiler.mjs --print-pin printed no $required" >&2
    exit 1
  fi
done

binary="$prefix/bin/aiken"
reported() { "$binary" --version 2>/dev/null || true; }

if [ -x "$binary" ] && [ "$(reported)" = "$AIKEN_FORK_VERSION" ]; then
  echo "build-aiken-fork: $binary already reports $AIKEN_FORK_VERSION; nothing to build"
  exit 0
fi

if ! cargo "+$rust_toolchain" --version >/dev/null 2>&1; then
  rustup toolchain install "$rust_toolchain" --profile minimal
fi
# --locked honours the fork's committed Cargo.lock; without it cargo re-resolves
# every dependency on each build, so an unrelated crate release could redden a
# branch or silently change the compiler that produced a blueprint. --force
# replaces a binary from an earlier rev left under the same prefix.
cargo "+$rust_toolchain" install \
  --git "$AIKEN_FORK_REPO" \
  --rev "$AIKEN_FORK_REV" \
  --locked \
  --force \
  --root "$prefix" \
  aiken

# Assert, don't just print: a build that reports anything but the pin must not
# be mistaken for the compiler every gate is judged against.
found="$(reported)"
printf 'pinned fork: %s\nexpected:    %s\n' "$found" "$AIKEN_FORK_VERSION"
if [ "$found" != "$AIKEN_FORK_VERSION" ]; then
  echo "build-aiken-fork: $binary reports '$found', not the pinned '$AIKEN_FORK_VERSION'" >&2
  exit 1
fi
