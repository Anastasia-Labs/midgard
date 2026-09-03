#!/bin/sh
set -eu

phase4_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
compose_file="$phase4_root/compose.yaml"
# The devnet lives in midgard-node-tools; the node under test is the sibling
# operator package. Both source/dist trees are bound into the snapshot identity.
tools_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
node_root=$(CDPATH= cd -- "$tools_root/../midgard-node" && pwd)

# Phase 4 is a matched-snapshot experiment.  Every image that can affect the
# chain, index, or database is pinned to an official immutable digest.  Keep
# these values in one place so helper containers and the compose graph cannot
# silently drift apart.
PHASE4_CARDANO_NODE_IMAGE="ghcr.io/intersectmbo/cardano-node:11.0.1@sha256:33378c806485729154e6652ffc6813748949ae049180788b94018a709f5a0400"
PHASE4_OGMIOS_IMAGE="cardanosolutions/ogmios:v7.0.0@sha256:8892ef5f77b94f1c95427cf9f2b40e6235a32b27a8b1e378db02289f3991617f"
PHASE4_KUPO_IMAGE="cardanosolutions/kupo:v2.11.0@sha256:0a8cd8b5e373103e9e0a68b162d82c69a6f76042bb073c31912724034e51ca9e"
PHASE4_POSTGRES_IMAGE="postgres:15.15-alpine@sha256:d4c38e1c60871a1e8fe5d05b639980ac7827c9e6920a5df663a16f86f398aca6"
# Phase 4 is a preprod-matched experiment, so the active protocol major is a
# pinned part of the environment just like the node and provider images.  The
# current Aiken bundle uses Plutus V3 builtins (for example CountSetBits) that
# are unavailable at Conway's introductory major 9 but are accepted by the
# target preprod major 11 ledger.
PHASE4_TARGET_PROTOCOL_MAJOR=11

die() {
  printf '%s\n' "phase4-process: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

require_run_dir() {
  : "${MIDGARD_PHASE4_RUN_DIR:?MIDGARD_PHASE4_RUN_DIR is required}"
  case "$MIDGARD_PHASE4_RUN_DIR" in
    /*) ;;
    *) die "MIDGARD_PHASE4_RUN_DIR must be absolute" ;;
  esac
  requested_run_dir=$(CDPATH= cd -- "$MIDGARD_PHASE4_RUN_DIR" && pwd)
  readonly requested_run_dir
  [ -f "$MIDGARD_PHASE4_RUN_DIR/run.env" ] || die "missing $MIDGARD_PHASE4_RUN_DIR/run.env"
  # shellcheck disable=SC1090
  . "$MIDGARD_PHASE4_RUN_DIR/run.env"
  configured_run_dir=$(CDPATH= cd -- "$MIDGARD_PHASE4_RUN_DIR" && pwd)
  [ "$configured_run_dir" = "$requested_run_dir" ] || die "run.env may not redirect the authoritative Phase 4 run directory"
  case "$MIDGARD_PHASE4_COMPOSE_PROJECT" in
    midgard_phase4_process_*) ;;
    *) die "compose project must use midgard_phase4_process_ prefix" ;;
  esac
  case "$MIDGARD_PHASE4_POSTGRES_DATABASE" in
    midgard_phase4_process_*) ;;
    *) die "database must use midgard_phase4_process_ prefix" ;;
  esac
  export MIDGARD_PHASE4_RUN_DIR MIDGARD_PHASE4_COMPOSE_PROJECT
  export MIDGARD_PHASE4_POSTGRES_DATABASE MIDGARD_PHASE4_POSTGRES_PASSWORD
  export MIDGARD_PHASE4_POSTGRES_USER MIDGARD_PHASE4_NETWORK_MAGIC
  export MIDGARD_PHASE4_PROTOCOL_MAJOR
  export MIDGARD_PHASE4_OGMIOS_PORT MIDGARD_PHASE4_KUPO_PORT MIDGARD_PHASE4_POSTGRES_PORT
}

compose() {
  docker compose --project-name "$MIDGARD_PHASE4_COMPOSE_PROJECT" --file "$compose_file" "$@"
}

# The reset and recovery commands have a machine-readable stdout contract: on
# success they emit exactly one JSON attestation. Docker Compose writes normal
# progress messages to stderr, and the acceptance runner intentionally combines
# both child streams before parsing so failures retain complete diagnostics.
# Capture successful Compose chatter, but replay it on failure.
compose_quiet() {
  compose_log="$MIDGARD_PHASE4_RUN_DIR/work/compose.$$.log"
  if compose "$@" >"$compose_log" 2>&1; then
    rm -f "$compose_log"
    return 0
  fi
  compose_status=$?
  cat "$compose_log" >&2
  rm -f "$compose_log"
  return "$compose_status"
}

sha256_file() {
  sha256sum "$1" | awk '{print $1}'
}

tree_sha256() {
  directory=$1
  [ -d "$directory" ] || die "artifact directory is missing: $directory"
  find "$directory" -type f -print0 | sort -z | xargs -0 sha256sum | sha256sum | awk '{print $1}'
}

image_id() {
  image=$1
  docker image inspect "$image" --format '{{.Id}}' 2>/dev/null || die "pinned image is not available locally: $image"
}

wait_http() {
  url=$1
  attempts=${2:-120}
  while [ "$attempts" -gt 0 ]; do
    if curl --fail --silent --show-error "$url" >/dev/null 2>&1; then
      return 0
    fi
    attempts=$((attempts - 1))
    sleep 1
  done
  die "timed out waiting for $url"
}

parse_kupo_checkpoint() {
  parsed_kupo_checkpoint=$(
    awk '
      BEGIN { samples = 0; invalid = 0 }
      /^[[:space:]]*kupo_most_recent_checkpoint/ {
        if ($0 !~ /^[[:space:]]*kupo_most_recent_checkpoint[[:space:]]+[0-9]+[[:space:]]*$/) {
          invalid = 1
          next
        }
        value = $0
        sub(/^[[:space:]]*kupo_most_recent_checkpoint[[:space:]]+/, "", value)
        sub(/[[:space:]]*$/, "", value)
        samples += 1
        checkpoint = value
      }
      END {
        if (invalid || samples != 1) exit 1
        print checkpoint
      }
    '
  ) || die "Kupo health must contain exactly one unlabeled finite nonnegative integer kupo_most_recent_checkpoint sample"
  printf '%s\n' "$parsed_kupo_checkpoint"
}

grant_cardano_socket_access() {
  cardano_socket="$MIDGARD_PHASE4_RUN_DIR/cardano/ipc/node.socket"
  attempts=${1:-180}
  while [ "$attempts" -gt 0 ]; do
    if [ -S "$cardano_socket" ] && docker run --rm \
      --volume "$MIDGARD_PHASE4_RUN_DIR/cardano/ipc:/ipc" \
      --entrypoint sh "$PHASE4_POSTGRES_IMAGE" \
      -ec 'test -S /ipc/node.socket; chmod 0666 /ipc/node.socket'; then
      return 0
    fi
    attempts=$((attempts - 1))
    sleep 1
  done
  die "timed out waiting for Cardano node socket access"
}
