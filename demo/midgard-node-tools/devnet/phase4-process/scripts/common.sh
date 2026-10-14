#!/bin/sh
set -eu

phase4_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
compose_file="$phase4_root/compose.yaml"
# The devnet lives in midgard-node-tools; the node under test is the sibling
# operator package. Both source/dist trees are bound into the snapshot identity.
tools_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
node_root=$(CDPATH= cd -- "$tools_root/../midgard-node" && pwd)

# Phase 4 is a matched-snapshot experiment.  Every image that can affect the
# chain, index, or database is pinned to an official immutable image list
# supporting ARM64 and AMD64. Keep these values in sync with compose.yaml so
# helper containers and the compose graph cannot silently drift apart.
PHASE4_CARDANO_NODE_IMAGE="ghcr.io/intersectmbo/cardano-node:11.1.0@sha256:78a3a5113b5989115a21bba74b8a990799aa43e5f9165641b9f0c062c266b58a"
PHASE4_OGMIOS_IMAGE="cardanosolutions/ogmios:v7.0.0@sha256:8892ef5f77b94f1c95427cf9f2b40e6235a32b27a8b1e378db02289f3991617f"
PHASE4_KUPO_IMAGE="cardanosolutions/kupo:v2.11.0@sha256:e7671142cfea29f43faf1a549c85cf2b28938e77bceddbf0f0b5cca07d493465"
PHASE4_POSTGRES_IMAGE="postgres:15.15-alpine@sha256:1d1095b0503145be79c79239372aefadfacf6b7c33c9245bbf99ce4b6b89c737"
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

# up.sh and down.sh take the run directory as their only argument and read
# every other setting from its run.env: a value left in the caller's
# environment (another run's project or password) must not reach compose.
use_run_dir() {
  for inherited in $(env | sed -n 's/^\(MIDGARD_PHASE4_[A-Za-z0-9_]*\)=.*/\1/p'); do
    unset "$inherited"
  done
  MIDGARD_PHASE4_RUN_DIR=$1
  require_run_dir
}

# Compose for the run, with the watcher-journey host-path overlay
# (compose.host-paths.json) when the run has one.
run_compose() {
  if [ -f "$MIDGARD_PHASE4_RUN_DIR/compose.host-paths.json" ]; then
    compose --file "$MIDGARD_PHASE4_RUN_DIR/compose.host-paths.json" "$@"
  else
    compose "$@"
  fi
}

# Stop the `compose logs --follow` processes up.sh left for this run. A pid
# is signalled only while it is still a log follower of this compose
# project, so a reused pid is never killed.
stop_log_followers() {
  for pid_file in "$MIDGARD_PHASE4_RUN_DIR"/logs/*.follower.pid; do
    [ -f "$pid_file" ] || continue
    follower=$(cat "$pid_file")
    case "$follower" in
      "" | *[!0-9]*) ;;
      *)
        if [ -r "/proc/$follower/cmdline" ] \
          && tr '\0' ' ' <"/proc/$follower/cmdline" | grep -F -- "$MIDGARD_PHASE4_COMPOSE_PROJECT" | grep -q -- ' logs '; then
          # Each follower leads its own process group (setsid).
          kill -TERM -"$follower" 2>/dev/null || kill -TERM "$follower" 2>/dev/null || true
        fi
        ;;
    esac
    rm -f "$pid_file"
  done
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
  # Kupo 2.11 returns JSON from /health. A disconnected or unsynchronized
  # service cannot attest a restoration checkpoint.
  jq -er 'select(.connection_status == "connected")
    | .most_recent_checkpoint
    | select(type == "number" and . >= 0 and . == floor)' \
    || die "Kupo health must report a connected nonnegative integer checkpoint"
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
