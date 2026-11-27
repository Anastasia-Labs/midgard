#!/bin/sh
set -eu

chain_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
compose_file="$chain_root/compose.yaml"
tools_root=$(CDPATH= cd -- "$chain_root/../.." && pwd)

# Every image that can affect the chain, index or database is pinned to an
# official immutable digest, here in one place so helper containers and the
# compose graph run the same images.
ISOLATED_CHAIN_CARDANO_NODE_IMAGE="ghcr.io/intersectmbo/cardano-node:11.1.3@sha256:b863d5751bf691588995a8e12509057076e207270db65e88bb16bd543aaa9347"
ISOLATED_CHAIN_OGMIOS_IMAGE="cardanosolutions/ogmios:v7.0.0@sha256:8892ef5f77b94f1c95427cf9f2b40e6235a32b27a8b1e378db02289f3991617f"
ISOLATED_CHAIN_KUPO_IMAGE="cardanosolutions/kupo:v2.11.0@sha256:0a8cd8b5e373103e9e0a68b162d82c69a6f76042bb073c31912724034e51ca9e"
ISOLATED_CHAIN_POSTGRES_IMAGE="postgres:15.15-alpine@sha256:d4c38e1c60871a1e8fe5d05b639980ac7827c9e6920a5df663a16f86f398aca6"
# The devnet is preprod-matched, so the active protocol major is a
# pinned part of the environment just like the node and provider images.  The
# current Aiken bundle uses Plutus V3 builtins (for example CountSetBits) that
# are unavailable at Conway's introductory major 9 but are accepted by the
# target preprod major 11 ledger.
ISOLATED_CHAIN_TARGET_PROTOCOL_MAJOR=11

die() {
  printf '%s\n' "isolated-chain: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

require_run_dir() {
  : "${MIDGARD_ISOLATED_CHAIN_RUN_DIR:?MIDGARD_ISOLATED_CHAIN_RUN_DIR is required}"
  case "$MIDGARD_ISOLATED_CHAIN_RUN_DIR" in
    /*) ;;
    *) die "MIDGARD_ISOLATED_CHAIN_RUN_DIR must be absolute" ;;
  esac
  requested_run_dir=$(CDPATH= cd -- "$MIDGARD_ISOLATED_CHAIN_RUN_DIR" && pwd)
  readonly requested_run_dir
  [ -f "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/run.env" ] || die "missing $MIDGARD_ISOLATED_CHAIN_RUN_DIR/run.env"
  # shellcheck disable=SC1090
  . "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/run.env"
  configured_run_dir=$(CDPATH= cd -- "$MIDGARD_ISOLATED_CHAIN_RUN_DIR" && pwd)
  [ "$configured_run_dir" = "$requested_run_dir" ] || die "run.env may not redirect the authoritative run directory"
  case "$MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT" in
    midgard_isolated_chain_*) ;;
    *) die "compose project must use midgard_isolated_chain_ prefix" ;;
  esac
  case "$MIDGARD_ISOLATED_CHAIN_POSTGRES_DATABASE" in
    midgard_isolated_chain_*) ;;
    *) die "database must use midgard_isolated_chain_ prefix" ;;
  esac
  export MIDGARD_ISOLATED_CHAIN_RUN_DIR MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT
  export MIDGARD_ISOLATED_CHAIN_POSTGRES_DATABASE MIDGARD_ISOLATED_CHAIN_POSTGRES_PASSWORD
  export MIDGARD_ISOLATED_CHAIN_POSTGRES_USER MIDGARD_ISOLATED_CHAIN_NETWORK_MAGIC
  export MIDGARD_ISOLATED_CHAIN_PROTOCOL_MAJOR
  export MIDGARD_ISOLATED_CHAIN_OGMIOS_PORT MIDGARD_ISOLATED_CHAIN_KUPO_PORT MIDGARD_ISOLATED_CHAIN_POSTGRES_PORT
}

compose() {
  docker compose --project-name "$MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT" --file "$compose_file" "$@"
}

# up.sh and down.sh take the run directory as their only argument and read
# every other setting from its run.env: a value left in the caller's
# environment (another run's project or password) must not reach compose.
use_run_dir() {
  for inherited in $(env | sed -n 's/^\(MIDGARD_ISOLATED_CHAIN_[A-Za-z0-9_]*\)=.*/\1/p'); do
    unset "$inherited"
  done
  MIDGARD_ISOLATED_CHAIN_RUN_DIR=$1
  require_run_dir
}

# Compose for the run, with the watcher-journey host-path overlay
# (compose.host-paths.json) when the run has one.
run_compose() {
  if [ -f "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/compose.host-paths.json" ]; then
    compose --file "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/compose.host-paths.json" "$@"
  else
    compose "$@"
  fi
}

# Stop the `compose logs --follow` processes up.sh left for this run. A pid
# is signalled only while it is still a log follower of this compose
# project, so a reused pid is never killed.
stop_log_followers() {
  for pid_file in "$MIDGARD_ISOLATED_CHAIN_RUN_DIR"/logs/*.follower.pid; do
    [ -f "$pid_file" ] || continue
    follower=$(cat "$pid_file")
    case "$follower" in
      "" | *[!0-9]*) ;;
      *)
        if [ -r "/proc/$follower/cmdline" ] \
          && tr '\0' ' ' <"/proc/$follower/cmdline" | grep -F -- "$MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT" | grep -q -- ' logs '; then
          # Each follower leads its own process group (setsid).
          kill -TERM -"$follower" 2>/dev/null || kill -TERM "$follower" 2>/dev/null || true
        fi
        ;;
    esac
    rm -f "$pid_file"
  done
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

grant_cardano_socket_access() {
  cardano_socket="$MIDGARD_ISOLATED_CHAIN_RUN_DIR/cardano/ipc/node.socket"
  attempts=${1:-180}
  while [ "$attempts" -gt 0 ]; do
    if [ -S "$cardano_socket" ] && docker run --rm \
      --volume "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/cardano/ipc:/ipc" \
      --entrypoint sh "$ISOLATED_CHAIN_POSTGRES_IMAGE" \
      -ec 'test -S /ipc/node.socket; chmod 0666 /ipc/node.socket'; then
      return 0
    fi
    attempts=$((attempts - 1))
    sleep 1
  done
  die "timed out waiting for Cardano node socket access"
}
