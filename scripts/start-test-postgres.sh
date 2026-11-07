#!/usr/bin/env bash
# Start (or reuse) the local Midgard test Postgres on 127.0.0.1:5433.
#
# The midgard-node and midgard-node-tools suites provision their own sharded
# databases in tests/global-setup.ts, but they need a reachable server first;
# without one Vitest reports "No test files found" and 150 files never run.
# This helper makes that server durable across reboots and sessions:
#
#   * no-op when something already listens on the port (another session's
#     server is left untouched);
#   * otherwise initdb (once) into a persistent data directory and start with
#     the flags the suites assume: synchronous_commit=on server-wide (CI
#     parity — database.test.ts asserts it), fsync=off for speed, no unix
#     socket (scratch paths overflow the socket path limit), 200 connections
#     so five shards x pool size never exhaust the server.
#
# Usage: scripts/start-test-postgres.sh [start|stop|status]
# Env:   PGPORT (5433), PGHOST (127.0.0.1), PGUSER (postgres),
#        PGPASSWORD (postgres), MIDGARD_TEST_PG_DATA (~/.midgard-pg/<port>),
#        MIDGARD_PG_BIN (explicit postgres bin directory)
set -euo pipefail

: "${PGHOST:=127.0.0.1}"
: "${PGPORT:=5433}"
: "${PGUSER:=postgres}"
: "${PGPASSWORD:=postgres}"
: "${MIDGARD_TEST_PG_DATA:=$HOME/.midgard-pg/$PGPORT}"
export PGHOST PGPORT PGUSER PGPASSWORD

action="${1:-start}"

port_listening() {
  ss -ltn 2>/dev/null | awk '{print $4}' | grep -qE "[:.]${PGPORT}\$"
}

listening_pid() {
  ss -ltnp 2>/dev/null | sed -n "s/.*:${PGPORT} .*pid=\([0-9]*\).*/\1/p" | head -1
}

# Resolve the postgres bin directory: explicit override, PATH, a running
# server's own binary, then the nix store (the local install is not on PATH).
resolve_bin_dir() {
  if [ -n "${MIDGARD_PG_BIN:-}" ]; then
    echo "$MIDGARD_PG_BIN"
    return
  fi
  if command -v pg_ctl >/dev/null 2>&1; then
    dirname "$(command -v pg_ctl)"
    return
  fi
  local pid
  pid=$(listening_pid)
  if [ -n "$pid" ] && [ -r "/proc/$pid/exe" ]; then
    dirname "$(readlink "/proc/$pid/exe")"
    return
  fi
  local candidate
  for candidate in /nix/store/*-postgresql-1[7-9].*/bin /nix/store/*-postgresql-1[7-9]/bin; do
    if [ -x "$candidate/pg_ctl" ]; then
      echo "$candidate"
      return
    fi
  done
  echo "error: no postgres binaries found (set MIDGARD_PG_BIN=/path/to/bin)" >&2
  exit 1
}

case "$action" in
  status)
    if port_listening; then
      pid=$(listening_pid)
      echo "listening: ${PGHOST}:${PGPORT} (pid ${pid:-unknown})"
      if [ -n "$pid" ] && [ -r "/proc/$pid/cmdline" ]; then
        tr '\0' ' ' < "/proc/$pid/cmdline"
        echo
      fi
      exit 0
    fi
    echo "not listening: ${PGHOST}:${PGPORT}"
    exit 1
    ;;
  start)
    if port_listening; then
      pid=$(listening_pid)
      echo "reusing server already listening on ${PGHOST}:${PGPORT} (pid ${pid:-unknown})"
      exit 0
    fi
    BIN=$(resolve_bin_dir)
    if [ ! -f "$MIDGARD_TEST_PG_DATA/PG_VERSION" ]; then
      mkdir -p "$MIDGARD_TEST_PG_DATA"
      pwfile=$(mktemp)
      trap 'rm -f "$pwfile"' EXIT
      printf '%s\n' "$PGPASSWORD" > "$pwfile"
      "$BIN/initdb" -D "$MIDGARD_TEST_PG_DATA" -U "$PGUSER" \
        --auth=scram-sha-256 --pwfile="$pwfile" --encoding=UTF8 >/dev/null
      echo "initialised: $MIDGARD_TEST_PG_DATA"
    fi
    "$BIN/pg_ctl" -D "$MIDGARD_TEST_PG_DATA" -l "$MIDGARD_TEST_PG_DATA/server.log" -w \
      -o "-h $PGHOST -p $PGPORT -c unix_socket_directories='' -c fsync=off -c synchronous_commit=on -c max_connections=200" \
      start >/dev/null
    for _ in $(seq 1 30); do
      if "$BIN/pg_isready" -h "$PGHOST" -p "$PGPORT" -q; then
        echo "started: ${PGHOST}:${PGPORT} data=$MIDGARD_TEST_PG_DATA log=$MIDGARD_TEST_PG_DATA/server.log"
        exit 0
      fi
      sleep 1
    done
    echo "error: server did not become ready on ${PGHOST}:${PGPORT}; see $MIDGARD_TEST_PG_DATA/server.log" >&2
    exit 1
    ;;
  stop)
    # Only stops a server running from this helper's data directory; a server
    # another session started elsewhere is deliberately left alone.
    if [ ! -f "$MIDGARD_TEST_PG_DATA/postmaster.pid" ]; then
      echo "no server running from $MIDGARD_TEST_PG_DATA"
      exit 0
    fi
    BIN=$(resolve_bin_dir)
    "$BIN/pg_ctl" -D "$MIDGARD_TEST_PG_DATA" -w -m fast stop >/dev/null
    echo "stopped: $MIDGARD_TEST_PG_DATA"
    ;;
  *)
    echo "usage: $0 [start|stop|status]" >&2
    exit 2
    ;;
esac
