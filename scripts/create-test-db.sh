#!/usr/bin/env bash
# Create (or repair) a Midgard test database on the local test Postgres.
#
# The local test server (port 5433) is launched with global
# `synchronous_commit=off` (and fsync=off) for speed, but CI runs Postgres
# with synchronous_commit=on, and the node suites assert the session-level
# setting is "on" (database.test.ts, tx-admissions-claim-load.test.ts) to
# prove the write-behind's `SET LOCAL synchronous_commit = off` relaxation
# stays transaction-local. A test database created without the per-database
# override therefore fails those suites locally. This helper folds the
# override into creation so it can never be forgotten (per-DB settings are
# also NOT inherited via CREATE DATABASE ... WITH TEMPLATE).
#
# Usage: scripts/create-test-db.sh <database-name> [...more names]
# Env:   PGHOST (127.0.0.1), PGPORT (5433), PGUSER (postgres),
#        PGPASSWORD (postgres), PSQL (explicit psql binary path)
set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "usage: $0 <database-name> [...more names]" >&2
  exit 2
fi

: "${PGHOST:=127.0.0.1}"
: "${PGPORT:=5433}"
: "${PGUSER:=postgres}"
: "${PGPASSWORD:=postgres}"
export PGHOST PGPORT PGUSER PGPASSWORD

# Resolve psql: explicit override, PATH, then the running server's own bin
# directory (the local server is launched from a nix store path that is not
# on PATH).
resolve_psql() {
  if [ -n "${PSQL:-}" ]; then
    echo "$PSQL"
    return
  fi
  if command -v psql >/dev/null 2>&1; then
    command -v psql
    return
  fi
  local pid
  pid=$(ss -ltnp 2>/dev/null | sed -n "s/.*:${PGPORT} .*pid=\([0-9]*\).*/\1/p" | head -1)
  if [ -n "$pid" ] && [ -r "/proc/$pid/exe" ]; then
    local server_bin
    server_bin=$(readlink "/proc/$pid/exe")
    if [ -x "$(dirname "$server_bin")/psql" ]; then
      echo "$(dirname "$server_bin")/psql"
      return
    fi
  fi
  echo "error: psql not found (set PSQL=/path/to/psql)" >&2
  exit 1
}

PSQL_BIN=$(resolve_psql)

for db in "$@"; do
  if ! [[ "$db" =~ ^[a-z_][a-z0-9_]*$ ]]; then
    echo "error: refusing unsafe database name: $db" >&2
    exit 1
  fi
  if [ "$db" = "midgard" ]; then
    echo "error: 'midgard' is the protected non-test database; pick an isolated test name" >&2
    exit 1
  fi
  exists=$("$PSQL_BIN" -d postgres -Atc "SELECT 1 FROM pg_database WHERE datname = '$db'")
  if [ "$exists" != "1" ]; then
    "$PSQL_BIN" -d postgres -c "CREATE DATABASE $db"
    echo "created: $db"
  else
    echo "exists:  $db"
  fi
  "$PSQL_BIN" -d postgres -c "ALTER DATABASE $db SET synchronous_commit = on" >/dev/null
  applied=$("$PSQL_BIN" -d "$db" -Atc "SHOW synchronous_commit")
  echo "synchronous_commit override: $db -> $applied (per-database, matches CI)"
done
