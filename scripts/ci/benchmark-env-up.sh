#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo/midgard-node"

BENCHMARK_CLASS="${BENCHMARK_CLASS:-A}"
OBSERVABILITY_PROFILE=()
if [ "${BENCHMARK_OBSERVABILITY:-0}" = "1" ]; then
  OBSERVABILITY_PROFILE=(--profile observability)
fi

COMPOSE_FILES=(-f docker-compose.yaml)
if [ "$BENCHMARK_CLASS" = "B" ]; then
  COMPOSE_FILES+=(-f docker-compose.kupmios.yaml)
fi
COMPOSE_FILES+=(-f docker-compose.benchmark.yaml)

docker compose "${COMPOSE_FILES[@]}" "${OBSERVABILITY_PROFILE[@]}" config >/tmp/midgard-benchmark-compose.yaml

if [ "$BENCHMARK_CLASS" = "B" ]; then
  docker compose "${COMPOSE_FILES[@]}" "${OBSERVABILITY_PROFILE[@]}" up -d
else
  docker compose "${COMPOSE_FILES[@]}" "${OBSERVABILITY_PROFILE[@]}" up -d postgres midgard-node
fi
