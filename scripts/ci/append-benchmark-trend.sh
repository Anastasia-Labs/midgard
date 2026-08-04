#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
RESULTS_ROOT="${BENCHMARK_RESULTS_ROOT:-$REPO_ROOT/benchmark-results/${GITHUB_SHA:-local}}"
TREND_DIR="${BENCHMARK_TREND_DIR:-$REPO_ROOT/docs/benchmark-trends}"

node "$REPO_ROOT/scripts/ci/check-benchmark-regression.mjs" \
  --trend-dir "$TREND_DIR" \
  --results "$RESULTS_ROOT" \
  --append \
  --class-a-only
