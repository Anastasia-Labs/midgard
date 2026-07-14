#!/usr/bin/env bash
set -euo pipefail

SCENARIO_SET="${1:-nightly}"
REPO_ROOT="$(git rev-parse --show-toplevel)"
RESULTS_ROOT="${BENCHMARK_RESULTS_ROOT:-$REPO_ROOT/benchmark-results/${GITHUB_SHA:-local}}"
NODE_LOG_PATH="${BENCHMARK_NODE_LOG_PATH:-}"

case "$SCENARIO_SET" in
  nightly)
    SCENARIOS=(find-max-ramp accept-2500-tps-gate)
    ;;
  weekly)
    SCENARIOS=(find-max-ramp accept-2500-tps-gate soak-10min-at-max burst-2x-target mixed-workload-multi-io)
    ;;
  all)
    SCENARIOS=(find-max-ramp accept-2500-tps-gate soak-10min-at-max burst-2x-target mixed-workload-multi-io)
    ;;
  *)
    echo "unknown scenario set: $SCENARIO_SET" >&2
    exit 2
    ;;
esac

mkdir -p "$RESULTS_ROOT"
for scenario in "${SCENARIOS[@]}"; do
  echo "::group::benchmark scenario: $scenario"
  set +e
  (
    cd "$REPO_ROOT/demo/midgard-node"
    STRESS_REPORT_PATH="$RESULTS_ROOT/$scenario.json" \
      pnpm run "bench:l2:scenario:$scenario"
  )
  status=$?
  set -e
  if [ "$status" -eq 78 ]; then
    echo "$scenario is explicitly blocked; continuing"
  elif [ "$status" -ne 0 ]; then
    exit "$status"
  fi
  if [ -f "$RESULTS_ROOT/$scenario.json" ]; then
    tag_args=(--report "$RESULTS_ROOT/$scenario.json")
    if [ -n "$NODE_LOG_PATH" ]; then
      tag_args+=(--log "$NODE_LOG_PATH")
    fi
    node "$REPO_ROOT/scripts/ci/tag-defect-signatures.mjs" "${tag_args[@]}"
  fi
  echo "::endgroup::"
done
