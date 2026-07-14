#!/usr/bin/env bash
set -euo pipefail

MESSAGE="${BENCHMARK_REGRESSION_MESSAGE:-Midgard benchmark regression or infra failure detected for ${GITHUB_SHA:-unknown-sha}.}"

if [ -z "${BENCHMARK_REGRESSION_WEBHOOK_URL:-}" ]; then
  echo "$MESSAGE"
  echo "BENCHMARK_REGRESSION_WEBHOOK_URL is unset; notification was logged only."
  exit 0
fi

curl -fsS \
  -H 'content-type: application/json' \
  --data "{\"text\":$(node -e 'process.stdout.write(JSON.stringify(process.argv[1]))' "$MESSAGE")}" \
  "$BENCHMARK_REGRESSION_WEBHOOK_URL"
