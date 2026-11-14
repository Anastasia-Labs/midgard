#!/bin/sh
# usage: up.sh <absolute-run-dir>
#
# Start a generated run's L1 services (cardano-node, ogmios, kupo, postgres)
# as a block producer, or start them again after down.sh: the containers
# reuse the run's bind-mounted state. Each service's log is followed into
# <run>/logs/<service>.log (appended, so a restart keeps the earlier log),
# the node socket is opened to the host user, and the command returns once
# Ogmios and Kupo answer /health.
set -eu
[ "$#" -eq 1 ] || {
  printf '%s\n' "usage: up.sh <absolute-run-dir>" >&2
  exit 2
}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command curl
require_command docker
require_command setsid
use_run_dir "$1"
export MIDGARD_PHASE4_BLOCK_PRODUCER=true

logs="$MIDGARD_PHASE4_RUN_DIR/logs"
mkdir -p "$logs"
stop_log_followers
started_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)
run_compose up --detach cardano-node ogmios kupo postgres
# The followers outlive this command (setsid), as the services do.
set -- docker compose --project-name "$MIDGARD_PHASE4_COMPOSE_PROJECT" --file "$compose_file"
if [ -f "$MIDGARD_PHASE4_RUN_DIR/compose.host-paths.json" ]; then
  set -- "$@" --file "$MIDGARD_PHASE4_RUN_DIR/compose.host-paths.json"
fi
for service in cardano-node ogmios kupo postgres; do
  setsid nohup "$@" logs --no-color --follow --since "$started_at" "$service" \
    >>"$logs/$service.log" 2>&1 </dev/null &
  printf '%s\n' "$!" >"$logs/$service.follower.pid"
done
grant_cardano_socket_access 240
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
printf '%s\n' "servicesStarted=true" "composeProject=$MIDGARD_PHASE4_COMPOSE_PROJECT" "logs=$logs"
