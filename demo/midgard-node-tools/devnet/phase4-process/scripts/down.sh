#!/bin/sh
# usage: down.sh <absolute-run-dir>
#
# Stop a run's L1 services and the log followers up.sh started. The
# containers are removed; the run directory and everything under it (chain,
# index and database state, logs) are kept, so up.sh can start the run again.
# Discarding a run is a separate, deliberate step: remove its directory.
#
# Compose interpolates compose.yaml's required variables even for `down`;
# they come from the run's own run.env, never from the caller's environment.
set -eu
[ "$#" -eq 1 ] || {
  printf '%s\n' "usage: down.sh <absolute-run-dir>" >&2
  exit 2
}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command docker
use_run_dir "$1"

# No --volumes: the services' state lives in bind mounts under the run
# directory, and down must never delete it.
run_compose down
stop_log_followers
printf '%s\n' "servicesStopped=true" "composeProject=$MIDGARD_PHASE4_COMPOSE_PROJECT" "statePreserved=$MIDGARD_PHASE4_RUN_DIR"
