#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
: "${MIDGARD_WATCHER_JOURNEY_RUN_DIR:?An absolute fresh run directory is required}"
export MIDGARD_PHASE4_RUN_DIR="$MIDGARD_WATCHER_JOURNEY_RUN_DIR"
export MIDGARD_PHASE4_RUN_ID="watcher_journeys_$(basename "$MIDGARD_PHASE4_RUN_DIR")"
sh "$script_dir/../../phase4-process/scripts/generate.sh"

# Reuse pinned service provisioning, then bind the watcher to the exact host
# genesis paths. Never change genesis after starting these services.
set -a
. "$MIDGARD_PHASE4_RUN_DIR/run.env"
set +a
run_dir=$MIDGARD_PHASE4_RUN_DIR
jq --arg root "$run_dir" \
  'with_entries(if (.key | endswith("GenesisFile")) then .value=($root + .value) else . end)' \
  "$run_dir/config/config.json" > "$run_dir/work/config.json"
mv "$run_dir/work/config.json" "$run_dir/config/config.json"
jq -n --arg mount "$run_dir/genesis:$run_dir/genesis:ro" \
  --arg nodeImage "$MIDGARD_PHASE4_CARDANO_NODE_IMAGE" \
  '{services:{"cardano-node":{image:$nodeImage,volumes:[$mount]},ogmios:{volumes:[$mount]}}}' \
  > "$run_dir/compose.host-paths.json"
printf '%s\n' "watcherGenesisPrepared=$run_dir" "configuration=verified-preprod" "servicesStarted=false"
