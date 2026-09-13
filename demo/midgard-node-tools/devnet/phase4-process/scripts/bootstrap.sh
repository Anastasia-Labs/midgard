#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command curl
require_command docker
require_command jq
require_run_dir
[ -f "$MIDGARD_PHASE4_RUN_DIR/secrets/wallets.env" ] || die "missing run-scoped secrets/wallets.env"
[ -f "$MIDGARD_PHASE4_RUN_DIR/secrets/node.env" ] || die "missing run-scoped secrets/node.env"

node_env="$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
umask 077
upsert_private_env() {
  key=$1
  value=$2
  file=$3
  temporary="$file.tmp.$$"
  awk -v key="$key" -v value="$value" '
    BEGIN { found = 0 }
    index($0, key "=") == 1 {
      if (found == 0) {
        print key "=" value
        found = 1
      }
      next
    }
    { print }
    END { if (found == 0) print key "=" value }
  ' "$file" >"$temporary"
  chmod 600 "$temporary"
  mv "$temporary" "$file"
}

upsert_private_env POSTGRES_HOST 127.0.0.1 "$node_env"
upsert_private_env POSTGRES_PORT "$MIDGARD_PHASE4_POSTGRES_PORT" "$node_env"
upsert_private_env POSTGRES_USER "$MIDGARD_PHASE4_POSTGRES_USER" "$node_env"
upsert_private_env POSTGRES_PASSWORD "$MIDGARD_PHASE4_POSTGRES_PASSWORD" "$node_env"
upsert_private_env POSTGRES_DB "$MIDGARD_PHASE4_POSTGRES_DATABASE" "$node_env"
compose up --detach cardano-node ogmios kupo postgres
grant_cardano_socket_access
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
"$script_dir/fund-wallets.sh"

wallet_addresses="$MIDGARD_PHASE4_RUN_DIR/work/wallet-addresses.json"
reference_script_address=$(jq -er '
  [.[] | select(.name == "L1_REFERENCE_SCRIPT_SEED_PHRASE") | .address]
  | if length == 1 and (.[0] | type) == "string" and (.[0] | length) > 0
    then .[0]
    else error("expected exactly one funded reference-script wallet address")
    end
' "$wallet_addresses")

# shellcheck disable=SC1090
. "$node_env"
if [ -n "${L1_REFERENCE_SCRIPT_ADDRESS:-}" ] && [ "$L1_REFERENCE_SCRIPT_ADDRESS" != "$reference_script_address" ]; then
  die "L1_REFERENCE_SCRIPT_ADDRESS conflicts with the funded reference-script wallet"
fi
if [ -n "${L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS:-}" ] && [ "$L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS" != "$reference_script_address" ]; then
  die "L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS conflicts with the funded reference-script wallet"
fi

upsert_private_env L1_REFERENCE_SCRIPT_ADDRESS "$reference_script_address" "$node_env"
upsert_private_env L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS "$reference_script_address" "$node_env"
"$script_dir/protocol-bootstrap.sh"
"$script_dir/write-acceptance-env.sh"
printf '%s\n' "bootstrapComplete=true" "runDir=$MIDGARD_PHASE4_RUN_DIR" "snapshotRequired=true"
