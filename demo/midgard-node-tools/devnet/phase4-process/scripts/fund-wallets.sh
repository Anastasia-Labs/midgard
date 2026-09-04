#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command jq
require_command node
require_run_dir
set -a
. "$MIDGARD_PHASE4_RUN_DIR/secrets/wallets.env"
set +a
wallet_vars='L1_OPERATOR_SEED_PHRASE L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX L1_REFERENCE_SCRIPT_SEED_PHRASE USER_SEED_PHRASE TESTNET_GENESIS_WALLET_SEED_PHRASE_A TESTNET_GENESIS_WALLET_SEED_PHRASE_B'
for var in $wallet_vars; do eval "value=\${$var:-}"; [ -n "$value" ] || die "missing wallet secret $var"; done
(
  cd "$node_root"
  NETWORK=Custom node --input-type=module - "$MIDGARD_PHASE4_RUN_DIR/work/wallet-addresses.json" $wallet_vars <<'NODE'
import { writeFileSync } from "node:fs";
import { walletFromSeed } from "@lucid-evolution/lucid";
const [out, ...names] = process.argv.slice(2);
const rows = names.map((name) => ({ name, address: walletFromSeed(process.env[name], { network: "Custom" }).address }));
if (new Set(rows.map(({ address }) => address)).size !== rows.length) throw new Error("Phase 4 wallet roles must be distinct");
writeFileSync(out, `${JSON.stringify(rows, null, 2)}\n`, { mode: 0o600 });
NODE
)
cardano_image="$PHASE4_CARDANO_NODE_IMAGE"
cli() { docker run --rm --user "$(id -u):$(id -g)" --volume "$MIDGARD_PHASE4_RUN_DIR:/run" --entrypoint cardano-cli "$cardano_image" latest "$@"; }
genesis_address=$(cli genesis initial-addr --verification-key-file /run/genesis/utxo-keys/utxo1/utxo.vkey --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC")
cli query utxo --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" --address "$genesis_address" --out-file /run/work/genesis-utxos.json
tx_in=$(jq -r 'to_entries | max_by(.value.value.lovelace) | .key' "$MIDGARD_PHASE4_RUN_DIR/work/genesis-utxos.json")
[ "$tx_in" != null ] || die "genesis UTxO not available"
set --
while IFS= read -r address; do set -- "$@" --tx-out "$address+10000000000"; done <<EOF
$(jq -r '.[].address' "$MIDGARD_PHASE4_RUN_DIR/work/wallet-addresses.json")
EOF
cli transaction build --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" --tx-in "$tx_in" "$@" --change-address "$genesis_address" --out-file /run/work/fund-wallets.txbody
cli transaction sign --tx-body-file /run/work/fund-wallets.txbody --signing-key-file /run/genesis/utxo-keys/utxo1/utxo.skey --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" --out-file /run/work/fund-wallets.tx
cli transaction submit --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" --tx-file /run/work/fund-wallets.tx
attempts=120
while [ "$attempts" -gt 0 ]; do
  confirmed=true
  : >"$MIDGARD_PHASE4_RUN_DIR/work/funding-confirmation.ndjson"
  while IFS= read -r row; do
    name=$(printf '%s' "$row" | jq -r '.name')
    address=$(printf '%s' "$row" | jq -r '.address')
    output="/run/work/funding-$name.json"
    cli query utxo --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" --address "$address" --out-file "$output"
    lovelace=$(jq '[to_entries[].value.value.lovelace // 0] | add // 0' "$MIDGARD_PHASE4_RUN_DIR/work/funding-$name.json")
    jq -n --arg name "$name" --arg address "$address" --argjson lovelace "$lovelace" '{name:$name,address:$address,lovelace:$lovelace}' >>"$MIDGARD_PHASE4_RUN_DIR/work/funding-confirmation.ndjson"
    [ "$lovelace" -ge 10000000000 ] || confirmed=false
  done <<EOF
$(jq -c '.[]' "$MIDGARD_PHASE4_RUN_DIR/work/wallet-addresses.json")
EOF
  [ "$confirmed" = true ] && break
  attempts=$((attempts - 1)); sleep 1
done
[ "$attempts" -gt 0 ] || die "all-wallet funding did not confirm at the required value"
