#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command jq
require_command node
require_command pnpm
require_command aiken
require_command sha256sum
require_run_dir
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
authoritative_run_dir=$MIDGARD_PHASE4_RUN_DIR
wallet_env="$MIDGARD_PHASE4_RUN_DIR/secrets/wallets.env"
node_env="$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
run_env="$MIDGARD_PHASE4_RUN_DIR/run.env"
manifest="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
run_state="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/deployment-run-state.json"
node_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
repo_root=$(CDPATH= cd -- "$node_root/../.." && pwd)
contract_root="$repo_root/onchain/aiken"
blueprint="$contract_root/plutus.json"
set -a; . "$node_env"; . "$wallet_env"; . "$run_env"; set +a
[ "$MIDGARD_PHASE4_RUN_DIR" = "$authoritative_run_dir" ] \
  || die "run.env changed the authoritative Phase 4 run directory"
# NodeConfig still carries an unrelated compatibility wallet C. The Phase 4
# process gate uses only A/B, so complete this isolated environment with C=A
# when C is absent instead of reading the checkout .env.
: "${TESTNET_GENESIS_WALLET_SEED_PHRASE_A:?Phase 4 wallet A is required}"
: "${TESTNET_GENESIS_WALLET_SEED_PHRASE_B:?Phase 4 wallet B is required}"
export TESTNET_GENESIS_WALLET_SEED_PHRASE_C="${TESTNET_GENESIS_WALLET_SEED_PHRASE_C:-$TESTNET_GENESIS_WALLET_SEED_PHRASE_A}"
export MIDGARD_DOTENV_MODE=disabled
# Reused node inputs may describe an earlier deployment. Fresh isolated
# bootstrap must derive identity only from this run and its newly created
# nonce/manifest; stale coordinates must never redirect output or select an
# existing one-shot UTxO.
unset L1_PROVIDER_FAILOVER HUB_ORACLE_ONE_SHOT_TX_HASH HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX
unset MIDGARD_DEPLOYMENT_MANIFEST_PATH MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH
export NETWORK=Custom L1_PROVIDER=Kupmios RUN_GENESIS_ON_STARTUP=false MIN_FEE_A=0 MIN_FEE_B=0
upsert_private_env MIN_FEE_A 0 "$node_env"
upsert_private_env MIN_FEE_B 0 "$node_env"
export L1_OGMIOS_KEY="http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT" L1_KUPO_KEY="http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT"
export POSTGRES_HOST=127.0.0.1 POSTGRES_PORT="$MIDGARD_PHASE4_POSTGRES_PORT" POSTGRES_USER="$MIDGARD_PHASE4_POSTGRES_USER" POSTGRES_PASSWORD="$MIDGARD_PHASE4_POSTGRES_PASSWORD" POSTGRES_DB="$MIDGARD_PHASE4_POSTGRES_DATABASE"
cd "$contract_root"
aiken build --env testnet
[ -s "$blueprint" ] || die "Aiken testnet build did not create plutus.json"
sha256sum "$blueprint" >"$MIDGARD_PHASE4_RUN_DIR/work/plutus.json.sha256"
export MIDGARD_REAL_BLUEPRINT_PATH="$blueprint"
cd "$node_root"
pnpm build
node dist/index.js db:migrate
MIDGARD_PHASE4_GENESIS_BOOTSTRAP=phase4-local-devnet-l2-genesis-v1 \
MIDGARD_PHASE4_PROCESS_TARGET=local-devnet \
  node dist/index.js phase4-genesis-ledger --seed
nonce_output="$MIDGARD_PHASE4_RUN_DIR/work/hub-oracle-nonce.json"
node dist/index.js prepare-hub-oracle-one-shot-nonce --run-state "$run_state" --fresh-redeploy --fresh-redeploy-reason "isolated Phase 4 process devnet $MIDGARD_PHASE4_RUN_ID" --json >"$nonce_output"
# The CLI keeps human-readable progress logs on stdout even with --json. Keep
# the complete transcript for evidence, but parse only the final JSON object.
nonce_json="$MIDGARD_PHASE4_RUN_DIR/work/hub-oracle-nonce.parsed.json"
sed -n '/^[[:space:]]*{/,$p' "$nonce_output" >"$nonce_json"
tx_hash=$(jq -er '(.txHash // (.outRef | split("#")[0]))' "$nonce_json")
output_index=$(jq -er '(.outputIndex // (.outRef | split("#")[1] | tonumber))' "$nonce_json")
export HUB_ORACLE_ONE_SHOT_TX_HASH="$tx_hash" HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX="$output_index"
upsert_private_env HUB_ORACLE_ONE_SHOT_TX_HASH "$tx_hash" "$node_env"
upsert_private_env HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX "$output_index" "$node_env"
node dist/index.js deploy-reference-script-node-runtime --run-state "$run_state" --contract-deployment-info-output "$manifest"
export MIDGARD_DEPLOYMENT_MANIFEST_PATH="$manifest"
export MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH="$manifest"
phas_output="$MIDGARD_PHASE4_RUN_DIR/work/phas-registration.raw.log"
phas_json="$MIDGARD_PHASE4_RUN_DIR/work/phas-registration.json"
phas_transaction_body="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/phas-registration-transaction-body.json"
node dist/index.js register-phas-membership-reward-account \
  --contract-deployment-info-output "$manifest" \
  --registration-transaction-body-output "$phas_transaction_body" \
  --json >"$phas_output"
sed -n '/^[[:space:]]*{/,$p' "$phas_output" >"$phas_json"
phas_reward_address=$(jq -er '.rewardAddress | select(test("^stake(_test)?1"))' "$phas_json")
phas_script_hash=$(jq -er '.scriptHash | select(test("^[a-f0-9]{56}$"))' "$phas_json")
phas_registration_tx_hash=$(jq -er '.steps.phasRegistration.txHash | select(test("^[a-f0-9]{64}$"))' "$manifest")
[ -s "$phas_transaction_body" ] || die "PHAS registration transaction-body evidence is missing"
phas_transaction_body_sha=$(sha256_file "$phas_transaction_body")
jq -e \
  --arg rewardAddress "$phas_reward_address" \
  --arg scriptHash "$phas_script_hash" \
  --arg registrationTxHash "$phas_registration_tx_hash" \
  --arg transactionBodyArtifactSha256 "$phas_transaction_body_sha" \
  '.steps.phasRegistration.status == "complete" and
   .steps.phasRegistration.txHash == $registrationTxHash and
   .steps.phasRegistration.rewardAddress == $rewardAddress and
   .steps.phasRegistration.scriptHash == $scriptHash and
   .steps.phasRegistration.transactionBody.schemaVersion == "midgard-phas-registration-transaction-body-v1" and
   .steps.phasRegistration.transactionBody.txHash == $registrationTxHash and
   .steps.phasRegistration.transactionBody.artifactSha256 == $transactionBodyArtifactSha256 and
   .steps.phasRegistration.transactionBody.certificate == {kind:"stake_registration",index:0,count:1,credentialType:"script",scriptHash:$scriptHash} and
   .contracts.phasMembershipWithdraw.scriptHash == $scriptHash' "$manifest" >/dev/null \
  || die "deployment manifest did not persist the exact completed PHAS registration identity"
jq -e \
  --arg rewardAddress "$phas_reward_address" \
  --arg scriptHash "$phas_script_hash" \
  --arg registrationTxHash "$phas_registration_tx_hash" \
  '.status == "registration_submitted" and .rewardAddress == $rewardAddress and
   .scriptHash == $scriptHash and .txHash == $registrationTxHash and
   .transactionBody.txHash == $registrationTxHash and
   .transactionBody.certificate == {kind:"stake_registration",index:0,count:1,credentialType:"script",scriptHash:$scriptHash}' "$phas_json" >/dev/null \
  || die "fresh Phase 4 PHAS registration did not produce exact auditable evidence"
upsert_private_env MIDGARD_PHASE4_PHAS_REWARD_ADDRESS "$phas_reward_address" "$node_env"
upsert_private_env MIDGARD_PHASE4_PHAS_SCRIPT_HASH "$phas_script_hash" "$node_env"
upsert_private_env MIDGARD_PHASE4_PHAS_REGISTRATION_TX_HASH "$phas_registration_tx_hash" "$node_env"
node dist/index.js reconcile reference-scripts-complete --scope node-runtime --json
node dist/index.js init --contract-deployment-info-output "$manifest"
node dist/index.js register-active-operator
[ -s "$manifest" ] || die "protocol bootstrap did not create deployment manifest"
