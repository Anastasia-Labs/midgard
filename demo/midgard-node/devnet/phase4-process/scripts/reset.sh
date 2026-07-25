#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command curl
require_command jq
require_command sha256sum
require_run_dir
scenario_label=${MIDGARD_PHASE4_SCENARIO_LABEL:-}
[ -n "$scenario_label" ] || die "MIDGARD_PHASE4_SCENARIO_LABEL is required"
snapshot_dir=${MIDGARD_PHASE4_SNAPSHOT_DIR:-$MIDGARD_PHASE4_RUN_DIR/snapshots/matched-v1}
[ "$snapshot_dir" = "$MIDGARD_PHASE4_RUN_DIR/snapshots/matched-v1" ] || die "snapshot override is not authorized for the run-scoped matched snapshot"
[ -f "$snapshot_dir/SHA256SUMS" ] || die "matched snapshot is incomplete"
[ -f "$snapshot_dir/config.tar.gz" ] || die "matched snapshot config archive is missing"
[ -f "$snapshot_dir/genesis.tar.gz" ] || die "matched snapshot genesis archive is missing"
[ -f "$snapshot_dir/acceptance.env" ] || die "matched snapshot acceptance env is missing"
[ -f "$snapshot_dir/phas-registration-proof.json" ] || die "matched snapshot PHAS registration proof is missing"
[ -f "$snapshot_dir/phas-registration-transaction-body.json" ] || die "matched snapshot PHAS registration transaction body is missing"
[ -f "$snapshot_dir/snapshot-identity.json" ] || die "matched snapshot identity is missing"
[ -f "$snapshot_dir/SNAPSHOT_IDENTITY_SHA256" ] || die "matched snapshot identity checksum is missing"
(cd "$snapshot_dir" && sha256sum --check SHA256SUMS >/dev/null)
expected_set=$(cat "$snapshot_dir/SNAPSHOT_SET_SHA256")
actual_set=$(sha256_file "$snapshot_dir/SHA256SUMS")
[ "$expected_set" = "$actual_set" ] || die "snapshot-set checksum mismatch"
expected_identity_sha=$(cat "$snapshot_dir/SNAPSHOT_IDENTITY_SHA256")
actual_identity_sha=$(sha256_file "$snapshot_dir/snapshot-identity.json")
[ "$expected_identity_sha" = "$actual_identity_sha" ] || die "snapshot identity checksum mismatch"

manifest_sha=$(sha256_file "$snapshot_dir/contract-deployment-info.json")
phas_registration_proof_sha=$(sha256_file "$snapshot_dir/phas-registration-proof.json")
identity="$snapshot_dir/snapshot-identity.json"
jq -e \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg postgresDatabase "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
  --arg deploymentManifestSha256 "$manifest_sha" \
  --arg cardanoImage "$PHASE4_CARDANO_NODE_IMAGE" --arg ogmiosImage "$PHASE4_OGMIOS_IMAGE" \
  --arg kupoImage "$PHASE4_KUPO_IMAGE" --arg postgresImage "$PHASE4_POSTGRES_IMAGE" \
  --arg phasRegistrationProofSha256 "$phas_registration_proof_sha" \
  '.schemaVersion == "midgard-phase4-matched-snapshot-identity-v1" and
   .composeProject == $composeProject and .networkMagic == $networkMagic and
   .postgresDatabase == $postgresDatabase and .deploymentManifestSha256 == $deploymentManifestSha256 and
   .images.cardanoNode.ref == $cardanoImage and .images.ogmios.ref == $ogmiosImage and
   .images.kupo.ref == $kupoImage and .images.postgres.ref == $postgresImage and
   (.blueprintSha256 | test("^[a-f0-9]{64}$")) and
   .artifacts.phasRegistrationProofSha256 == $phasRegistrationProofSha256 and
   .phasRegistration.schemaVersion == "midgard-phase4-phas-registration-proof-v1" and
   .phasRegistration.readOnly == true and .phasRegistration.registered == true and
   .kupoCheckpoint == .cardanoTip.slot' "$identity" >/dev/null \
  || die "matched snapshot identity does not bind this run and pinned runtime"

transaction_body_sha=$(sha256_file "$snapshot_dir/phas-registration-transaction-body.json")
jq -e \
  --arg transactionBodySha256 "$transaction_body_sha" \
  --slurpfile phasRegistration "$snapshot_dir/phas-registration-proof.json" \
  '.phasRegistration == $phasRegistration[0] and
   .phasRegistration.transactionBody.artifactSha256 == $transactionBodySha256 and
   .phasRegistration.transactionBody.cardanoCliTxHash == .phasRegistration.registrationTxHash and
   .phasRegistration.rewardAddressBase16 == ("f0" + .phasRegistration.scriptHash)' \
  "$identity" >/dev/null \
  || die "snapshot PHAS proof, canonical identity, and transaction body are not exactly bound"

node_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
current_source_sha=$(tree_sha256 "$node_root/src")
current_dist_sha=$(tree_sha256 "$node_root/dist")
current_genesis_sha=$(tree_sha256 "$MIDGARD_PHASE4_RUN_DIR/genesis")
current_config_sha=$(tree_sha256 "$MIDGARD_PHASE4_RUN_DIR/config")
current_acceptance_env_sha=$(sha256_file "$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env")
current_compose_sha=$(sha256_file "$compose_file")
current_phase4_assets_sha=$(tree_sha256 "$phase4_root")
jq -e \
  --arg sourceSha256 "$current_source_sha" --arg distSha256 "$current_dist_sha" \
  --arg genesisSha256 "$current_genesis_sha" --arg configSha256 "$current_config_sha" \
  --arg acceptanceEnvSha256 "$current_acceptance_env_sha" --arg composeSha256 "$current_compose_sha" \
  --arg phase4AssetsSha256 "$current_phase4_assets_sha" \
  '.artifacts.sourceSha256 == $sourceSha256 and .artifacts.distSha256 == $distSha256 and
   .artifacts.genesisSha256 == $genesisSha256 and .artifacts.configSha256 == $configSha256 and
   .artifacts.acceptanceEnvSha256 == $acceptanceEnvSha256 and .artifacts.composeSha256 == $composeSha256 and
   .artifacts.phase4AssetsSha256 == $phase4AssetsSha256' "$identity" >/dev/null \
  || die "source/dist/genesis/config/acceptance/Phase4 assets do not match the frozen snapshot identity"
for image in "$PHASE4_CARDANO_NODE_IMAGE" "$PHASE4_OGMIOS_IMAGE" "$PHASE4_KUPO_IMAGE" "$PHASE4_POSTGRES_IMAGE"; do
  image_id "$image" >/dev/null
done
jq -e \
  --arg cardanoId "$(image_id "$PHASE4_CARDANO_NODE_IMAGE")" \
  --arg ogmiosId "$(image_id "$PHASE4_OGMIOS_IMAGE")" \
  --arg kupoId "$(image_id "$PHASE4_KUPO_IMAGE")" \
  --arg postgresId "$(image_id "$PHASE4_POSTGRES_IMAGE")" \
  '.images.cardanoNode.id == $cardanoId and .images.ogmios.id == $ogmiosId and
   .images.kupo.id == $kupoId and .images.postgres.id == $postgresId' "$identity" >/dev/null \
  || die "effective pinned image IDs do not match the frozen snapshot identity"

# Only after every snapshot checksum and identity check succeeds may the
# reset stop services or replace any durable state.
compose_quiet stop kupo ogmios cardano-node postgres

restore_dir() {
  archive=$1
  target=$2
  mkdir -p "$target"
  docker run --rm --entrypoint sh --volume "$target:/target" --volume "$snapshot_dir:/snapshot:ro" "$PHASE4_POSTGRES_IMAGE" -ec "rm -rf /target/* /target/.[!.]* /target/..?* 2>/dev/null || true; tar -xzf /snapshot/$archive -C /target"
}
restore_dir cardano-db.tar.gz "$MIDGARD_PHASE4_RUN_DIR/cardano/db"
restore_dir kupo.tar.gz "$MIDGARD_PHASE4_RUN_DIR/kupo"
restore_dir postgres.tar.gz "$MIDGARD_PHASE4_RUN_DIR/postgres"
restore_dir config.tar.gz "$MIDGARD_PHASE4_RUN_DIR/config"
restore_dir genesis.tar.gz "$MIDGARD_PHASE4_RUN_DIR/genesis"
cp "$snapshot_dir/contract-deployment-info.json" "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
cp "$snapshot_dir/phas-registration-transaction-body.json" "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/phas-registration-transaction-body.json"
cp "$snapshot_dir/acceptance.env" "$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env"
if [ -f "$snapshot_dir/deployment-run-state.json" ]; then cp "$snapshot_dir/deployment-run-state.json" "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/deployment-run-state.json"; fi

MIDGARD_PHASE4_BLOCK_PRODUCER=false compose_quiet up --detach --force-recreate cardano-node ogmios kupo postgres
grant_cardano_socket_access
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
cardano_image="$PHASE4_CARDANO_NODE_IMAGE"
frozen_slot=$(jq -er '.cardanoTip.slot' "$identity")
frozen_hash=$(jq -er '.cardanoTip.hash' "$identity")
frozen_kupo=$(jq -er '.kupoCheckpoint' "$identity")
[ "$frozen_slot" = "$frozen_kupo" ] || die "snapshot identity Cardano/Kupo checkpoint mismatch"
jq -e \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg postgresDatabase "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
  --arg deploymentManifestSha256 "$manifest_sha" \
  --arg phasRegistrationProofSha256 "$phas_registration_proof_sha" \
  '.schemaVersion == "midgard-phase4-matched-snapshot-identity-v1" and
   .composeProject == $composeProject and
   .networkMagic == $networkMagic and
   .postgresDatabase == $postgresDatabase and
   .deploymentManifestSha256 == $deploymentManifestSha256 and
   (.blueprintSha256 | test("^[a-f0-9]{64}$")) and
   .artifacts.phasRegistrationProofSha256 == $phasRegistrationProofSha256 and
   .phasRegistration.readOnly == true and .phasRegistration.registered == true and
   .kupoCheckpoint == .cardanoTip.slot' "$identity" >/dev/null || die "restored Cardano/Kupo state does not match the frozen snapshot identity"

# Observer mode holds the restored Cardano tip fixed while Kupo catches up.
attempts=180
while [ "$attempts" -gt 0 ]; do
  tip=$(docker run --rm --user "$(id -u):$(id -g)" --volume "$MIDGARD_PHASE4_RUN_DIR:/run" --entrypoint cardano-cli "$cardano_image" latest query tip --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC")
  observed_slot=$(printf '%s' "$tip" | jq -er '.slot')
  observed_hash=$(printf '%s' "$tip" | jq -er '.hash')
  [ "$observed_slot" = "$frozen_slot" ] || die "restored Cardano slot does not match frozen snapshot"
  [ "$observed_hash" = "$frozen_hash" ] || die "restored Cardano hash does not match frozen snapshot"
  kupo_health=$(curl --fail --silent --show-error "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health")
  observed_kupo=$(printf '%s' "$kupo_health" | parse_kupo_checkpoint)
  [ "$observed_kupo" -le "$frozen_kupo" ] || die "restored Kupo checkpoint advanced beyond frozen snapshot"
  [ "$observed_kupo" = "$frozen_kupo" ] && break
  attempts=$((attempts - 1))
  sleep 1
done
[ "$attempts" -gt 0 ] || die "Kupo did not catch up to the frozen snapshot checkpoint"

restored_phas_proof="$MIDGARD_PHASE4_RUN_DIR/work/reset-phas-registration-proof.json"
restored_phas_proof_pending="$MIDGARD_PHASE4_RUN_DIR/work/reset-phas-registration-proof.pending.json"
rm -f "$restored_phas_proof" "$restored_phas_proof_pending"
"$script_dir/phas-registration-preflight.sh" >"$restored_phas_proof_pending"
cmp -s "$snapshot_dir/phas-registration-proof.json" "$restored_phas_proof_pending" \
  || die "restored PHAS registration proof does not match the frozen read-only proof"
mv "$restored_phas_proof_pending" "$restored_phas_proof"

attestation_path="$MIDGARD_PHASE4_RUN_DIR/work/reset-attestation.json"
attestation_pending_path="$MIDGARD_PHASE4_RUN_DIR/work/reset-attestation.pending.json"
rm -f "$attestation_path" "$attestation_pending_path"
jq -n \
  --arg schemaVersion midgard-phase4-local-devnet-reset-attestation-v1 \
  --arg scenarioLabel "$scenario_label" \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg postgresDatabase "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
  --arg deploymentManifestSha256 "$manifest_sha" \
  --arg snapshotSetSha256 "$expected_set" \
  --arg snapshotIdentitySha256 "$expected_identity_sha" \
  --arg phasRegistrationProofSha256 "$phas_registration_proof_sha" \
  --argjson phasRegistration "$(cat "$restored_phas_proof")" \
  --argjson tip "$tip" --argjson kupoCheckpoint "$observed_kupo" \
  '{schemaVersion:$schemaVersion,scenarioLabel:$scenarioLabel,composeProject:$composeProject,networkMagic:$networkMagic,postgresDatabase:$postgresDatabase,deploymentManifestSha256:$deploymentManifestSha256,snapshotSetSha256:$snapshotSetSha256,snapshotIdentitySha256:$snapshotIdentitySha256,phasRegistrationProofSha256:$phasRegistrationProofSha256,phasRegistration:$phasRegistration,cardanoTip:{slot:$tip.slot,hash:$tip.hash},kupoCheckpoint:$kupoCheckpoint}' >"$attestation_pending_path"

# Resume the producer only after the frozen identity is attested. Recreating the
# producer replaces the socket inode, so reconnect Ogmios first and Kupo second;
# neither provider may continue against its pre-recreation connection. Do not
# print the frozen identity attestation until the producer and Kupo have both
# advanced strictly beyond the frozen checkpoint.
MIDGARD_PHASE4_BLOCK_PRODUCER=true compose_quiet up --detach --force-recreate cardano-node
grant_cardano_socket_access
compose_quiet restart ogmios
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
compose_quiet restart kupo
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
attempts=180
while [ "$attempts" -gt 0 ]; do
  resumed_tip=$(docker run --rm --user "$(id -u):$(id -g)" --volume "$MIDGARD_PHASE4_RUN_DIR:/run" --entrypoint cardano-cli "$cardano_image" latest query tip --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC")
  resumed_slot=$(printf '%s' "$resumed_tip" | jq -er '.slot')
  resumed_kupo_health=$(curl --fail --silent --show-error "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health")
  resumed_kupo=$(printf '%s' "$resumed_kupo_health" | parse_kupo_checkpoint)
  [ "$resumed_slot" -gt "$frozen_slot" ] && [ "$resumed_kupo" -gt "$frozen_kupo" ] && break
  attempts=$((attempts - 1))
  sleep 1
done
[ "$attempts" -gt 0 ] || die "resumed producer and Kupo did not advance strictly beyond the frozen checkpoint"
mv "$attestation_pending_path" "$attestation_path"
cat "$attestation_path"
