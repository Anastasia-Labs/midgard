#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"

require_command curl
require_command docker
require_command jq
require_command node
require_command sha256sum

[ "${MIDGARD_PHASE4_PROCESS_ACCEPTANCE:-}" = "pipelined-commit-live-v1" ] \
  || die "T1 recovery requires the process-acceptance token"
[ "${MIDGARD_PHASE4_PROCESS_TARGET:-}" = "local-devnet" ] \
  || die "T1 recovery refuses every target except local-devnet"
[ "${MIDGARD_PHASE4_T1_ACCEPTANCE_TOKEN:-}" = "phase4-t1-local-canonical-advance-v1" ] \
  || die "T1 recovery requires its dedicated mutation token"
require_run_dir
# run.env is data, never an authorization source. Recheck after sourcing it so
# a modified run cannot replace any caller-supplied gate value.
[ "${MIDGARD_PHASE4_PROCESS_ACCEPTANCE:-}" = "pipelined-commit-live-v1" ] \
  || die "run.env changed the process-acceptance token"
[ "${MIDGARD_PHASE4_PROCESS_TARGET:-}" = "local-devnet" ] \
  || die "run.env changed the local-devnet target"
[ "${MIDGARD_PHASE4_T1_ACCEPTANCE_TOKEN:-}" = "phase4-t1-local-canonical-advance-v1" ] \
  || die "run.env changed the dedicated T1 mutation token"

scenario_label=${MIDGARD_PHASE4_SCENARIO_LABEL:-}
attempt_id=${MIDGARD_PHASE4_T1_ATTEMPT_ID:-}
abandoned_header_hash=${MIDGARD_PHASE4_T1_ABANDONED_HEADER_HASH:-}
abandoned_submitted_tx_hash=${MIDGARD_PHASE4_T1_ABANDONED_SUBMITTED_TX_HASH:-}
base_header_hash=${MIDGARD_PHASE4_T1_BASE_HEADER_HASH:-}
minimum_end_time_ms=${MIDGARD_PHASE4_T1_MINIMUM_END_TIME_MS:-}
expected_identity_sha=${MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256:-}
evidence_dir=${MIDGARD_PHASE4_T1_EVIDENCE_DIR:-}

[ -n "$scenario_label" ] || die "MIDGARD_PHASE4_SCENARIO_LABEL is required"
case "$attempt_id" in
  '' | *[!A-Za-z0-9_.-]*) die "T1 attempt id is missing or unsafe" ;;
esac
case "$abandoned_header_hash" in '' | *[!a-f0-9]*) die "abandoned L2 header hash must be lowercase hex" ;; esac
[ "${#abandoned_header_hash}" -eq 56 ] || die "abandoned L2 header hash must be exactly 56 lowercase hex"
case "$base_header_hash" in *[!a-f0-9]*) die "base L2 header hash must be lowercase hex" ;; esac
[ "${#base_header_hash}" -eq 56 ] || die "base L2 header hash must be exactly 56 lowercase hex"
case "$abandoned_submitted_tx_hash" in *[!a-f0-9]*) die "abandoned Cardano tx hash must be lowercase hex" ;; esac
[ "${#abandoned_submitted_tx_hash}" -eq 64 ] || die "abandoned Cardano tx hash must be exactly 64 lowercase hex"
case "$expected_identity_sha" in *[!a-f0-9]*) die "snapshot identity digest must be lowercase hex" ;; esac
[ "${#expected_identity_sha}" -eq 64 ] || die "snapshot identity digest must be exactly 64 lowercase hex"
case "$minimum_end_time_ms" in '' | *[!0-9]*) die "minimum end time must be a positive integer" ;; esac
[ "$minimum_end_time_ms" -gt 0 ] || die "minimum end time must be positive"
case "$evidence_dir" in
  "$MIDGARD_PHASE4_RUN_DIR"/*) ;;
  *) die "T1 evidence directory must be a fresh absolute child of the run directory" ;;
esac
[ ! -e "$evidence_dir" ] || die "refusing to reuse T1 per-attempt evidence directory"
mkdir -m 700 "$evidence_dir"

snapshot_dir="$MIDGARD_PHASE4_RUN_DIR/snapshots/matched-v1"
identity="$snapshot_dir/snapshot-identity.json"
[ -f "$snapshot_dir/SHA256SUMS" ] || die "matched snapshot is incomplete"
[ -f "$snapshot_dir/SNAPSHOT_SET_SHA256" ] || die "snapshot-set digest is missing"
[ -f "$snapshot_dir/SNAPSHOT_IDENTITY_SHA256" ] || die "snapshot identity digest is missing"
[ -f "$snapshot_dir/cardano-db.tar.gz" ] || die "Cardano snapshot archive is missing"
[ -f "$snapshot_dir/kupo.tar.gz" ] || die "Kupo snapshot archive is missing"
[ -f "$snapshot_dir/contract-deployment-info.json" ] || die "deployment manifest snapshot is missing"
[ -f "$snapshot_dir/acceptance.env" ] || die "acceptance env snapshot is missing"
(cd "$snapshot_dir" && sha256sum --check SHA256SUMS >/dev/null)
snapshot_set_sha=$(sha256_file "$snapshot_dir/SHA256SUMS")
[ "$snapshot_set_sha" = "$(cat "$snapshot_dir/SNAPSHOT_SET_SHA256")" ] \
  || die "snapshot-set checksum mismatch"
actual_identity_sha=$(sha256_file "$identity")
[ "$actual_identity_sha" = "$(cat "$snapshot_dir/SNAPSHOT_IDENTITY_SHA256")" ] \
  || die "snapshot identity checksum mismatch"
[ "$actual_identity_sha" = "$expected_identity_sha" ] \
  || die "T1 request is not bound to this matched snapshot identity"

manifest_sha=$(sha256_file "$snapshot_dir/contract-deployment-info.json")
phas_registration_proof_sha=$(sha256_file "$snapshot_dir/phas-registration-proof.json")
jq -e \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg postgresDatabase "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
  --arg deploymentManifestSha256 "$manifest_sha" \
  --arg cardanoImage "$PHASE4_CARDANO_NODE_IMAGE" \
  --arg ogmiosImage "$PHASE4_OGMIOS_IMAGE" \
  --arg kupoImage "$PHASE4_KUPO_IMAGE" \
  --arg postgresImage "$PHASE4_POSTGRES_IMAGE" \
  --arg phasRegistrationProofSha256 "$phas_registration_proof_sha" \
  '.schemaVersion == "midgard-phase4-matched-snapshot-identity-v1" and
   .composeProject == $composeProject and .networkMagic == $networkMagic and
   .postgresDatabase == $postgresDatabase and .deploymentManifestSha256 == $deploymentManifestSha256 and
   .images.cardanoNode.ref == $cardanoImage and .images.ogmios.ref == $ogmiosImage and
   .images.kupo.ref == $kupoImage and .images.postgres.ref == $postgresImage and
   (.blueprintSha256 | test("^[a-f0-9]{64}$")) and
   .artifacts.phasRegistrationProofSha256 == $phasRegistrationProofSha256 and
   .phasRegistration.readOnly == true and .phasRegistration.registered == true and
   .kupoCheckpoint == .cardanoTip.slot' "$identity" >/dev/null \
  || die "matched snapshot identity does not bind this run and pinned runtime"

node_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
dist="$node_root/dist/index.js"
[ -f "$dist" ] || die "built Midgard node CLI is missing"
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

postgres_container=$(compose ps -q postgres)
[ -n "$postgres_container" ] || die "isolated Postgres must already be running"
[ "$(docker inspect --format '{{.State.Running}}' "$postgres_container")" = true ] \
  || die "isolated Postgres is not running"

journal_tables='--table=public.pending_block_finalizations --table=public.pending_block_finalization_deposits --table=public.pending_block_finalization_forced_transactions --table=public.pending_block_finalization_withdrawals --table=public.pending_block_finalization_txs --table=public.pending_block_finalization_utxos --table=public.pending_block_finalization_transition_trace --table=public.pending_block_finalization_event_to_step'
dump_journal() {
  output=$1
  # shellcheck disable=SC2086
  compose exec -T postgres pg_dump \
    --username "$MIDGARD_PHASE4_POSTGRES_USER" \
    --dbname "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
    --data-only --inserts --rows-per-insert=1 --no-owner --no-privileges \
    $journal_tables >"$output"
  chmod 600 "$output"
}
dump_journal "$evidence_dir/journal-before.sql"
journal_sha_before=$(sha256_file "$evidence_dir/journal-before.sql")

cardano_tip() {
  docker run --rm --user "$(id -u):$(id -g)" \
    --volume "$MIDGARD_PHASE4_RUN_DIR:/run" \
    --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" latest query tip \
    --socket-path /run/cardano/ipc/node.socket \
    --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC"
}

wait_synchronized_providers() {
  attempts=${1:-240}
  while [ "$attempts" -gt 0 ]; do
    synchronized_tip=$(cardano_tip 2>/dev/null || true)
    synchronized_slot=$(printf '%s' "$synchronized_tip" | jq -er '.slot' 2>/dev/null || true)
    synchronized_hash=$(printf '%s' "$synchronized_tip" | jq -er '.hash' 2>/dev/null || true)
    kupo_health=$(curl --fail --silent --show-error "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 2>/dev/null || true)
    synchronized_kupo=$(printf '%s' "$kupo_health" | parse_kupo_checkpoint 2>/dev/null || true)
    if [ -n "$synchronized_slot" ] && [ -n "$synchronized_hash" ] && \
       [ "$synchronized_slot" = "$synchronized_kupo" ]; then
      return 0
    fi
    attempts=$((attempts - 1))
    sleep 1
  done
  die "Cardano and Kupo did not reach an exact synchronized checkpoint"
}

export MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256="$expected_identity_sha"
export MIDGARD_PHASE4_T1_ATTEMPT_ID="$attempt_id"

wait_synchronized_providers 240
node "$dist" phase4-t1-probe \
  --snapshot-identity-sha256 "$expected_identity_sha" \
  --attempt-id "$attempt_id" \
  --expected-present-header-hash "$abandoned_header_hash" \
  --evidence-out "$evidence_dir/pre-rollback-probe.json" \
  >"$evidence_dir/pre-rollback-probe.raw.log" 2>&1

# This is the only destructive section: restore Cardano and Kupo from their
# matched archives while keeping Postgres, its container, and every journal
# byte untouched. The Midgard listen process is already stopped by the caller.
compose_quiet stop kupo ogmios cardano-node
restore_chain_dir() {
  archive=$1
  target=$2
  mkdir -p "$target"
  docker run --rm --entrypoint sh \
    --volume "$target:/target" \
    --volume "$snapshot_dir:/snapshot:ro" \
    "$PHASE4_POSTGRES_IMAGE" \
    -ec "rm -rf /target/* /target/.[!.]* /target/..?* 2>/dev/null || true; tar -xzf /snapshot/$archive -C /target"
}
restore_chain_dir cardano-db.tar.gz "$MIDGARD_PHASE4_RUN_DIR/cardano/db"
restore_chain_dir kupo.tar.gz "$MIDGARD_PHASE4_RUN_DIR/kupo"

MIDGARD_PHASE4_BLOCK_PRODUCER=false compose_quiet up --detach --force-recreate cardano-node ogmios kupo
grant_cardano_socket_access
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
frozen_slot=$(jq -er '.cardanoTip.slot' "$identity")
frozen_hash=$(jq -er '.cardanoTip.hash' "$identity")
frozen_kupo=$(jq -er '.kupoCheckpoint' "$identity")
restored_tip=$(cardano_tip)
[ "$(printf '%s' "$restored_tip" | jq -er '.slot')" = "$frozen_slot" ] \
  || die "restored Cardano slot does not match the frozen snapshot"
[ "$(printf '%s' "$restored_tip" | jq -er '.hash')" = "$frozen_hash" ] \
  || die "restored Cardano hash does not match the frozen snapshot"
attempts=180
while [ "$attempts" -gt 0 ]; do
  restored_kupo=$(curl --fail --silent --show-error "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" | parse_kupo_checkpoint)
  [ "$restored_kupo" -le "$frozen_kupo" ] || die "restored Kupo advanced beyond the frozen snapshot"
  [ "$restored_kupo" = "$frozen_kupo" ] && break
  attempts=$((attempts - 1))
  sleep 1
done
[ "$attempts" -gt 0 ] || die "Kupo did not catch up to the frozen snapshot checkpoint"

"$script_dir/phas-registration-preflight.sh" >"$evidence_dir/restored-phas-registration-proof.json"
cmp -s "$snapshot_dir/phas-registration-proof.json" "$evidence_dir/restored-phas-registration-proof.json" \
  || die "T1 chain restore changed the frozen PHAS registration proof"

MIDGARD_PHASE4_BLOCK_PRODUCER=true compose_quiet up --detach --force-recreate cardano-node
grant_cardano_socket_access
compose_quiet restart ogmios
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
compose_quiet restart kupo
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
wait_synchronized_providers 240
[ "$synchronized_slot" -gt "$frozen_slot" ] \
  || die "resumed producer/providers did not advance beyond the frozen checkpoint"

node "$dist" phase4-t1-probe \
  --snapshot-identity-sha256 "$expected_identity_sha" \
  --attempt-id "$attempt_id" \
  --expected-tip-header-hash "$base_header_hash" \
  --expected-absent-header-hash "$abandoned_header_hash" \
  --evidence-out "$evidence_dir/post-rollback-probe.json" \
  >"$evidence_dir/post-rollback-probe.raw.log" 2>&1

node "$dist" phase4-t1-advance \
  --snapshot-identity-sha256 "$expected_identity_sha" \
  --attempt-id "$attempt_id" \
  --expected-base-header-hash "$base_header_hash" \
  --abandoned-header-hash "$abandoned_header_hash" \
  --minimum-end-time-ms "$minimum_end_time_ms" \
  --evidence-out "$evidence_dir/canonical-advance.json" \
  >"$evidence_dir/canonical-advance.raw.log" 2>&1

recovered_tip_header_hash=$(jq -er '.recoveredTipHeaderHash | select(test("^[a-f0-9]{56}$"))' "$evidence_dir/canonical-advance.json")
canonical_advance_tx_hash=$(jq -er '.submittedTxHash | select(test("^[a-f0-9]{64}$"))' "$evidence_dir/canonical-advance.json")
jq -e \
  --arg snapshotIdentitySha256 "$expected_identity_sha" \
  --arg attemptId "$attempt_id" \
  --arg abandonedHeaderHash "$abandoned_header_hash" \
  --arg baseHeaderHash "$base_header_hash" \
  '.schemaVersion == "midgard-phase4-t1-canonical-advance-v1" and
   .snapshotIdentitySha256 == $snapshotIdentitySha256 and .attemptId == $attemptId and
   .abandonedHeaderHash == $abandonedHeaderHash and
   .before.canonicalTip.headerHash == $baseHeaderHash and
   .after.canonicalTip.headerHash == .recoveredTipHeaderHash and
   .after.canonicalTip.prevHeaderHash == $baseHeaderHash and
   .invariants.rootsPreserved == true and .invariants.transitionIsEmpty == true and
   (.after.canonicalHeaderHashes | index($abandonedHeaderHash) | not)' \
  "$evidence_dir/canonical-advance.json" >/dev/null \
  || die "canonical advance evidence does not prove the T1 F invariants"

wait_synchronized_providers 240
node "$dist" phase4-t1-probe \
  --snapshot-identity-sha256 "$expected_identity_sha" \
  --attempt-id "$attempt_id" \
  --expected-tip-header-hash "$recovered_tip_header_hash" \
  --expected-absent-header-hash "$abandoned_header_hash" \
  --evidence-out "$evidence_dir/final-probe.json" \
  >"$evidence_dir/final-probe.raw.log" 2>&1

[ "$(compose ps -q postgres)" = "$postgres_container" ] \
  || die "T1 recovery replaced the Postgres container"
[ "$(docker inspect --format '{{.State.Running}}' "$postgres_container")" = true ] \
  || die "T1 recovery stopped the Postgres container"
dump_journal "$evidence_dir/journal-after.sql"
journal_sha_after=$(sha256_file "$evidence_dir/journal-after.sql")
cmp -s "$evidence_dir/journal-before.sql" "$evidence_dir/journal-after.sql" \
  || die "T1 chain recovery changed the durable pending-finalization journal"
[ "$journal_sha_after" = "$journal_sha_before" ] \
  || die "T1 journal digest changed during chain recovery"

chmod -R go-rwx "$evidence_dir"
jq -cS -n \
  --arg schemaVersion midgard-phase4-t1-recovery-attestation-v1 \
  --arg scenarioLabel "$scenario_label" \
  --arg attemptId "$attempt_id" \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg snapshotSetSha256 "$snapshot_set_sha" \
  --arg snapshotIdentitySha256 "$expected_identity_sha" \
  --arg abandonedHeaderHash "$abandoned_header_hash" \
  --arg abandonedSubmittedTxHash "$abandoned_submitted_tx_hash" \
  --arg baseHeaderHash "$base_header_hash" \
  --arg recoveredTipHeaderHash "$recovered_tip_header_hash" \
  --arg canonicalAdvanceTxHash "$canonical_advance_tx_hash" \
  --arg journalSha256Before "$journal_sha_before" \
  --arg journalSha256After "$journal_sha_after" \
  --argjson cardanoTip "$synchronized_tip" \
  --argjson kupoCheckpoint "$synchronized_kupo" \
  '{schemaVersion:$schemaVersion,scenarioLabel:$scenarioLabel,attemptId:$attemptId,composeProject:$composeProject,networkMagic:$networkMagic,snapshotSetSha256:$snapshotSetSha256,snapshotIdentitySha256:$snapshotIdentitySha256,abandonedHeaderHash:$abandonedHeaderHash,abandonedSubmittedTxHash:$abandonedSubmittedTxHash,baseHeaderHash:$baseHeaderHash,recoveredTipHeaderHash:$recoveredTipHeaderHash,canonicalAdvanceTxHash:$canonicalAdvanceTxHash,journalSha256Before:$journalSha256Before,journalSha256After:$journalSha256After,cardanoTip:{slot:$cardanoTip.slot,hash:$cardanoTip.hash},kupoCheckpoint:$kupoCheckpoint}'
