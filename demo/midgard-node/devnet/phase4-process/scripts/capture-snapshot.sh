#!/bin/sh
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command curl
require_command jq
require_command sha256sum
require_run_dir
[ -s "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json" ] || die "deployment manifest is missing"
[ -s "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/phas-registration-transaction-body.json" ] || die "PHAS registration transaction body is missing"
[ -s "$MIDGARD_PHASE4_RUN_DIR/work/plutus.json.sha256" ] || die "Aiken blueprint checksum is missing"
[ -s "$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env" ] || die "acceptance env is missing"
snapshot_dir="$MIDGARD_PHASE4_RUN_DIR/snapshots/matched-v1"
[ ! -e "$snapshot_dir" ] || die "refusing to overwrite matched snapshot"
mkdir -p "$snapshot_dir"

cardano_image="$PHASE4_CARDANO_NODE_IMAGE"
node_root=$(CDPATH= cd -- "$phase4_root/../.." && pwd)
source_sha=$(tree_sha256 "$node_root/src")
dist_sha=$(tree_sha256 "$node_root/dist")
genesis_sha=$(tree_sha256 "$MIDGARD_PHASE4_RUN_DIR/genesis")
config_sha=$(tree_sha256 "$MIDGARD_PHASE4_RUN_DIR/config")
acceptance_env_sha=$(sha256_file "$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env")
compose_sha=$(sha256_file "$compose_file")
phase4_assets_sha=$(tree_sha256 "$phase4_root")
# Replace the producer with the same image in observer mode before selecting
# the canonical checkpoint. This prevents another block from racing archive
# capture after the identity probe.
compose stop cardano-node
MIDGARD_PHASE4_BLOCK_PRODUCER=false compose up --detach --force-recreate cardano-node
grant_cardano_socket_access
# Recreating cardano-node replaces the socket inode. The already-running
# Ogmios process does not reliably reconnect to that new socket, and Kupo must
# reconnect only after Ogmios is healthy. Restart both bridges in dependency
# order before selecting the frozen checkpoint.
compose restart ogmios
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_OGMIOS_PORT/health" 180
compose restart kupo
wait_http "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" 180
attempts=120
while [ "$attempts" -gt 0 ]; do
  docker run --rm --user "$(id -u):$(id -g)" --volume "$MIDGARD_PHASE4_RUN_DIR:/run" --entrypoint cardano-cli "$cardano_image" latest query tip --socket-path /run/cardano/ipc/node.socket --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC" >"$snapshot_dir/cardano-tip.json"
  curl --fail --silent --show-error "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/health" >"$snapshot_dir/kupo-health.json"
  cardano_slot=$(jq -er '.slot | select(type == "number" and . >= 0)' "$snapshot_dir/cardano-tip.json")
  kupo_checkpoint=$(parse_kupo_checkpoint <"$snapshot_dir/kupo-health.json")
  [ "$cardano_slot" = "$kupo_checkpoint" ] && break
  attempts=$((attempts - 1))
  sleep 1
done
[ "$attempts" -gt 0 ] || die "Cardano tip and Kupo checkpoint did not converge before snapshot"
phas_registration_proof="$snapshot_dir/phas-registration-proof.json"
"$script_dir/phas-registration-preflight.sh" >"$phas_registration_proof"
phas_registration_proof_sha=$(sha256_file "$phas_registration_proof")
jq -e \
  --argjson cardanoSlot "$cardano_slot" \
  '.schemaVersion == "midgard-phase4-phas-registration-proof-v1" and
   .readOnly == true and .registered == true and
   .transactionBody.cardanoCliTxHash == .registrationTxHash and
   .transactionBody.certificate.scriptHash == .scriptHash and
   .rewardAddressBase16 == ("f0" + .scriptHash) and
   .confirmation.slot <= $cardanoSlot and .observedAtTip.slot == $cardanoSlot' \
  "$phas_registration_proof" >/dev/null \
  || die "PHAS registration proof is not bound to the frozen Cardano tip"
manifest_sha=$(sha256_file "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json")
blueprint_sha=$(awk 'NR == 1 { print $1 }' "$MIDGARD_PHASE4_RUN_DIR/work/plutus.json.sha256")
[ "${#blueprint_sha}" -eq 64 ] || die "Aiken blueprint checksum is invalid"
cardano_image_id=$(image_id "$PHASE4_CARDANO_NODE_IMAGE")
ogmios_image_id=$(image_id "$PHASE4_OGMIOS_IMAGE")
kupo_image_id=$(image_id "$PHASE4_KUPO_IMAGE")
postgres_image_id=$(image_id "$PHASE4_POSTGRES_IMAGE")
jq -n \
  --arg schemaVersion midgard-phase4-matched-snapshot-identity-v2 \
  --arg composeProject "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg postgresDatabase "$MIDGARD_PHASE4_POSTGRES_DATABASE" \
  --arg deploymentManifestSha256 "$manifest_sha" \
  --arg blueprintSha256 "$blueprint_sha" \
  --arg cardanoImage "$PHASE4_CARDANO_NODE_IMAGE" \
  --arg ogmiosImage "$PHASE4_OGMIOS_IMAGE" \
  --arg kupoImage "$PHASE4_KUPO_IMAGE" \
  --arg postgresImage "$PHASE4_POSTGRES_IMAGE" \
  --arg cardanoImageId "$cardano_image_id" \
  --arg ogmiosImageId "$ogmios_image_id" \
  --arg kupoImageId "$kupo_image_id" \
  --arg postgresImageId "$postgres_image_id" \
  --arg sourceSha256 "$source_sha" --arg distSha256 "$dist_sha" \
  --arg genesisSha256 "$genesis_sha" --arg configSha256 "$config_sha" \
  --arg acceptanceEnvSha256 "$acceptance_env_sha" --arg composeSha256 "$compose_sha" \
  --arg phase4AssetsSha256 "$phase4_assets_sha" \
  --arg phasRegistrationProofSha256 "$phas_registration_proof_sha" \
  --argjson phasRegistration "$(cat "$phas_registration_proof")" \
  --argjson tip "$(cat "$snapshot_dir/cardano-tip.json")" \
  --argjson kupoCheckpoint "$kupo_checkpoint" \
  '{schemaVersion:$schemaVersion,composeProject:$composeProject,networkMagic:$networkMagic,postgresDatabase:$postgresDatabase,deploymentManifestSha256:$deploymentManifestSha256,blueprintSha256:$blueprintSha256,images:{cardanoNode:{ref:$cardanoImage,id:$cardanoImageId},ogmios:{ref:$ogmiosImage,id:$ogmiosImageId},kupo:{ref:$kupoImage,id:$kupoImageId},postgres:{ref:$postgresImage,id:$postgresImageId}},artifacts:{sourceSha256:$sourceSha256,distSha256:$distSha256,genesisSha256:$genesisSha256,configSha256:$configSha256,acceptanceEnvSha256:$acceptanceEnvSha256,composeSha256:$composeSha256,phase4AssetsSha256:$phase4AssetsSha256,phasRegistrationProofSha256:$phasRegistrationProofSha256},phasRegistration:$phasRegistration,cardanoTip:{slot:$tip.slot,hash:$tip.hash},kupoCheckpoint:$kupoCheckpoint}' \
  | jq -cS . >"$snapshot_dir/snapshot-identity.json"

# Freeze every durable participant before copying any of them.
compose stop kupo ogmios postgres
compose stop cardano-node
archive_dir() {
  source_dir=$1
  output=$2
  docker run --rm --entrypoint tar --volume "$source_dir:/source:ro" --volume "$snapshot_dir:/snapshot" "$PHASE4_POSTGRES_IMAGE" -czf "/snapshot/$output" -C /source .
}
archive_dir "$MIDGARD_PHASE4_RUN_DIR/cardano/db" cardano-db.tar.gz
archive_dir "$MIDGARD_PHASE4_RUN_DIR/kupo" kupo.tar.gz
archive_dir "$MIDGARD_PHASE4_RUN_DIR/postgres" postgres.tar.gz
archive_dir "$MIDGARD_PHASE4_RUN_DIR/config" config.tar.gz
archive_dir "$MIDGARD_PHASE4_RUN_DIR/genesis" genesis.tar.gz
cp "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json" "$snapshot_dir/contract-deployment-info.json"
cp "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/phas-registration-transaction-body.json" "$snapshot_dir/phas-registration-transaction-body.json"
cp "$MIDGARD_PHASE4_RUN_DIR/work/plutus.json.sha256" "$snapshot_dir/plutus.json.sha256"
cp "$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env" "$snapshot_dir/acceptance.env"
[ ! -f "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/deployment-run-state.json" ] || cp "$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/deployment-run-state.json" "$snapshot_dir/deployment-run-state.json"
(
  cd "$snapshot_dir"
  snapshot_files="cardano-db.tar.gz kupo.tar.gz postgres.tar.gz config.tar.gz genesis.tar.gz contract-deployment-info.json phas-registration-transaction-body.json plutus.json.sha256 acceptance.env cardano-tip.json kupo-health.json phas-registration-proof.json snapshot-identity.json"
  [ ! -f deployment-run-state.json ] || snapshot_files="$snapshot_files deployment-run-state.json"
  sha256sum $snapshot_files >SHA256SUMS
  sha256sum SHA256SUMS | awk '{print $1}' >SNAPSHOT_SET_SHA256
  sha256sum snapshot-identity.json | awk '{print $1}' >SNAPSHOT_IDENTITY_SHA256
)

require_numeric_id() {
  id_label=$1
  id_value=$2
  case "$id_value" in
    '' | *[!0-9]*) die "snapshot $id_label must be numeric" ;;
  esac
}
snapshot_uid=$(id -u)
snapshot_gid=$(id -g)
require_numeric_id uid "$snapshot_uid"
require_numeric_id gid "$snapshot_gid"
docker run --rm \
  --volume "$snapshot_dir:/snapshot" \
  --entrypoint sh "$PHASE4_POSTGRES_IMAGE" \
  -ec '
    uid=$1
    gid=$2
    case "$uid" in ""|*[!0-9]*) exit 64;; esac
    case "$gid" in ""|*[!0-9]*) exit 64;; esac
    chown -R "$uid:$gid" /snapshot
  ' snapshot-owner "$snapshot_uid" "$snapshot_gid"
chmod -R go-rwx "$snapshot_dir"
printf '%s\n' "snapshotDir=$snapshot_dir" "snapshotSetSha256=$(cat "$snapshot_dir/SNAPSHOT_SET_SHA256")" "snapshotIdentitySha256=$(cat "$snapshot_dir/SNAPSHOT_IDENTITY_SHA256")" "servicesRunning=false"
