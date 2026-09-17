#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
. "$script_dir/common.sh"
require_command curl
require_command jq
require_command sha256sum
require_run_dir

manifest="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
transaction_body="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/phas-registration-transaction-body.json"
[ -s "$manifest" ] || die "deployment manifest is missing"
[ -s "$transaction_body" ] || die "PHAS registration transaction body is missing"
jq -e \
  'type == "object" and
   keys == ["cborHex","description","type"] and
   .type == "Unwitnessed Tx ConwayEra" and
   (.description | type == "string" and length > 0) and
   (.cborHex | type == "string" and test("^[a-f0-9]+$") and (length % 2 == 0))' \
  "$transaction_body" >/dev/null \
  || die "PHAS registration transaction body is not exact canonical V1"

manifest_id=$(jq -er '.manifestId | select(test("^[a-f0-9]{64}$"))' "$manifest")
registration_tx_hash=$(jq -er '.steps.phasRegistration.txHash | select(test("^[a-f0-9]{64}$"))' "$manifest")
reward_address=$(jq -er '.steps.phasRegistration.rewardAddress | select(test("^stake(_test)?1[0-9a-z]+$"))' "$manifest")
script_hash=$(jq -er '.steps.phasRegistration.scriptHash | select(test("^[a-f0-9]{56}$"))' "$manifest")
transaction_body_artifact_sha=$(jq -er '.steps.phasRegistration.transactionBody.artifactSha256 | select(test("^[a-f0-9]{64}$"))' "$manifest")
transaction_body_cbor_sha=$(jq -er '.steps.phasRegistration.transactionBody.cborSha256 | select(test("^[a-f0-9]{64}$"))' "$manifest")
transaction_body_cbor_size=$(jq -er '.steps.phasRegistration.transactionBody.cborSizeBytes | select(type == "number" and . == floor and . > 0)' "$manifest")
[ "$(sha256_file "$transaction_body")" = "$transaction_body_artifact_sha" ] \
  || die "PHAS registration transaction-body artifact checksum mismatch"
jq -e \
  --arg txHash "$registration_tx_hash" \
  --arg rewardAddress "$reward_address" \
  --arg scriptHash "$script_hash" \
  '.steps.phasRegistration.status == "complete" and
   .steps.phasRegistration.txHash == $txHash and
   .steps.phasRegistration.rewardAddress == $rewardAddress and
   .steps.phasRegistration.scriptHash == $scriptHash and
   .steps.phasRegistration.transactionBody.txHash == $txHash and
   .steps.phasRegistration.transactionBody.certificate == {kind:"stake_registration",index:0,count:1,credentialType:"script",scriptHash:$scriptHash} and
   .contracts.phasMembershipWithdraw.scriptHash == $scriptHash' "$manifest" >/dev/null \
  || die "deployment manifest PHAS registration identity is incomplete or mismatched"

cardano_image_id=$(image_id "$PHASE4_CARDANO_NODE_IMAGE")
address_info=$(docker run --rm \
  --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" \
  address info --address "$reward_address")
reward_address_base16=$(printf '%s' "$address_info" | jq -er \
  --arg rewardAddress "$reward_address" \
  --arg scriptHash "$script_hash" \
  'select(.address == $rewardAddress and .encoding == "bech32" and .era == "shelley" and .type == "stake") |
   .base16 | select(. == ("f0" + $scriptHash))') \
  || die "PHAS reward address is not the canonical testnet script credential"

cardano_txid=$(docker run --rm \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run:ro" \
  --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" \
  latest transaction txid \
  --tx-body-file /run/deploymentInfo/phas-registration-transaction-body.json \
  | jq -er '.txhash | select(test("^[a-f0-9]{64}$"))')
[ "$cardano_txid" = "$registration_tx_hash" ] \
  || die "pinned cardano-cli transaction-body hash does not match the submitted PHAS transaction"
transaction_view=$(docker run --rm \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run:ro" \
  --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" \
  debug transaction view --output-json \
  --tx-body-file /run/deploymentInfo/phas-registration-transaction-body.json)
printf '%s' "$transaction_view" | jq -e \
  --arg scriptHash "$script_hash" \
  '.era == "Conway" and
   (.certificates | length) == 1 and
   .certificates[0] == {"Stake address registration":{"stake credential":{scriptHash:$scriptHash}}}' >/dev/null \
  || die "pinned cardano-cli did not prove the exact PHAS script registration certificate"
stake_info=$(docker run --rm \
  --user "$(id -u):$(id -g)" \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run" \
  --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" \
  latest query stake-address-info \
  --address "$reward_address" \
  --socket-path /run/cardano/ipc/node.socket \
  --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC")
registration_deposit=$(printf '%s' "$stake_info" | jq -er \
  --arg rewardAddress "$reward_address" \
  'select(type == "array" and length == 1) | .[0] |
   select(.address == $rewardAddress) |
   (.delegationDeposit // .stakeRegistrationDeposit) |
   select(type == "number" and . == floor and . > 0)') \
  || die "Cardano local-state query did not prove the exact PHAS reward account registered"

tip=$(docker run --rm \
  --user "$(id -u):$(id -g)" \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run" \
  --entrypoint cardano-cli "$PHASE4_CARDANO_NODE_IMAGE" \
  latest query tip \
  --socket-path /run/cardano/ipc/node.socket \
  --testnet-magic "$MIDGARD_PHASE4_NETWORK_MAGIC")
tip_slot=$(printf '%s' "$tip" | jq -er '.slot | select(type == "number" and . == floor and . >= 0)')
tip_hash=$(printf '%s' "$tip" | jq -er '.hash | select(test("^[a-f0-9]{64}$"))')

kupo_matches=$(curl --fail --silent --show-error \
  "http://127.0.0.1:$MIDGARD_PHASE4_KUPO_PORT/matches/*%40$registration_tx_hash")
confirmation=$(printf '%s' "$kupo_matches" | jq -cer \
  --arg txHash "$registration_tx_hash" \
  '[.[] | select(.transaction_id == $txHash and
    (.created_at.slot_no | type == "number" and . == floor and . >= 0) and
    (.created_at.header_hash | type == "string" and test("^[a-f0-9]{64}$")))] |
   if length == 0 then error("registration transaction has no canonical Kupo output")
   else .[0].created_at end') \
  || die "Kupo did not prove the PHAS registration transaction canonical"
confirmation_slot=$(printf '%s' "$confirmation" | jq -er '.slot_no')
[ "$confirmation_slot" -le "$tip_slot" ] \
  || die "PHAS registration confirmation is beyond the observed Cardano tip"

jq -cnS \
  --arg schemaVersion midgard-phase4-phas-registration-proof-v1 \
  --arg source cardano-cli-local-state-query \
  --arg cardanoImage "$PHASE4_CARDANO_NODE_IMAGE" \
  --arg cardanoImageId "$cardano_image_id" \
  --argjson networkMagic "$MIDGARD_PHASE4_NETWORK_MAGIC" \
  --arg manifestId "$manifest_id" \
  --arg registrationTxHash "$registration_tx_hash" \
  --arg rewardAddress "$reward_address" \
  --arg scriptHash "$script_hash" \
  --arg rewardAddressBase16 "$reward_address_base16" \
  --arg transactionBodyArtifactSha256 "$transaction_body_artifact_sha" \
  --arg transactionBodyCborSha256 "$transaction_body_cbor_sha" \
  --argjson transactionBodyCborSizeBytes "$transaction_body_cbor_size" \
  --arg cardanoCliTxHash "$cardano_txid" \
  --argjson registrationDepositLovelace "$registration_deposit" \
  --argjson confirmation "$confirmation" \
  --argjson tip "$tip" \
  '{schemaVersion:$schemaVersion,source:$source,readOnly:true,registered:true,
    cardanoImage:{ref:$cardanoImage,id:$cardanoImageId},networkMagic:$networkMagic,
    manifestId:$manifestId,registrationTxHash:$registrationTxHash,
    rewardAddress:$rewardAddress,rewardAddressBase16:$rewardAddressBase16,scriptHash:$scriptHash,
    transactionBody:{schemaVersion:"midgard-phas-registration-transaction-body-v1",
      artifactSha256:$transactionBodyArtifactSha256,cborSha256:$transactionBodyCborSha256,
      cborSizeBytes:$transactionBodyCborSizeBytes,cardanoCliTxHash:$cardanoCliTxHash,
      certificate:{kind:"stake_registration",index:0,count:1,credentialType:"script",scriptHash:$scriptHash}},
    registrationDepositLovelace:$registrationDepositLovelace,
    confirmation:{slot:$confirmation.slot_no,blockHeaderHash:$confirmation.header_hash},
    observedAtTip:{slot:$tip.slot,hash:$tip.hash}}'
