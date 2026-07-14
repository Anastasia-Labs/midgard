#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=common.sh
. "$script_dir/common.sh"

require_command jq
[ "$#" -eq 2 ] || die "usage: validate-custom-chain-config.sh <shelley-genesis.json> <config.json>"
shelley_genesis=$1
config=$2
[ -f "$shelley_genesis" ] || die "Shelley genesis is missing: $shelley_genesis"
[ -f "$config" ] || die "node config is missing: $config"

# These values are part of the reusable matched-devnet contract. The Shelley
# stability/forecast horizon is ceil(3k/f) slots. Pinning one-second slots,
# k=90000, and f=1 yields 270000 seconds (75 hours), while epochLength=10k/f
# preserves the conventional epoch relationship used by generated devnets.
expected_slot_length=1
expected_active_slots_coeff=1
expected_security_param=90000
expected_epoch_length=900000
minimum_snapshot_reuse_seconds=259200

slot_length=$(jq -er '.slotLength | select(type == "number" and . == floor)' "$shelley_genesis") \
  || die "Shelley slotLength must be an integer"
active_slots_coeff=$(jq -er '.activeSlotsCoeff | select(type == "number")' "$shelley_genesis") \
  || die "Shelley activeSlotsCoeff must be numeric"
security_param=$(jq -er '.securityParam | select(type == "number" and . == floor)' "$shelley_genesis") \
  || die "Shelley securityParam must be an integer"
epoch_length=$(jq -er '.epochLength | select(type == "number" and . == floor)' "$shelley_genesis") \
  || die "Shelley epochLength must be an integer"

[ "$slot_length" -eq "$expected_slot_length" ] \
  || die "Shelley slotLength must be $expected_slot_length; found $slot_length"
[ "$active_slots_coeff" = "$expected_active_slots_coeff" ] \
  || die "Shelley activeSlotsCoeff must be $expected_active_slots_coeff; found $active_slots_coeff"
[ "$security_param" -eq "$expected_security_param" ] \
  || die "Shelley securityParam must be $expected_security_param; found $security_param"
[ "$epoch_length" -eq "$expected_epoch_length" ] \
  || die "Shelley epochLength must be $expected_epoch_length; found $epoch_length"

forecast_horizon_slots=$((3 * security_param / active_slots_coeff))
forecast_horizon_seconds=$((forecast_horizon_slots * slot_length))
[ "$forecast_horizon_seconds" -ge "$minimum_snapshot_reuse_seconds" ] \
  || die "Shelley forecast horizon must cover at least $minimum_snapshot_reuse_seconds seconds; found $forecast_horizon_seconds"
[ "$epoch_length" -eq $((10 * security_param / active_slots_coeff)) ] \
  || die "Shelley epochLength must equal 10k/f"

protocol_major=$(jq -er '.protocolParams.protocolVersion.major' "$shelley_genesis")
case "$protocol_major" in
  ''|*[!0-9]*) die "Shelley protocol major is not a natural number: $protocol_major" ;;
esac
if [ "$protocol_major" -ne "$PHASE4_TARGET_PROTOCOL_MAJOR" ]; then
  die "custom Conway chain must match target protocol major $PHASE4_TARGET_PROTOCOL_MAJOR; found $protocol_major"
fi

jq -e \
  '.ShelleyGenesisFile == "/genesis/shelley-genesis.json"
   and .TestConwayHardForkAtEpoch == 0
   and .ExperimentalHardForksEnabled == true' \
  "$config" >/dev/null \
  || die "node config must force Conway at epoch zero with experimental hard forks enabled"
