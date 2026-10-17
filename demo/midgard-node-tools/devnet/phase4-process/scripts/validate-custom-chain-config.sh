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

# Consensus parameters come from the verified Preprod profile. Snapshot recovery
# must respect this chain; it may not extend consensus horizons for convenience.
profile="$script_dir/../../preprod/configuration.json"
expected_slot_length=$(jq -er '.consensus.slotLength' "$profile")
expected_active_slots_coeff=$(jq -er '.consensus.activeSlotsCoeff' "$profile")
expected_security_param=$(jq -er '.consensus.securityParam' "$profile")
expected_epoch_length=$(jq -er '.consensus.epochLength' "$profile")

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
