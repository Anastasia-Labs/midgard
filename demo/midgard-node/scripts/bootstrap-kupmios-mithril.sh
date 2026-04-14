#!/bin/sh
set -eu

require_env() {
  var_name="$1"
  eval "var_value=\${$var_name:-}"
  if [ -z "$var_value" ]; then
    echo "Missing required environment variable: $var_name" >&2
    exit 1
  fi
}

dir_has_substantive_entries() {
  dir_path="$1"
  [ -n "$(find "$dir_path" -mindepth 1 -maxdepth 1 ! -name '.gitkeep' -print -quit 2>/dev/null || true)" ]
}

normalize_cardano_network() {
  case "$1" in
    Mainnet|mainnet)
      printf "mainnet\n"
      ;;
    Preprod|preprod)
      printf "preprod\n"
      ;;
    Preview|preview)
      printf "preview\n"
      ;;
    *)
      echo "Unsupported NETWORK '$1' for Mithril-backed Kupmios bootstrap." >&2
      echo "Supported values are Mainnet, Preprod, and Preview." >&2
      exit 1
      ;;
  esac
}

resolve_mithril_network() {
  case "$1" in
    mainnet)
      printf "release-mainnet\n"
      ;;
    preprod)
      printf "release-preprod\n"
      ;;
    preview)
      printf "pre-release-preview\n"
      ;;
    *)
      echo "Unsupported Cardano network '$1' for Mithril network mapping." >&2
      exit 1
      ;;
  esac
}

validate_mithril_endpoint() {
  endpoint="$1"
  expected_network="$2"
  if printf "%s" "$endpoint" | grep -q "api.mithril.network"; then
    expected_host="aggregator.${expected_network}.api.mithril.network/aggregator"
    if ! printf "%s" "$endpoint" | grep -q "$expected_host"; then
      echo "MITHRIL_AGGREGATOR_ENDPOINT '$endpoint' does not match NETWORK=$NETWORK." >&2
      echo "Expected official endpoint containing '$expected_host'." >&2
      exit 1
    fi
  fi
}

validate_mithril_key_url() {
  var_name="$1"
  url="$2"
  expected_network="$3"
  expected_suffix="$4"
  if printf "%s" "$url" | grep -q "raw.githubusercontent.com/input-output-hk/mithril/"; then
    expected_path="/configuration/${expected_network}/${expected_suffix}"
    if ! printf "%s" "$url" | grep -q "$expected_path"; then
      echo "$var_name '$url' does not match NETWORK=$NETWORK." >&2
      echo "Expected official URL containing '$expected_path'." >&2
      exit 1
    fi
  fi
}

validate_cardano_node_image_tag() {
  image_tag="$1"
  case "$image_tag" in
    ''|*[!0-9.]*|*.*.*.*|.*|*.)
      echo "CARDANO_NODE_IMAGE_TAG '$image_tag' must be an explicit Cardano node version like '10.6.2'." >&2
      exit 1
      ;;
  esac
}

version_ge() {
  left="$1"
  right="$2"

  old_ifs="${IFS}"
  IFS=.
  set -- $left
  left_major="${1:-0}"
  left_minor="${2:-0}"
  left_patch="${3:-0}"
  set -- $right
  right_major="${1:-0}"
  right_minor="${2:-0}"
  right_patch="${3:-0}"
  IFS="${old_ifs}"

  if [ "$left_major" -gt "$right_major" ]; then
    return 0
  fi
  if [ "$left_major" -lt "$right_major" ]; then
    return 1
  fi
  if [ "$left_minor" -gt "$right_minor" ]; then
    return 0
  fi
  if [ "$left_minor" -lt "$right_minor" ]; then
    return 1
  fi
  if [ "$left_patch" -ge "$right_patch" ]; then
    return 0
  fi

  return 1
}

extract_json_string() {
  json="$1"
  key="$2"
  printf "%s\n" "$json" | sed -n "s/.*\"$key\":\"\\([^\"]*\\)\".*/\\1/p"
}

ensure_cardano_node_image_compatible() {
  configured_version="$1"
  required_version="$2"

  if version_ge "$configured_version" "$required_version"; then
    return 0
  fi

  echo "CARDANO_NODE_IMAGE_TAG '$configured_version' is older than the Mithril snapshot requirement '$required_version'." >&2
  echo "Choose a Cardano node image version that is greater than or equal to the certified snapshot's cardano_node_version." >&2
  exit 1
}

require_env NETWORK

restore_root="/data"
final_db="$restore_root/db"
metadata_dir="$restore_root/.midgard-kupmios"
network_marker="$metadata_dir/cardano-network"
snapshot_marker="$metadata_dir/mithril-snapshot-requested"
aggregator_marker="$metadata_dir/mithril-aggregator-endpoint"
snapshot_cardano_node_version_marker="$metadata_dir/mithril-cardano-node-version"
requested_snapshot="${MITHRIL_SNAPSHOT_DIGEST:-latest}"
tmp_root="$restore_root/.mithril-restore"
stage_dir="$tmp_root/stage.$$"
resolved_cardano_network="$(normalize_cardano_network "$NETWORK")"
mithril_network="$(resolve_mithril_network "$resolved_cardano_network")"
default_mithril_aggregator="https://aggregator.${mithril_network}.api.mithril.network/aggregator"
default_mithril_genesis_key_url="https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/${mithril_network}/genesis.vkey"
default_mithril_ancillary_key_url="https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/${mithril_network}/ancillary.vkey"
resolved_mithril_aggregator="${MITHRIL_AGGREGATOR_ENDPOINT:-$default_mithril_aggregator}"
resolved_mithril_genesis_key_url="${MITHRIL_GENESIS_VERIFICATION_KEY_URL:-$default_mithril_genesis_key_url}"
resolved_mithril_ancillary_key_url="${MITHRIL_ANCILLARY_VERIFICATION_KEY_URL:-$default_mithril_ancillary_key_url}"
cardano_node_image_tag="${CARDANO_NODE_IMAGE_TAG:-10.6.2}"

validate_cardano_node_image_tag "$cardano_node_image_tag"

validate_mithril_endpoint "$resolved_mithril_aggregator" "$mithril_network"
validate_mithril_key_url \
  "MITHRIL_GENESIS_VERIFICATION_KEY_URL" \
  "$resolved_mithril_genesis_key_url" \
  "$mithril_network" \
  "genesis.vkey"
validate_mithril_key_url \
  "MITHRIL_ANCILLARY_VERIFICATION_KEY_URL" \
  "$resolved_mithril_ancillary_key_url" \
  "$mithril_network" \
  "ancillary.vkey"

mkdir -p "$restore_root" "$metadata_dir" "$tmp_root"

if dir_has_substantive_entries "$final_db"; then
  if [ -f "$network_marker" ]; then
    existing_network="$(cat "$network_marker")"
    if [ "$existing_network" != "$resolved_cardano_network" ]; then
      echo "Existing Cardano DB at $final_db targets '$existing_network', not '$resolved_cardano_network'." >&2
      echo "Remove ./cardano/db and ./cardano/kupo explicitly before switching networks." >&2
      exit 1
    fi
  else
    echo "Existing Cardano DB detected at $final_db; skipping Mithril restore." >&2
    echo "Network compatibility was not verified because $network_marker is missing." >&2
    exit 0
  fi

  if [ -f "$snapshot_cardano_node_version_marker" ]; then
    stored_snapshot_cardano_node_version="$(cat "$snapshot_cardano_node_version_marker")"
    ensure_cardano_node_image_compatible \
      "$cardano_node_image_tag" \
      "$stored_snapshot_cardano_node_version"
  else
    echo "Existing Cardano DB detected at $final_db for network '$resolved_cardano_network'; skipping Mithril restore." >&2
    echo "Cardano node compatibility was not verified because $snapshot_cardano_node_version_marker is missing." >&2
    exit 0
  fi

  echo "Existing Cardano DB detected at $final_db for network '$resolved_cardano_network'; skipping Mithril restore." >&2
  exit 0
fi

rm -rf "$stage_dir"
mkdir -p "$stage_dir"
trap 'rm -rf "$stage_dir"' EXIT INT TERM

echo "Fetching Mithril verification keys for network '$resolved_cardano_network'..." >&2
export AGGREGATOR_ENDPOINT="$resolved_mithril_aggregator"
export GENESIS_VERIFICATION_KEY="$(wget -q -O - "$resolved_mithril_genesis_key_url")"
export ANCILLARY_VERIFICATION_KEY="$(wget -q -O - "$resolved_mithril_ancillary_key_url")"

echo "Inspecting Mithril snapshot metadata for network '$resolved_cardano_network'..." >&2
snapshot_metadata_json="$(
  /app/bin/mithril-client --json cardano-db snapshot show "$requested_snapshot"
)"
snapshot_cardano_node_version="$(extract_json_string "$snapshot_metadata_json" "cardano_node_version")"
if [ -z "$snapshot_cardano_node_version" ]; then
  echo "Unable to determine cardano_node_version from Mithril snapshot metadata." >&2
  exit 1
fi
ensure_cardano_node_image_compatible \
  "$cardano_node_image_tag" \
  "$snapshot_cardano_node_version"

echo "Restoring Mithril Cardano DB snapshot '$requested_snapshot' into staging..." >&2
cd "$stage_dir"
/app/bin/mithril-client -vvv cardano-db download --include-ancillary "$requested_snapshot"

if ! dir_has_substantive_entries "$stage_dir/db"; then
  echo "Mithril restore completed without producing $stage_dir/db." >&2
  exit 1
fi

if [ -e "$final_db" ] && [ ! -d "$final_db" ]; then
  echo "Refusing to overwrite non-directory destination at $final_db." >&2
  exit 1
fi

mkdir -p "$final_db"
rm -f "$final_db/.gitkeep"
if dir_has_substantive_entries "$final_db"; then
  echo "Refusing to overwrite non-empty destination at $final_db." >&2
  exit 1
fi

(
  cd "$stage_dir/db"
  for entry in * .[!.]* ..?*; do
    [ -e "$entry" ] || continue
    mv "$entry" "$final_db"/
  done
)

printf "%s\n" "$resolved_cardano_network" > "$network_marker"
printf "%s\n" "$requested_snapshot" > "$snapshot_marker"
printf "%s\n" "$resolved_mithril_aggregator" > "$aggregator_marker"
printf "%s\n" "$snapshot_cardano_node_version" > "$snapshot_cardano_node_version_marker"

echo "Mithril restore complete. Cardano DB is ready at $final_db." >&2
