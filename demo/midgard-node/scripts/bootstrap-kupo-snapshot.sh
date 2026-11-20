#!/bin/sh
set -eu

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
      echo "Unsupported NETWORK '$1' for Kupo snapshot bootstrap." >&2
      echo "Supported values are Mainnet, Preprod, and Preview." >&2
      exit 1
      ;;
  esac
}

resolve_default_kupo_snapshot_md5() {
  case "$1" in
    mainnet)
      printf "972da57c519a281561c4d6b904a82301\n"
      ;;
    preprod)
      printf "d67726a64b5714fdd1dd68790762659e\n"
      ;;
    preview)
      printf "d67726a64b5714fdd1dd68790762659e\n"
      ;;
    *)
      echo "Unsupported Cardano network '$1' for Kupo snapshot mapping." >&2
      exit 1
      ;;
  esac
}

download_default_snapshot_archive() {
  resolved_cardano_network="$1"
  release_tag="$2"
  archive_path="$3"
  parts_dir="$4"

  base_url="https://github.com/CardanoSolutions/kupo/releases/download/${release_tag}"

  case "$resolved_cardano_network" in
    preprod)
      wget -q -O "$archive_path" "${base_url}/kupo.sqlite3-preprod.tar.gz"
      ;;
    preview)
      rm -rf "$parts_dir"
      mkdir -p "$parts_dir"
      for suffix in aa ab; do
        wget -q -O "$parts_dir/kupo.sqlite3-preview.tar.gz.part_${suffix}" \
          "${base_url}/kupo.sqlite3-preview.tar.gz.part_${suffix}"
      done
      cat "$parts_dir"/kupo.sqlite3-preview.tar.gz.part_* >"$archive_path"
      ;;
    mainnet)
      rm -rf "$parts_dir"
      mkdir -p "$parts_dir"
      for suffix in aa ab ac ad ae af ag ah ai aj ak al; do
        wget -q -O "$parts_dir/kupo.sqlite3-mainnet.tar.gz.part_${suffix}" \
          "${base_url}/kupo.sqlite3-mainnet.tar.gz.part_${suffix}"
      done
      cat "$parts_dir"/kupo.sqlite3-mainnet.tar.gz.part_* >"$archive_path"
      ;;
    *)
      echo "Unsupported Cardano network '$resolved_cardano_network' for default Kupo snapshot download." >&2
      exit 1
      ;;
  esac
}

require_env() {
  var_name="$1"
  eval "var_value=\${$var_name:-}"
  if [ -z "$var_value" ]; then
    echo "Missing required environment variable: $var_name" >&2
    exit 1
  fi
}

require_env NETWORK

restore_root="/data"
final_db_dir="$restore_root/kupo"
metadata_dir="$restore_root/.midgard-kupmios"
network_marker="$metadata_dir/kupo-network"
snapshot_source_marker="$metadata_dir/kupo-snapshot-source"
snapshot_md5_marker="$metadata_dir/kupo-snapshot-md5"
tmp_root="$restore_root/.kupo-restore"
stage_dir="$tmp_root/stage.$$"
resolved_cardano_network="$(normalize_cardano_network "$NETWORK")"
snapshot_release_tag="${KUPO_SNAPSHOT_RELEASE_TAG:-v2.8}"
configured_snapshot_url="${KUPO_SNAPSHOT_URL:-}"
expected_snapshot_md5="${KUPO_SNAPSHOT_MD5:-$(resolve_default_kupo_snapshot_md5 "$resolved_cardano_network")}"

mkdir -p "$restore_root" "$metadata_dir" "$tmp_root"

if dir_has_substantive_entries "$final_db_dir"; then
  if [ -f "$network_marker" ]; then
    existing_network="$(cat "$network_marker")"
    if [ "$existing_network" != "$resolved_cardano_network" ]; then
      echo "Existing Kupo DB at $final_db_dir targets '$existing_network', not '$resolved_cardano_network'." >&2
      echo "Remove ./cardano/kupo explicitly before switching networks." >&2
      exit 1
    fi
    echo "Existing Kupo DB detected at $final_db_dir for network '$resolved_cardano_network'; skipping snapshot restore." >&2
    exit 0
  fi

  echo "Existing Kupo DB detected at $final_db_dir; skipping snapshot restore." >&2
  echo "Network compatibility was not verified because $network_marker is missing." >&2
  exit 0
fi

rm -rf "$stage_dir"
mkdir -p "$stage_dir"
trap 'rm -rf "$stage_dir"' EXIT INT TERM

archive_path="$stage_dir/kupo.sqlite3.tar.gz"
parts_dir="$stage_dir/parts"

if [ -n "$configured_snapshot_url" ]; then
  snapshot_source="$configured_snapshot_url"
  echo "Downloading Kupo snapshot from '$snapshot_source'..." >&2
  wget -q -O "$archive_path" "$snapshot_source"
else
  snapshot_source="https://github.com/CardanoSolutions/kupo/releases/download/${snapshot_release_tag}/kupo.sqlite3-${resolved_cardano_network}.tar.gz"
  echo "Downloading official Kupo snapshot for '$resolved_cardano_network' from release '$snapshot_release_tag'..." >&2
  download_default_snapshot_archive \
    "$resolved_cardano_network" \
    "$snapshot_release_tag" \
    "$archive_path" \
    "$parts_dir"
fi

actual_snapshot_md5="$(md5sum "$archive_path" | awk '{print $1}')"
if [ "$actual_snapshot_md5" != "$expected_snapshot_md5" ]; then
  echo "Kupo snapshot MD5 mismatch for '$resolved_cardano_network'." >&2
  echo "Expected: $expected_snapshot_md5" >&2
  echo "Actual:   $actual_snapshot_md5" >&2
  exit 1
fi

echo "Extracting Kupo snapshot into staging..." >&2
tar -xzf "$archive_path" -C "$stage_dir"

if [ ! -f "$stage_dir/kupo.sqlite3" ]; then
  echo "Kupo snapshot archive did not contain kupo.sqlite3." >&2
  exit 1
fi

if [ -e "$final_db_dir" ] && [ ! -d "$final_db_dir" ]; then
  echo "Refusing to overwrite non-directory destination at $final_db_dir." >&2
  exit 1
fi

mkdir -p "$final_db_dir"
rm -f "$final_db_dir/.gitkeep"
if dir_has_substantive_entries "$final_db_dir"; then
  echo "Refusing to overwrite non-empty destination at $final_db_dir." >&2
  exit 1
fi

mv "$stage_dir/kupo.sqlite3" "$final_db_dir/kupo.sqlite3"

printf "%s\n" "$resolved_cardano_network" >"$network_marker"
printf "%s\n" "$snapshot_source" >"$snapshot_source_marker"
printf "%s\n" "$actual_snapshot_md5" >"$snapshot_md5_marker"

echo "Kupo snapshot restore complete. SQLite DB is ready at $final_db_dir/kupo.sqlite3." >&2
