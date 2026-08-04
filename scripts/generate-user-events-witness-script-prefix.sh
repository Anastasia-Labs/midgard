#!/usr/bin/env bash
set -euo pipefail

mock_raw="7777777777777777777777777777777777777777777777777777777777777777"
mock_data="5820${mock_raw}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_dir="$(cd "$script_dir/../onchain/aiken" && pwd)"

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

cd "$project_dir"

generate_prefix() {
  local env_name="$1"
  local blueprint="$2"
  local applied_blueprint="$3"

  echo "🧪 Applying mock user-events witness nonce for ${env_name}..."
  aiken blueprint apply \
    --module user_events/witness \
    --validator main \
    --in "$blueprint" \
    --out "$applied_blueprint" \
    "$mock_data" >/dev/null

  local compiled_code
  compiled_code="$(
    jq -r '.validators[]
      | select(.title == "user_events/witness.main.publish")
      | .compiledCode' "$applied_blueprint"
  )"

  echo "🔎 Extracting user-events witness script prefix for ${env_name}..."
  if [ -z "$compiled_code" ] || [ "$compiled_code" = "null" ]; then
    echo "Could not find compiled code for user_events/witness.main.publish" >&2
    exit 1
  fi

  local marker_count
  marker_count="$(grep -o "$mock_raw" <<<"$compiled_code" | wc -l | tr -d ' ')"
  if [ "$marker_count" != "1" ]; then
    echo "Expected raw mock bytes to occur exactly once, found $marker_count" >&2
    exit 1
  fi

  # Keep only bytes before the raw mock value. This deliberately drops the mock
  # bytes and all bytes after them, while retaining the CBOR bytearray header
  # (`5820`) as the last two bytes of the prefix.
  prefix="${compiled_code%%$mock_raw*}"

  if [ -z "$prefix" ]; then
    echo "Generated witness script prefix is empty" >&2
    exit 1
  fi
}

update_prefix() {
  local file="$1"
  local prefix="$2"

  echo "✍️  Updating $file..."
  awk -v prefix="$prefix" '
    BEGIN { replaced = 0 }
    /^pub const user_events_witness_script_prefix: ByteArray =/ {
      print
      getline
      print "  #\"" prefix "\""
      replaced = 1
      next
    }
    { print }
    END {
      if (!replaced) {
        exit 1
      }
    }
  ' "$file" >"$file.tmp"
  mv "$file.tmp" "$file"
}

default_blueprint="$tmp_dir/default.plutus.json"
testnet_blueprint="$tmp_dir/testnet.plutus.json"
default_applied_blueprint="$tmp_dir/default-applied.plutus.json"
testnet_applied_blueprint="$tmp_dir/testnet-applied.plutus.json"

echo "🏗️  Building default Aiken blueprint..."
aiken build --out "$default_blueprint" >/dev/null
generate_prefix "default" "$default_blueprint" "$default_applied_blueprint"
default_prefix="$prefix"
update_prefix "env/default.ak" "$default_prefix"

echo "🏗️  Building testnet Aiken blueprint..."
aiken build --env testnet --out "$testnet_blueprint" >/dev/null
generate_prefix "testnet" "$testnet_blueprint" "$testnet_applied_blueprint"
testnet_prefix="$prefix"
update_prefix "env/testnet.ak" "$testnet_prefix"

echo "🎨 Formatting Aiken sources..."
aiken fmt >/dev/null

echo "✅ User-events witness script prefix generated."
