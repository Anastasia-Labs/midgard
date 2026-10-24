#!/bin/sh
# Copies the cardano-node image's config directory for NETWORK to /export
# (./cardano/config on the host): the files run-cardano-node.sh starts the
# node with, so midgard-node's L1 follower reads the same config and genesis.
set -eu

case "${NETWORK:-}" in
  Mainnet|mainnet)
    cardano_network="mainnet"
    ;;
  Preprod|preprod)
    cardano_network="preprod"
    ;;
  Preview|preview)
    cardano_network="preview"
    ;;
  *)
    echo "Unsupported NETWORK '${NETWORK:-}' for local Cardano node." >&2
    echo "Supported values are Mainnet, Preprod, and Preview." >&2
    exit 1
    ;;
esac

source_directory="/opt/cardano/config/${cardano_network}"
[ -f "$source_directory/config.json" ] || {
  echo "the cardano-node image has no $source_directory/config.json" >&2
  exit 1
}
# Stage, then move each file, so a reader never sees a half-written file.
staging="/export/.export.$$"
rm -rf "$staging"
mkdir -p "$staging"
cp "$source_directory"/* "$staging"/
for file in "$staging"/*; do
  mv -f "$file" /export/
done
rmdir "$staging"
echo "exported $source_directory to ./cardano/config"
