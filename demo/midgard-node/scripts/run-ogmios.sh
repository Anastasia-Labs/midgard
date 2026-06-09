#!/bin/sh
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
    echo "Unsupported NETWORK '${NETWORK:-}' for Ogmios." >&2
    echo "Supported values are Mainnet, Preprod, and Preview." >&2
    exit 1
    ;;
esac

exec ogmios \
  --host 0.0.0.0 \
  --node-config "/config/${cardano_network}/cardano-node/config.json" \
  --node-socket /ipc/node.socket