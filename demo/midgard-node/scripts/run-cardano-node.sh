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
    echo "Unsupported NETWORK '${NETWORK:-}' for local Cardano node." >&2
    echo "Supported values are Mainnet, Preprod, and Preview." >&2
    exit 1
    ;;
esac

unset NETWORK

exec entrypoint run \
  --config "/opt/cardano/config/${cardano_network}/config.json" \
  --topology "/opt/cardano/config/${cardano_network}/topology.json" \
  --database-path /data/db \
  --socket-path /ipc/node.socket \
  --host-addr 0.0.0.0 \
  --port 3001
