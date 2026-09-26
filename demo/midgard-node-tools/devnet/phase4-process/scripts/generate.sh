#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=common.sh
. "$script_dir/common.sh"

require_command docker
require_command jq
require_command node
require_command sha256sum

: "${MIDGARD_PHASE4_RUN_DIR:?MIDGARD_PHASE4_RUN_DIR is required}"
case "$MIDGARD_PHASE4_RUN_DIR" in
  /*) ;;
  *) die "MIDGARD_PHASE4_RUN_DIR must be absolute" ;;
esac
[ ! -e "$MIDGARD_PHASE4_RUN_DIR" ] || die "refusing to overwrite existing run directory: $MIDGARD_PHASE4_RUN_DIR"

run_id=${MIDGARD_PHASE4_RUN_ID:-$(basename "$MIDGARD_PHASE4_RUN_DIR")}
slug=$(printf '%s' "$run_id" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9_]/_/g; s/^_*//; s/_*$//; s/__*/_/g')
[ -n "$slug" ] || die "run id has no safe project-name characters"
network_magic=${MIDGARD_PHASE4_NETWORK_MAGIC:-424242}
case "$network_magic" in
  ''|*[!0-9]*) die "network magic must be a natural number" ;;
  1|2|764824073) die "refusing known public-network magic $network_magic" ;;
esac

# A linked git worktree gets its own compose project and host ports, so two
# checkouts' devnets can run at once; the main checkout keeps the historical
# names and ports exactly. Explicit port variables still win. The database
# lives in this run's own Postgres container, so its name needs no namespace.
worktree_identity="$tools_root/../../scripts/lib/worktree-identity.mjs"
[ -f "$worktree_identity" ] || die "missing $worktree_identity"
worktree_main=$(node "$worktree_identity" isMainCheckout --root "$tools_root") \
  || die "could not derive the checkout identity"
if [ "$worktree_main" = true ]; then
  compose_project="midgard_phase4_process_${slug}"
else
  worktree_hash=$(node "$worktree_identity" hash --root "$tools_root") \
    || die "could not derive the checkout identity"
  compose_project="midgard_phase4_process_${worktree_hash}_${slug}"
fi
port_offset=$(node "$worktree_identity" portOffset --root "$tools_root") \
  || die "could not derive the checkout identity"
ogmios_port=${MIDGARD_PHASE4_OGMIOS_PORT:-$((2337 + port_offset))}
kupo_port=${MIDGARD_PHASE4_KUPO_PORT:-$((2442 + port_offset))}
postgres_port=${MIDGARD_PHASE4_POSTGRES_PORT:-$((5544 + port_offset))}
printf 'phase4-process: compose project %s, host ports ogmios=%s kupo=%s postgres=%s\n' \
  "$compose_project" "$ogmios_port" "$kupo_port" "$postgres_port" >&2
postgres_database="midgard_phase4_process_${slug}"
postgres_user=${MIDGARD_PHASE4_POSTGRES_USER:-midgard_phase4_process}
postgres_password=${MIDGARD_PHASE4_POSTGRES_PASSWORD:-}
[ -n "$postgres_password" ] || postgres_password=$(od -An -N24 -tx1 /dev/urandom | tr -d ' \n')
genesis_start=${MIDGARD_PHASE4_GENESIS_START_TIME:-$(date -u -d '+2 minutes' '+%Y-%m-%dT%H:%M:%SZ')}
cardano_image="$PHASE4_CARDANO_NODE_IMAGE"

umask 077
for relative_dir in config cardano/db cardano/ipc kupo postgres deploymentInfo snapshots secrets work; do
  mkdir -p "$MIDGARD_PHASE4_RUN_DIR/$relative_dir"
done
# The run directory is private (0700); only its Postgres bind must be writable by the container UID.
chmod 0777 "$MIDGARD_PHASE4_RUN_DIR/postgres"

docker run --rm --user "$(id -u):$(id -g)" \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run" \
  --entrypoint cardano-cli "$cardano_image" \
  latest genesis create-testnet-data \
  --genesis-keys 1 --pools 1 --stake-delegators 1 --utxo-keys 1 --committee-keys 1 --drep-keys 1 \
  --total-supply 100000000000000 --delegated-supply 50000000000000 \
  --testnet-magic "$network_magic" --start-time "$genesis_start" --out-dir /run/genesis

# Use the verified Preprod consensus and active ledger configuration. Local
# identities and funded accounts belong to this isolated chain.
node "$script_dir/../../preprod/apply-configuration.mjs" "$MIDGARD_PHASE4_RUN_DIR/genesis"
cp "$script_dir/../../preprod/configuration.json" "$MIDGARD_PHASE4_RUN_DIR/config/preprod-configuration.json"
jq -e '(.extraConfig.stakePools.data | length) == 1 and (.extraConfig.stakeCredentials.data | length) == 1' \
  "$MIDGARD_PHASE4_RUN_DIR/genesis/shelley-genesis.json" >/dev/null \
  || die "generated genesis must contain exactly one registered pool and stake delegation"

genesis_hash() {
  file=$1
  docker run --rm --user "$(id -u):$(id -g)" \
    --volume "$MIDGARD_PHASE4_RUN_DIR:/run:ro" \
    --entrypoint cardano-cli "$cardano_image" \
    latest genesis hash --genesis "/run/genesis/$file"
}

shelley_hash=$(genesis_hash shelley-genesis.json)
alonzo_hash=$(genesis_hash alonzo-genesis.json)
conway_hash=$(genesis_hash conway-genesis.json)
dijkstra_hash=$(genesis_hash dijkstra-genesis.json)
byron_hash=$(docker run --rm --user "$(id -u):$(id -g)" \
  --volume "$MIDGARD_PHASE4_RUN_DIR:/run:ro" \
  --entrypoint cardano-cli "$cardano_image" \
  byron genesis print-genesis-hash --genesis-json /run/genesis/byron-genesis.json)

jq -n \
  --arg byron "$byron_hash" --arg shelley "$shelley_hash" \
  --arg alonzo "$alonzo_hash" --arg conway "$conway_hash" --arg dijkstra "$dijkstra_hash" \
  '{Protocol:"Cardano",ConsensusMode:"PraosMode",RequiresNetworkMagic:"RequiresMagic",ByronGenesisFile:"/genesis/byron-genesis.json",ByronGenesisHash:$byron,ShelleyGenesisFile:"/genesis/shelley-genesis.json",ShelleyGenesisHash:$shelley,AlonzoGenesisFile:"/genesis/alonzo-genesis.json",AlonzoGenesisHash:$alonzo,ConwayGenesisFile:"/genesis/conway-genesis.json",ConwayGenesisHash:$conway,DijkstraGenesisFile:"/genesis/dijkstra-genesis.json",DijkstraGenesisHash:$dijkstra,"LastKnownBlockVersion-Major":2,"LastKnownBlockVersion-Minor":0,"LastKnownBlockVersion-Alt":0,TestShelleyHardForkAtEpoch:0,TestAllegraHardForkAtEpoch:0,TestMaryHardForkAtEpoch:0,TestAlonzoHardForkAtEpoch:0,TestBabbageHardForkAtEpoch:0,TestConwayHardForkAtEpoch:0,ExperimentalHardForksEnabled:true,TxSubmissionInitDelay:0,TraceOptions:{"":{severity:"Info",detail:"DNormal",backends:["Stdout MachineFormat"]}}}' \
  >"$MIDGARD_PHASE4_RUN_DIR/config/config.json"
"$script_dir/validate-custom-chain-config.sh" \
  "$MIDGARD_PHASE4_RUN_DIR/genesis/shelley-genesis.json" \
  "$MIDGARD_PHASE4_RUN_DIR/config/config.json"
printf '%s\n' '{"localRoots":[],"publicRoots":[],"useLedgerAfterSlot":-1}' >"$MIDGARD_PHASE4_RUN_DIR/config/topology.json"

cat >"$MIDGARD_PHASE4_RUN_DIR/run.env" <<EOF
MIDGARD_PHASE4_RUN_ID=$slug
MIDGARD_PHASE4_RUN_DIR=$MIDGARD_PHASE4_RUN_DIR
MIDGARD_PHASE4_COMPOSE_PROJECT=$compose_project
MIDGARD_PHASE4_NETWORK_MAGIC=$network_magic
MIDGARD_PHASE4_PROTOCOL_MAJOR=$PHASE4_TARGET_PROTOCOL_MAJOR
MIDGARD_PHASE4_OGMIOS_PORT=$ogmios_port
MIDGARD_PHASE4_KUPO_PORT=$kupo_port
MIDGARD_PHASE4_POSTGRES_PORT=$postgres_port
MIDGARD_PHASE4_POSTGRES_USER=$postgres_user
MIDGARD_PHASE4_POSTGRES_PASSWORD=$postgres_password
MIDGARD_PHASE4_POSTGRES_DATABASE=$postgres_database
MIDGARD_PHASE4_CARDANO_NODE_IMAGE=$PHASE4_CARDANO_NODE_IMAGE
MIDGARD_PHASE4_OGMIOS_IMAGE=$PHASE4_OGMIOS_IMAGE
MIDGARD_PHASE4_KUPO_IMAGE=$PHASE4_KUPO_IMAGE
MIDGARD_PHASE4_POSTGRES_IMAGE=$PHASE4_POSTGRES_IMAGE
EOF

if [ -n "${MIDGARD_PHASE4_WALLET_ENV_FILE:-}" ]; then
  [ -f "$MIDGARD_PHASE4_WALLET_ENV_FILE" ] || die "wallet env file not found"
  cp "$MIDGARD_PHASE4_WALLET_ENV_FILE" "$MIDGARD_PHASE4_RUN_DIR/secrets/wallets.env"
fi
find "$MIDGARD_PHASE4_RUN_DIR/genesis" -type f -name '*.skey' -exec chmod 600 {} +
chmod 600 "$MIDGARD_PHASE4_RUN_DIR/run.env"
find "$MIDGARD_PHASE4_RUN_DIR/secrets" -type f -exec chmod 600 {} +

MIDGARD_PHASE4_RUN_DIR="$MIDGARD_PHASE4_RUN_DIR" \
MIDGARD_PHASE4_COMPOSE_PROJECT="$compose_project" \
MIDGARD_PHASE4_POSTGRES_DATABASE="$postgres_database" \
MIDGARD_PHASE4_POSTGRES_PASSWORD="$postgres_password" \
docker compose --project-name "$compose_project" --file "$compose_file" config --quiet

printf '%s\n' "generated=$MIDGARD_PHASE4_RUN_DIR" "composeProject=$compose_project" "networkMagic=$network_magic" "servicesStarted=false"
