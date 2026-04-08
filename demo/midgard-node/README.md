# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## What This Package Does

`midgard-node` is the demo off-chain runtime that ties the protocol together.
It is responsible for:

- serving the HTTP API used by wallets, tests, and local tooling,
- validating and enqueuing submitted Midgard-native transactions,
- maintaining PostgreSQL-backed views of mempool, latest-ledger, immutable
  history, and auxiliary indexes,
- maintaining LevelDB-backed Merkle Patricia Trie state for ledger and mempool
  roots,
- running background fibers that monitor mempool state, commit blocks, confirm
  block commitments, merge confirmed state, fetch deposit UTxOs, and sweep old
  retention data.

## Runtime Layout

- `src/commands`: HTTP handlers, CLI entrypoints, readiness logic, and response
  shaping.
- `src/fibers`: long-running background loops started by `listen`.
- `src/workers`: transaction-building and commitment helpers used by the
  background fibers.
- `src/database`: SQL access layers and persistence utilities.
- `src/services`: Effect services for config, contracts, Lucid, database, and
  global runtime state.
- `src/transactions`: one-shot transaction programs for initialization,
  operator lifecycle, and deposit flows.
- `scripts`: standalone stress and maintenance scripts used outside the main
  server loop.

## Operational State

- PostgreSQL stores durable relational views such as mempool entries, latest
  ledger entries, immutable transactions, address history, and rejection logs.
- `LEDGER_MPT_DB_PATH` and `MEMPOOL_MPT_DB_PATH` point at the LevelDB
  directories used to persist trie-backed state roots across restarts.
- `pnpm build` regenerates `src/generated/midgard-sdk-types.d.ts` by syncing
  the built SDK declarations before bundling the node.
- `ADMIN_API_KEY` gates the admin-only HTTP surface; keep it set in any shared
  or remotely reachable environment.

## How to Run

### With Docker

Using Docker, you can run Midgard node on `localhost:3000` (or another port)
quite easily.

0. If you don't have Docker yet or want to update, follow this [GUIDE](https://docs.docker.com/engine/install/). After installation, do not forget to execute also the [POST-INSTALLATION STEPS](https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user) to avoid using sudo with Docker.

1. Run Docker daemon if it's not running already:

   ```sh
   sudo dockerd
   ```

2. Pack the `midgard-sdk` tarball (see [here](../midgard-sdk/README.md)).

3. Prepare your `.env` file. You can use `.env.example` as your starting point:

   ```sh
   cd ../midgard-node
   cp .env.example .env
   ```

   1. When running with `L1_PROVIDER=Kupmios`, you do not need to fill out
      `L1_BLOCKFROST_API_URL` and `L1_BLOCKFROST_KEY`. The remaining fields need
      to be filled out.

4. Install all the dependencies:

   ```sh
   pnpm install --frozen-lockfile
   ```

   1. If the install fails with an incorrect SHA, that most likely means
      `midgard-sdk` was updated recently, but `pnpm-lock.yaml` still expects the
      old hash. Update the SHA value inside the `pnpm-lock.yaml` file with the
      new one.
   2. Rerun `pnpm install --frozen-lockfile`. Now it should install correctly.

5. Build the midgard-node:

   ```sh
   pnpm build
   ```

6. Run the application stack:

   ```sh
   docker compose up -d

   # or this for development:
   docker compose -f docker-compose.dev.yaml up -d
   ```

Midgard node should be running on port `PORT` (from your `.env`).

You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
docker logs -f midgard-node-midgard-node-1
```

If you made any changes to `midgard-node` and had an image running, restart it
with the 3 steps:

```sh
docker compose down -v
docker compose up -d --build
```

### Without Docker (No Monitoring)

For running the node itself, a running PostgreSQL server is also needed. The
fields you most likely want to modify in your `.env` file are:

```sh
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DB=midgard
POSTGRES_HOST=localhost
LEDGER_MPT_DB_PATH=midgard-ledger-mpt-db
MEMPOOL_MPT_DB_PATH=midgard-mempool-mpt-db
```

With a properly setup database, the following set of commands should start the
most up to date `midgard-node`:

```sh
# Optional
nix develop

# Bundle the SDK
cd ../midgard-sdk
pnpm install
pnpm repack

# Go back to `midgard-node` and force reinstallation of the SDK (faster than
# `pnpm install --force`)
cd ../midgard-node
rm -rf node_modules
pnpm install
pnpm listen
```

## Key Entry Points

- `pnpm listen`: build and start the HTTP server plus background fibers.
- `pnpm init`: initialize hub-oracle, state-queue, operator roots, and
  scheduler state.
- `node dist/index.js export-contract-deployment-info --out <path>`: write a
  JSON manifest describing the currently configured validator bundle and any
  published reference-script UTxOs visible in the reference-script wallet.
- `node dist/index.js submit-deposit`: build and submit an L1 deposit into the
  Midgard deposit contract for a target L2 address.
- `pnpm submit:l2-transfer`: build and submit a Midgard-native user transfer.
- `node dist/index.js project-deposits-once`: fetch L1 deposit events once and
  project newly visible deposits into the local Midgard ledger view.
- `pnpm audit:blocks-immutable`: inspect immutable block state and related
  persistence.
- `pnpm stress:valid`: run the high-throughput valid-transaction submitter.
- `pnpm stress:nominal`: run the lower-rate sustained activity generator.

## HTTP Surface

The main listener exposes a small operator-facing API. Common routes include:

- `/deposit/build` for building unsigned L1 deposit transactions from a
  caller-supplied wallet view,
- `/submit` for submitting Midgard-native transactions,
- `/utxos` for querying spendable Midgard mempool-ledger UTxOs,
- `/tx-status` for resolving the node's canonical status for a transaction,
- `/healthz` and `/readyz` for health and readiness checks.

The listener also exposes admin/operator routes for initialization, state
inspection, and operational control. See
[`src/commands/listen-router.ts`](./src/commands/listen-router.ts) for the
authoritative route graph.

## Testing

### With Docker

```sh
docker compose run --rm midgard-node-tests
```

### Without Docker

```sh
cd midgard-node
pnpm test
```

## Submit A Midgard L2 Transfer

Build and submit a key-signed Midgard-native transfer directly against the
running node.

```sh
cd midgard-node
pnpm build
node dist/index.js submit-l2-transfer \
  --l2-address <destination-l2-address> \
  --lovelace 5000000
```

Useful options:

```sh
# Override the default USER_WALLET seed source.
node dist/index.js submit-l2-transfer \
  --l2-address <destination-l2-address> \
  --lovelace 5000000 \
  --wallet-seed-phrase-env USER_WALLET

# Provide the seed phrase directly and send additional assets.
node dist/index.js submit-l2-transfer \
  --l2-address <destination-l2-address> \
  --lovelace 5000000 \
  --wallet-seed-phrase "<seed phrase>" \
  --endpoint http://127.0.0.1:3000 \
  0123456789abcdef0123456789abcdef0123456789abcdef01234567.4d4944:3
```

Notes:

- The command derives the sender address from `USER_WALLET` by default.
- If `USER_WALLET` is unset, it falls back to `USER_SEED_PHRASE` for
  compatibility with older local env files.
- The CLI now rejects wallets that collide with the node's operational
  operator, merge, or reference-script wallets. Use a distinct user wallet for
  deposit and L2 transfer flows.
- The command queries `/utxos`, builds a balanced Midgard-native transaction
  with explicit change, and submits it to `/submit`.

## Build An Unsigned L1 Deposit

Build a Cardano deposit transaction without giving the node any signing key.
The caller provides the funding address plus the exact wallet UTxOs that may be
spent, and the node returns unsigned transaction CBOR for the caller to sign
externally.

```sh
curl -X POST http://127.0.0.1:3000/deposit/build \
  -H 'content-type: application/json' \
  -d '{
    "l2Address": "addr_test1...",
    "lovelace": "12000000",
    "fundingAddress": "addr_test1...",
    "fundingUtxos": [
      {
        "txHash": "11...11",
        "outputIndex": 0,
        "address": "addr_test1...",
        "assets": {
          "lovelace": "30000000"
        }
      }
    ]
  }'
```

Optional fields:

- `l2Datum`: even-length hex inline datum for the projected Midgard UTxO.
- `additionalAssets`: array of `{ unit, amount }` entries to deposit alongside
  lovelace.
- `fundingUtxos[].datumHash`, `fundingUtxos[].datum`, `fundingUtxos[].scriptRef`:
  optional hex fields preserved in the external wallet view when present.

The response includes:

- `unsignedTxCbor`: unsigned Cardano transaction CBOR hex.

## Export Contract Deployment Info

Write a manifest of the currently configured validator bundle, keyed by explicit
script names such as `depositMint` and `depositSpend`.

```sh
cd midgard-node
pnpm build
node dist/index.js export-contract-deployment-info \
  --out contract-deployment-info.json
```

Each entry has the shape:

```json
{
  "depositMint": {
    "refScriptUTxO": null,
    "contract": {
      "type": "PlutusV3",
      "cborHex": "..."
    },
    "scriptHash": "..."
  }
}
```

`init` now always writes the manifest. By default it goes to the repository root
at `deploymentInfo/contract-deployment-info.json`. If you want to override that
path, pass:

```sh
node dist/index.js init \
  --contract-deployment-info-output contract-deployment-info.json
```

## Valid Throughput Stress Test

Run a high-throughput submitter that builds and submits **valid Midgard-native**
transactions (parallel dependent chains), then prints submit/accept/reject rates
from Prometheus metrics.

```sh
cd midgard-node
pnpm stress:valid
```

Useful overrides:

```sh
STRESS_CHAIN_LENGTH=500 \
STRESS_MAX_CHAINS=6 \
STRESS_TARGET_ACCEPTED_TPS=600 \
pnpm stress:valid
```

Notes:

- This script reads `TESTNET_GENESIS_WALLET_SEED_PHRASE_A/B/C` from `.env`.
- It uses `/utxos` to pick spendable UTxOs and submits to `/submit`.
- It reports counters from `validation_accept_count_total`,
  `validation_reject_count_total`, and `tx_count_total`.

## Nominal Sustained Activity Test

Run a lower-rate sustained generator that:

- queries current Midgard state via `/utxos`,
- builds fresh valid Midgard-native txs on-demand (no huge prebuild),
- submits at randomized intervals to emulate real network activity.

```sh
cd midgard-node
pnpm stress:nominal
```

Examples:

```sh
# Run for 5 minutes, stop after 100 successful submits.
pnpm stress:nominal -- --duration 5m --target-txs 100

# Run for 10 minutes with slower sporadic traffic.
pnpm stress:nominal -- --duration 10m --target-txs 120 --min-interval-ms 1000 --max-interval-ms 9000
```

Useful environment overrides:

```sh
ACTIVITY_DURATION=10m
ACTIVITY_TARGET_TXS=100
ACTIVITY_MIN_INTERVAL_MS=750
ACTIVITY_MAX_INTERVAL_MS=7000
ACTIVITY_WALLET_MODE=random
ACTIVITY_SUBMIT_ENDPOINT=http://127.0.0.1:3000
ACTIVITY_METRICS_ENDPOINT=http://127.0.0.1:9464/metrics
```

## Related Documentation

- [Root repository guide](../../README.md)
- [Midgard SDK guide](../midgard-sdk/README.md)
- [Preprod deposit and send-tx runbook](./PREPROD_DEPOSIT_AND_SEND_TX.md)
- [Technical specification guide](../../technical-spec/README.md)
