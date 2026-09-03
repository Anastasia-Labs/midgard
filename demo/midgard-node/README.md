# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## What This Package Does

`midgard-node` is the demo off-chain runtime that ties the protocol together.
It is responsible for:

- serving the HTTP API used by wallets, tests, and local tooling,
- validating and enqueuing submitted Midgard-native transactions,
- maintaining PostgreSQL-backed views of mempool, latest-ledger, immutable
  history, and auxiliary indexes,
- maintaining LevelDB-backed Merkle Patricia Forestry state for ledger and
  transaction roots,
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
- `LEDGER_MPF_DB_PATH` and `TRANSACTIONS_MPF_DB_PATH` point at the LevelDB
  directories used to persist MPF-backed state roots across restarts.
- `pnpm build` regenerates `src/generated/midgard-sdk-types.d.ts` by syncing
  the built SDK declarations before bundling the node.
- `ADMIN_API_KEY` gates the admin-only HTTP surface; keep it set in any shared
  or remotely reachable environment.
- Accepted `mempool` and `mempool_ledger` rows, spent-input deletion, consumed
  deposit tracking, and the durable admission terminal update commit atomically.
  `mempool_tx_deltas` and produced-side `address_history` rows are auxiliary
  projections flushed by a bounded write-behind writer after that commit.
- A crash can lose the unflushed auxiliary window. Missing tx deltas are safe:
  the commit worker re-decodes transaction CBOR. Missing address-history rows
  remain absent until an operator rebuilds that auxiliary index from retained
  mempool/immutable transaction CBOR. The writer never drops live-process rows:
  queue overflow falls back to an inline write, and graceful shutdown drains the
  queue.

## Transaction Preparation Checks

Before using live preprod e2e as a debugging loop for builder, wallet,
validity-window, worker, DA, or post-submit recovery changes, run the focused
feedback ladder in [TX_PREP_FEEDBACK_LADDER.md](./docs/TX_PREP_FEEDBACK_LADDER.md).
The package-level shortcuts are:

```sh
cd ../
pnpm run test:tx-prep:sdk
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
```

Record the relevant lower-layer commands in live acceptance evidence. If live
e2e finds a deterministic transaction-preparation defect, add a local or
emulator regression before rerunning the full live flow.

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

   1. Demo-node acceptance supports `L1_PROVIDER=Kupmios` only. Use local Kupo
      and Ogmios endpoints, and run
      `node dist/index.js l1-provider-preflight --json` before long-running
      deployment steps.
   2. If local Kupo or Ogmios is unhealthy, fix the local
      `docker-compose.kupmios.yaml` stack instead of switching to a remote L1
      provider.

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

   `docker compose up` starts PostgreSQL, runs the one-shot
   `midgard-node-migrate` service with `node ./dist/index.js db:migrate`, and
   starts `midgard-node` only after the migration service exits successfully.
   On an empty Postgres volume this creates the Midgard schema before the node
   begins listening. If migration fails, the node is not started; inspect the
   migration logs with:

   ```sh
   docker compose logs midgard-node-migrate
   ```

7. To run against an in-stack local `Kupmios` provider backed by a Mithril
   bootstrap, start with the compose override:

   ```sh
   docker compose -f docker-compose.yaml -f docker-compose.kupmios.yaml up -d
   ```

   Notes:

   1. The first run restores a Mithril-certified Cardano DB snapshot into
      `./cardano/db` only when that directory is empty. Existing data is never
      overwritten implicitly.
   2. `NETWORK` is the source of truth. The bootstrap validates that
      `CARDANO_NODE_IMAGE_TAG` is new enough for the certified Mithril snapshot
      and that any explicit Mithril endpoints/keys all match that network
      before the local Cardano stack is allowed to start.
   3. Changing networks requires explicit cleanup of `./cardano/db` and
      `./cardano/kupo` before restarting the stack.
   4. The local stack restores an official Kupo SQLite snapshot into
      `./cardano/kupo` when that directory is empty, then continues syncing
      with `--match * --since origin --prune-utxo`. That preserves a full
      wildcard current-UTxO index without forcing every fresh checkout to
      start from an empty Kupo database.
   5. Kupo is considered healthy only once its `/health` endpoint returns
      `200`, not while it is still returning `202 Accepted` during replay. That
      keeps `midgard-node` from starting against a stale wildcard index.
   6. The local stack intentionally runs standalone `cardano-node` and Ogmios
      containers instead of the combined `cardano-node-ogmios` image, because
      the certified Mithril snapshot can move ahead of that combined image's
      bundled Cardano node version.

Midgard node should be running on port `PORT` (from your `.env`).

You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
docker logs -f midgard-node-midgard-node-1
```

If you made any changes to `midgard-node` and had an image running, restart it
without deleting durable state:

```sh
docker compose stop midgard-node
docker compose up -d --build
```

Only wipe Docker volumes or local MPF/PostgreSQL state as part of a full clean
protocol redeploy. Do not combine a fresh local database with previously
deployed on-chain protocol state.

### Without Docker (No Monitoring)

For running the node itself, a running PostgreSQL server is also needed. The
fields you most likely want to modify in your `.env` file are:

```sh
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DB=midgard
POSTGRES_HOST=localhost
LEDGER_MPF_DB_PATH=midgard-ledger-mpf-db
TRANSACTIONS_MPF_DB_PATH=midgard-transactions-mpf-db
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
- `node dist/index.js prepare-hub-oracle-one-shot-nonce`: create a fresh marked
  operator-wallet UTxO and print the `HUB_ORACLE_ONE_SHOT_*` values needed for a
  clean protocol deployment.
- `node dist/index.js deploy-reference-script-node-runtime`: generate the
  reference-script auth timelock policy, publish the node-runtime reference
  scripts with role tokens, and write the deployment-info manifest.
- `pnpm init`: initialize hub-oracle, state-queue, operator roots, and
  scheduler state.
- `node dist/index.js export-contract-deployment-info --out <path>`: write a
  JSON manifest describing the currently configured validator bundle,
  generated reference-script auth policy, and any published reference-script
  UTxOs visible at the configured reference-script deploy address.
- `node dist/index.js submit-deposit`: build and submit an L1 deposit into the
  Midgard deposit contract for a target L2 address. Submitted deposits are
  journaled before confirmation wait so timeouts can be reconciled by tx hash.
- `pnpm submit:l2-transfer`: build and submit a Midgard-native user transfer.
- `node dist/index.js project-deposits-once`: fetch L1 deposit events once and
  project newly visible deposits into the local Midgard ledger view.
- `pnpm audit:blocks-immutable`: inspect immutable block state and related
  persistence.
- The e2e step runner, service supervisor, run finalizer, stress-wallet
  tooling, corpus generator/verifier, bounded L2 stress harness, and the Phase
  4 local-devnet acceptance gate are `midgard-node-tools` commands
  (`../midgard-node-tools/dist/index.js`); see
  [`../midgard-node-tools/README.md`](../midgard-node-tools/README.md). None
  of them ship in this operator binary.
- `pnpm stress:valid`: run the high-throughput valid-transaction submitter.

### Confirmation polling default

As of the throughput Phase 4 work, `WAIT_BETWEEN_BLOCK_CONFIRMATION` defaults
to `2000` ms instead of `10000` ms. Pending submissions use a targeted tx probe
before a periodic full state-queue scan, so the shorter detection interval does
not turn every poll into an O(queue length) provider request. Operators can set
the variable back to `10000` as an operational rollback.

`SPECULATIVE_COMMIT_BUILD` remains disabled by default until the Phase 3 MPF
growth gate authorizes `MPF_ENGINE=overlay` as the default. Enabling speculation
explicitly while using the legacy MPF engine fails configuration validation.

- `pnpm bench:l2:scenario:<name>`: run a named benchmark scenario and write a
  scenario artifact under `benchmark-results/<git-sha>/`.
- `pnpm stress:nominal`: run the lower-rate sustained activity generator.

## Reconciliation Commands

Use `node dist/index.js reconcile <milestone> --json` after an interrupted e2e
run, timeout, provider lag, or worker restart. Commands are read-only by
default and return `schemaVersion: "midgard-e2e-reconciliation-v1"` plus
`satisfied`, `pending`, `repaired`, `blocked`, `ambiguous`, or `failed`.
`--repair` only runs existing idempotent recovery paths.

```sh
node dist/index.js reconcile phas-registered --json [--repair]
node dist/index.js reconcile reference-scripts-complete --scope node-runtime --json [--repair]
node dist/index.js reconcile deposit-projected --cardano-tx-hash <hash> --json [--repair]
node dist/index.js reconcile tx-committed --tx-hash <l2-tx-id> --json
node dist/index.js reconcile da-attested --header-hash <hash> --watcher-url <url> --contract-deployment-info deploymentInfo/contract-deployment-info.json --json [--repair]
node dist/index.js reconcile block-committed --header-hash <hash> --json
node dist/index.js reconcile merge-complete --header-hash <hash> --json [--repair]
```

For deposit confirmation timeouts, use:

```sh
node dist/index.js reconcile-deposit-submission --tx-hash <cardano-tx-hash> --json
```

If a reconciler reports `ambiguous`, do not blindly repeat the original
state-changing step. Inspect the emitted evidence and next action first.

## HTTP Surface

The main listener exposes a small operator-facing API. Common routes include:

- `/deposit/build` for building unsigned L1 deposit transactions from a
  caller-supplied wallet view,
- `/submit` for submitting raw Midgard canonical transaction CBOR with
  `Content-Type: application/cbor`,
- `/utxo` for querying one spendable Midgard mempool-ledger UTxO by raw
  TxOutRef CBOR hex,
- `/utxos` for querying spendable Midgard mempool-ledger UTxOs either by
  address (`GET`) or by a requested list of `txHash#outputIndex` references
  at `POST /utxos?by-outrefs`,
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
- Override the default with `--wallet-seed-phrase-env <ENV_VAR>` or pass
  `--wallet-seed-phrase` directly for one-off manual testing.
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
  Midgard L2 outputs support only absent datums or inline datums; datum-hash
  outputs are rejected.
- `additionalAssets`: array of `{ unit, amount }` entries to deposit alongside
  lovelace.
- `fundingUtxos[].datumHash`, `fundingUtxos[].datum`, `fundingUtxos[].scriptRef`:
  optional hex fields preserved in the external wallet view when present.

The response includes:

- `unsignedTxCbor`: unsigned Cardano transaction CBOR hex.

## Export Contract Deployment Info

Write a manifest of the currently configured validator bundle. Contract entries
are keyed by explicit script names such as `depositMint` and `depositSpend`.

For a new deployment, publish reference scripts first; this generates the
reference-script auth policy used to parameterize DA attestation:

```sh
node dist/index.js prepare-hub-oracle-one-shot-nonce
```

Copy the printed `HUB_ORACLE_ONE_SHOT_TX_HASH` and
`HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX` into the deployment environment before
publishing reference scripts or running `init`.

```sh
node dist/index.js deploy-reference-script-node-runtime
```

Before publishing or republishing reference scripts, inspect the deploy wallet:

```sh
node dist/index.js reference-script-wallet-status --json
```

The status separates explorer-like total ADA from plain ADA-only ADA that can be
used for deployment funding. If stale script-ref or token-bearing ADA is present,
use `node dist/index.js sweep-reference-script-wallet` as a dry run and execute
it only after confirming the referenced scripts are retired.

```sh
cd midgard-node
pnpm build
node dist/index.js export-contract-deployment-info \
  --out contract-deployment-info.json
```

Each entry has the shape:

```json
{
  "schemaVersion": "midgard-deployment-manifest-v1",
  "manifestId": "...",
  "consensusProfile": {
    "profileId": "midgard-consensus-v1",
    "protocolVersion": 1
  },
  "network": "Preprod",
  "referenceScriptDeployAddress": "addr_test...",
  "hubOracleOneShot": {
    "txHash": "...",
    "outputIndex": 0,
    "outRef": "...#0",
    "status": "prepared"
  },
  "referenceScriptAuthPolicy": {
    "policyId": "...",
    "nativeScript": {
      "type": "Native",
      "cborHex": "...",
      "expiresAtSlot": 0,
      "expiresAtUnixTime": 0,
      "timelockDurationMs": 14400000
    },
    "tokenNames": {
      "state-queue minting": "StateQueueMint"
    },
    "postTimelockAudit": {
      "required": true,
      "rule": "..."
    }
  },
  "contracts": {
    "depositMint": {
      "refScriptUTxO": null,
      "contract": {
        "type": "PlutusV3",
        "cborHex": "..."
      },
      "scriptHash": "..."
    }
  },
  "referenceScripts": {},
  "steps": {}
}
```

Reference scripts are currently published to `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS`,
which defaults to the separate reference-script wallet address
`L1_REFERENCE_SCRIPT_ADDRESS` so stale test deployments can reclaim ADA. Before
production, set `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS` to the intended
non-spendable reference-script address and deploy fresh reference scripts.

Reference-script deployment creates a timelock native minting policy and mints
one role token into each published reference-script UTxO. The default window is
four hours (`REFERENCE_SCRIPT_AUTH_TIMELOCK_MS=14400000`), and publication fails
before submitting a batch when less than
`REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS` remains. The policy metadata is written
to `referenceScriptAuthPolicy` in the manifest. After the timelock expires, the
deployment must be audited before production use: for every token name listed in
`referenceScriptAuthPolicy.tokenNames`, exactly one token under
`referenceScriptAuthPolicy.policyId` must exist.

`deployment-status` reports both the V1 manifest verification result and live
protocol deployment status. If a present manifest disagrees with configured
network, one-shot outref, or reference-script deploy address, startup refuses to
attach until config is corrected or a fresh redeploy is explicit.

`init` now always writes the manifest. By default it goes to the repository root
at `deploymentInfo/contract-deployment-info.json`. If you want to override that
path, pass:

```sh
node dist/index.js init \
  --contract-deployment-info-output contract-deployment-info.json
```

## Parallel Fanout Stress Wallets

Stress-wallet creation, funding, fan-out, and the bounded
`e2e-stress-l2-throughput` harness moved to `midgard-node-tools`; see
[`../midgard-node-tools/README.md`](../midgard-node-tools/README.md). They
drive this node from the outside and are not part of its binary.

## Valid Throughput Stress Test

Run the high-throughput benchmark engine against a prebuilt, verified corpus of
signed Midgard-native transactions. Corpus mode performs no `/utxos` lookup or
transaction construction in the measured path. It reports offered, queued,
accepted, committed, and merge rates from client and Prometheus evidence. The
`e2e-stress-l2-throughput` CLI separately collects SQL-grounded stage metrics
for functional/e2e stress runs.

Generate the corpus from already prepared stress-wallet snapshots with the
tooling CLI (built with `pnpm --dir ../midgard-node-tools build`), then verify
it before use:

```sh
cd demo/midgard-node
TOOLS_CLI=../midgard-node-tools/dist/index.js

node "$TOOLS_CLI" stress-corpus-generate \
  --target-rate-tps 2500 \
  --duration-ms 300000 \
  --wallets-dir .stress-wallets \
  --out-dir corpus/accept-2500 \
  --yes

node "$TOOLS_CLI" stress-corpus-verify \
  --corpus-path corpus/accept-2500/corpus.ndjson \
  --rebuild-wallets-dir .stress-wallets
```

Select a manifest slice and point the runner at the corpus:

```sh
STRESS_CORPUS_PATH=corpus/accept-2500/corpus.ndjson \
STRESS_CORPUS_SLICE_ID=default \
STRESS_CORPUS_SHAPE=fanout \
pnpm stress:valid
```

Named scenarios set their rate, duration, and acceptance gates consistently:

```sh
pnpm bench:l2:scenario:find-max-ramp
pnpm bench:l2:scenario:accept-2500-tps-gate
pnpm bench:l2:scenario:soak-10min-at-max
pnpm bench:l2:scenario:burst-2x-target
```

The scenario wrapper does not currently enforce corpus mode. Set both
`STRESS_CORPUS_PATH` and `STRESS_CORPUS_SLICE_ID` for comparable Class A
evidence; if they are absent, the engine falls back to the legacy transaction
builder path. Use `--dry-run` directly with `scripts/benchmark-scenario.mjs` to
inspect the resolved scenario environment before a run.
The benchmark env also requires no-op calibration by default: start
`pnpm stress:noop` separately and set `STRESS_NOOP_ENDPOINT` to that echo
server, or explicitly disable the requirement for a non-gating diagnostic run.

For an isolated Compose run, layer `docker-compose.benchmark.yaml` over the
normal stack and set deployment-specific values in `.env.benchmark`. The
optional cohosted load generator is behind the `load-generator-cohosted`
profile; a separate load-generator host is the preferred topology.

Additional run matrix:

- `pnpm bench:l2:valid`: fixed-rate or closed-loop L2 admission run.
- `pnpm bench:l2:find-max`: searches for the maximum sustainable accepted
  L2 tx/s, then confirms the best candidate with repeat steady-state runs.
- `STRESS_WAIT_FOR_COMMIT=true pnpm bench:l2:find-max`: commit-aware run that
  reports committed tx/s separately from accepted tx/s.
- `STRESS_WAIT_FOR_MERGE=true STRESS_WAIT_FOR_COMMIT=true pnpm bench:l2:find-max`:
  end-to-end run that reports merge blocks/s. It does not report merged tx/s
  until the node exposes a merged-transaction counter.
- `pnpm bench:l2:profile`: profiling run with Node CPU/heap profiles. Enable
  `STRESS_PG_STAT_STATEMENTS=true` and `STRESS_PYROSCOPE=true` when those
  services are available.

Notes:

- Corpus mode reads canonical transaction bytes from NDJSON and submits raw
  CBOR to `/submit` with `Content-Type: application/cbor`.
- The legacy no-corpus mode still reads test wallets and `/utxos`; do not use
  that mode for repeatable acceptance or regression evidence.
- It uses pooled Undici HTTP clients and supports `STRESS_MODE=closed`,
  `STRESS_MODE=open`, `STRESS_MODE=ramp`, and `STRESS_MODE=find-max`.
- `STRESS_MODE=find-max` is a candidate search, not a ramp average. It records
  each candidate's pass/fail reasons and reports one
  `maxSustainableAcceptedTxPerSec` only after confirmation repeats pass.
- Measured submit stages do not retry `429`/`503` by default
  (`STRESS_MEASURED_RETRY_503=0`). Queue-full responses are treated as ingress
  saturation evidence.
- For trustworthy deltas, run against a dedicated or demonstrably idle node.
  The runner enforces this by default with `STRESS_REQUIRE_IDLE_NODE=true`
  because Prometheus counters are global.
- It reports fixed 1s/5s/30s rolling rates, measured-window average TPS,
  submit/status latency percentiles, runtime event-loop stats, and a likely
  bottleneck classification.
- Rate names are explicit: physical submit attempts/s, queued submit
  successes/s, accepted tx/s, committed tx/s, and merge blocks/s.
- It writes a JSON artifact to `benchmark-results/l2-throughput-*.json` unless
  `STRESS_REPORT_PATH` is provided. The artifact includes git/runtime metadata,
  candidate evaluations, pass/fail reasons, and the metric evidence used for
  bottleneck classification.

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

## DA Payload Hardening and Rollout

DA payload storage and transport accept one durable format:
`DaPayloadEnvelopeV1`. It contains exact `DaPayloadV1` bytes, their decoded
length and SHA-256, and an explicit `identity` or `zstd` content encoding.
Raw payload storage, an `off` mode, and format inference are rejected.

Both the stored/transmitted envelope and its declared and actual decoded
content are capped by the pinned DA protocol limit. Zstd decoding uses
`maxOutputLength`, then verifies exact length and inner SHA-256 before the
existing strict payload validator runs. Midgard node and committee runtimes
therefore require Node.js 22.15 or newer.

Retained-payload and fault-proof consumers preserve the stored artifact as the
hash identity. They carry `payloadSchemaVersion` from retained metadata, verify
the stored-byte SHA-256, and then use the pinned-bound envelope unwrap before
strictly decoding the inner `DaPayloadV1`. Fault-proof callers should use
`fetchRetainedDaPayloadByHeaderHash` followed by `reconstructDaPayloadV1`;
unsupported or malformed envelope and payload versions fail closed.

Publication returns once the manifest threshold accepts. Slow peers continue
as detached, bounded stragglers. The `da_payload_publications` durable outbox
records every current committee peer, is recovered from `da_payloads` after a
process crash, and retries incomplete replication with exponential backoff.
Terminal success is monotone; a late transport failure cannot downgrade it,
while a payload conflict has evidence-preserving precedence and stops retry.
Owner/token/expiry fencing also applies to completion: a detached foreground
result may update only an unleased or expired row and cannot clear or overwrite
an active reconciler claim. Fence loss is surfaced rather than treated as a
successful durable write.
Local block finalization remains durable when immediate DA publication is
below threshold, but merge safety is unchanged: an on-chain DA attestation is
still mandatory, and startup fails if transport threshold is lower than the
on-chain threshold.

The controls are documented in `.env.example`. Protocol byte limits and the
acceptance threshold deliberately have no environment override.

## Related Documentation

- [Root repository guide](../../README.md)
- [Midgard SDK guide](../midgard-sdk/README.md)
- [Preprod deposit and send-tx runbook](./docs/PREPROD_DEPOSIT_AND_SEND_TX.md)
- [Technical specification guide](../../technical-spec/README.md)
