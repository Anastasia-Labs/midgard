# Live Acceptance Runbook

Use this reference for fresh runs and value-submitting attach/resume runs. Read
it completely before changing live state.

## Contents

1. [Shared preparation](#shared-preparation)
2. [Attach or resume](#attach-or-resume)
3. [Fresh deployment](#fresh-deployment)
4. [DA manifests and watcher](#da-manifests-and-watcher)
5. [Node, deposit, and L2 activity](#node-deposit-and-l2-activity)
6. [DA, finality, and automatic merge](#da-finality-and-automatic-merge)
7. [State-correction and recovery acceptance](#state-correction-and-recovery-acceptance)
8. [Final evidence](#final-evidence)

## Shared preparation

Start from the repository root. Keep local Preprod provider state intact.

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
NODE_DIR="$REPO_ROOT/demo/midgard-node"
DA_NODE_DIR="$REPO_ROOT/demo/da-committee-node"
cd "$NODE_DIR"

COMPOSE="docker compose -f docker-compose.yaml -f docker-compose.kupmios.yaml"
RUN_ID="${RUN_ID:-e2e-run-$(date -u +%Y%m%dT%H%M%SZ)}"
E2E_STEP_DIR="logs/$RUN_ID/steps"
RUN_STATE_PATH="logs/$RUN_ID/deployment-run-state.json"
mkdir -p "$E2E_STEP_DIR" logs deploymentInfo db
```

Verify `.env` without printing seed phrases:

- `NETWORK=Preprod`;
- `L1_PROVIDER=Kupmios` and no `L1_PROVIDER_FAILOVER`;
- host Kupo/Ogmios endpoints use `127.0.0.1`, while container endpoints use
  Compose service names;
- `RUN_GENESIS_ON_STARTUP=false`;
- distinct operator, reference-script, merge, user, and DA submitter roles;
- `MIDGARD_DEPLOYMENT_MANIFEST_PATH` points to the producer runtime manifest;
  and
- `DA_LIBP2P_PRIVATE_KEY_SOURCE` matches the producer manifest identity.

Build the current CLI and start only the local provider plumbing needed by host
commands. Compose dependencies start Cardano node and bootstrap services.

```bash
pnpm build
$COMPOSE up -d cardano-node-ogmios kupo
node dist/index.js l1-provider-preflight --json
```

Do not continue until the preflight reports local Kupmios healthy and no
failover.

## Attach or resume

Do not run `init` or reset local state.

```bash
node dist/index.js deployment-status
node dist/index.js reference-script-wallet-status --json
node dist/index.js l1-provider-preflight --json
node dist/index.js reconcile phas-registered --json
node dist/index.js reconcile reference-scripts-complete \
  --scope node-runtime \
  --json
```

Verify manifest ID and SHA-256, network, hub-oracle one-shot, reference-script
auth policy and UTxOs, operator status, provider route, DB route, and wallet
role addresses. Use addresses or hashes in evidence, never seed phrases.

For an interrupted milestone, read `recovery.md`, reconcile the specific
transaction, and add the previous and recovery step summaries to the final
dashboard. A recovery summary uses `--mode resume`; an ordinary attach summary
uses `--mode attach`.

Once identity is proven, generate or verify the DA manifests as described below,
start the watcher, run the producer preflight in the appropriate mode, and then
start the node with `$COMPOSE up -d midgard-node`.

## Fresh deployment

### Build contracts and images

```bash
cd "$REPO_ROOT/onchain/aiken"
aiken build --env testnet
cd "$NODE_DIR"
pnpm build
$COMPOSE build midgard-node midgard-node-migrate
```

### Create the fresh hub-oracle one-shot

The local Kupmios stack must be healthy before this transaction.

```bash
HUB_ORACLE_NONCE_LOG="logs/$RUN_ID/hub-oracle-nonce.log"
HUB_ORACLE_NONCE_STEP="$E2E_STEP_DIR/hub-oracle-nonce.json"
node dist/index.js e2e-run-step \
  --id hub-oracle-nonce \
  --cwd "$NODE_DIR" \
  --raw-log "$HUB_ORACLE_NONCE_LOG" \
  --summary-out "$HUB_ORACLE_NONCE_STEP" \
  --timeout-ms 1200000 \
  -- \
  node dist/index.js prepare-hub-oracle-one-shot-nonce \
  --run-state "$RUN_STATE_PATH" \
  --fresh-redeploy \
  --fresh-redeploy-reason "fresh e2e acceptance $RUN_ID" \
  --json
```

Patch `.env` with the returned `txHash` and `outputIndex`. Confirm they differ
from the previous deployment and that the UTxO is funded and confirmed. Keep
the same `$RUN_STATE_PATH` for reference-script publication.

If the command reports insufficient funding, use `l1-utxos` or
`reference-script-wallet-status --json` for the intended role, fund it, wait
for confirmation, and retry only after proving the first attempt did not
submit.

### Reset only matching Midgard state

This reset is allowed only because the same run proceeds to fresh reference
scripts and fresh `init`. It deliberately does not delete `cardano/db` or
`cardano/kupo`.

```bash
$COMPOSE down -v --remove-orphans
mkdir -p db deploymentInfo logs
docker run --rm \
  -v "$PWD/deploymentInfo:/mnt/deploymentInfo" \
  -v "$PWD/db:/mnt/db" \
  busybox sh -lc \
  'rm -rf /mnt/db/* /mnt/deploymentInfo/* && chown -R 1000:1000 /mnt/db /mnt/deploymentInfo'

$COMPOSE up -d cardano-node-ogmios kupo
node dist/index.js l1-provider-preflight --json
```

The run-state is under `logs/$RUN_ID`, so the reset preserves the new
deployment identity.

### Publish node-runtime reference scripts

```bash
REFERENCE_LOG="logs/$RUN_ID/reference-scripts.log"
REFERENCE_STEP="$E2E_STEP_DIR/reference-scripts.json"
node dist/index.js e2e-run-step \
  --id reference-scripts \
  --cwd "$NODE_DIR" \
  --raw-log "$REFERENCE_LOG" \
  --summary-out "$REFERENCE_STEP" \
  --timeout-ms 10800000 \
  -- \
  node dist/index.js deploy-reference-script-node-runtime \
  --run-state "$RUN_STATE_PATH" \
  --contract-deployment-info-output \
  deploymentInfo/contract-deployment-info.json

node dist/index.js reconcile reference-scripts-complete \
  --scope node-runtime \
  --json
```

Batch splits and funding-input escalation are progress while transactions keep
submitting and confirming. Record duration, policy ID, UTxO count, batch splits,
highest funding-input count, step summary, and raw log.

If interrupted, preserve the run-state and log. Resume only when network,
one-shot, manifest path, and auth policy all match. Never guess a policy or mix
identities.

### Initialize and activate the operator

```bash
INIT_LOG="logs/$RUN_ID/init-protocol.log"
INIT_STEP="$E2E_STEP_DIR/init-protocol.json"
node dist/index.js e2e-run-step \
  --id init-protocol \
  --cwd "$NODE_DIR" \
  --raw-log "$INIT_LOG" \
  --summary-out "$INIT_STEP" \
  --timeout-ms 1200000 \
  -- \
  node dist/index.js init \
  --contract-deployment-info-output \
  deploymentInfo/contract-deployment-info.json

INIT_TX_HASH="$(node --input-type=module -e '
  import { readFileSync } from "node:fs";
  const summary = JSON.parse(readFileSync(process.argv[1], "utf8"));
  const hash = summary?.parsedJson?.initTxHash
    ?? summary?.parsedJson?.txHash
    ?? summary?.observedTxHashes?.[0];
  if (typeof hash !== "string" || !/^[0-9a-f]{64}$/i.test(hash)) {
    throw new Error("init step summary contains no transaction hash");
  }
  process.stdout.write(hash.toLowerCase());
' "$INIT_STEP")"

node dist/index.js reconcile deployment-manifest \
  --out deploymentInfo/contract-deployment-info.json \
  --init-tx-hash "$INIT_TX_HASH" \
  --json

OPERATOR_LOG="logs/$RUN_ID/operator-lifecycle.log"
OPERATOR_STEP="$E2E_STEP_DIR/operator-lifecycle.json"
node dist/index.js e2e-run-step \
  --id operator-lifecycle \
  --cwd "$NODE_DIR" \
  --raw-log "$OPERATOR_LOG" \
  --summary-out "$OPERATOR_STEP" \
  --timeout-ms 1200000 \
  -- \
  node dist/index.js register-active-operator
```

The combined operator command may resume an exactly-one-registered,
not-yet-active operator. Use `activate-operator` only after chain evidence proves
that exact recovery state. Do not deregister or rewrite SQL to recover.

## DA manifests and watcher

Generate three manifests from the finalized v2 contract deployment manifest:

- a producer runtime manifest for the producer container;
- a producer host-preflight manifest used while the producer is stopped; and
- a watcher runtime manifest for the host watcher.

The host-preflight manifest matters: `host.docker.internal` is a container route,
so the host preflight must use the `host` address profile.

Set an explicit funded L1 submitter key source without printing it. Supported
forms include `seed:<mnemonic>` and `file:<path-containing-a-supported-source>`.
Keep this wallet distinct from the operator and DA signer.

The command block below is the default legacy 1-of-1 profile. If `.env`
configures a larger committee or threshold, supply one `--committee-member`
entry per configured member, generate a target-specific watcher manifest for
each signer, and start enough watcher instances to reach the configured
threshold. Do not lower the threshold or collapse the committee to make the
run pass.

```bash
cd "$NODE_DIR"
set -a
. ./.env
set +a

: "${L1_OPERATOR_SEED_PHRASE:?missing L1 operator seed phrase}"
: "${DA_L1_SUBMITTER_KEY_SOURCE:?set a funded DA L1 submitter key source}"

CONTRACT_INFO="$NODE_DIR/deploymentInfo/contract-deployment-info.json"
PRODUCER_MANIFEST="$NODE_DIR/deploymentInfo/da-libp2p-producer-manifest.json"
PRODUCER_PREFLIGHT_MANIFEST="$NODE_DIR/deploymentInfo/da-libp2p-producer-host-preflight-manifest.json"
WATCHER_MANIFEST="$DA_NODE_DIR/run/$RUN_ID-watcher-manifest.json"
WATCHER_DB="$DA_NODE_DIR/run/$RUN_ID-watcher-store.json"
PRODUCER_LIBP2P_KEY_SOURCE="${DA_PRODUCER_LIBP2P_KEY_SOURCE:-seed:0000000000000000000000000000000000000000000000000000000000000001}"
WATCHER_LIBP2P_KEY_SOURCE="${DA_WATCHER_LIBP2P_KEY_SOURCE:-seed:0000000000000000000000000000000000000000000000000000000000000002}"
DA_THRESHOLD="${DA_THRESHOLD:-1}"
mkdir -p "$DA_NODE_DIR/run" "$DA_NODE_DIR/db"

DA_VKEY="$(node --input-type=module <<'NODE'
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
const network = process.env.NETWORK || "Preprod";
const seed = process.env.L1_OPERATOR_SEED_PHRASE;
if (!seed) throw new Error("L1_OPERATOR_SEED_PHRASE is required");
const wallet = walletFromSeed(seed, { network });
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
process.stdout.write(
  Buffer.from(privateKey.to_public().to_raw_bytes()).toString("hex"),
);
NODE
)"

COMMON_MANIFEST_ARGS=(
  --contract-deployment-info "$CONTRACT_INFO"
  --producer-libp2p-key-source "$PRODUCER_LIBP2P_KEY_SOURCE"
  --threshold "$DA_THRESHOLD"
  --committee-member "0,$DA_VKEY,$WATCHER_LIBP2P_KEY_SOURCE,committee+retrieval+coordinator"
  --network "${NETWORK:-Preprod}"
)

node dist/index.js da-libp2p-generate-manifest \
  --target producer \
  --profile producer-container-watcher-host \
  "${COMMON_MANIFEST_ARGS[@]}" \
  --out "$PRODUCER_MANIFEST"

node dist/index.js da-libp2p-generate-manifest \
  --target producer \
  --profile host \
  "${COMMON_MANIFEST_ARGS[@]}" \
  --out "$PRODUCER_PREFLIGHT_MANIFEST"

node dist/index.js da-libp2p-generate-manifest \
  --target watcher \
  --profile producer-container-watcher-host \
  "${COMMON_MANIFEST_ARGS[@]}" \
  --local-signer-index 0 \
  --out "$WATCHER_MANIFEST"
```

Ensure `.env` contains the container-relative producer runtime path and matching
producer identity, and does not define `DA_PAYLOAD_ENDPOINTS`:

```dotenv
MIDGARD_DEPLOYMENT_MANIFEST_PATH=deploymentInfo/da-libp2p-producer-manifest.json
DA_LIBP2P_PRIVATE_KEY_SOURCE=seed:0000000000000000000000000000000000000000000000000000000000000001
```

Build the watcher and export its environment for the wallet preflight and
service. `export` is a shell builtin, so secret-bearing values do not become
child-process arguments. Do not enable shell tracing or print the array because
it contains key sources.

```bash
pnpm --dir "$DA_NODE_DIR" install --frozen-lockfile
pnpm --dir "$DA_NODE_DIR" build

if [ -z "${DA_COMMITTEE_HEX:-}" ]; then
  unset DA_COMMITTEE_HEX
fi

DA_WATCHER_ENV=(
  "MIDGARD_NETWORK=${NETWORK:-Preprod}"
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH=$WATCHER_MANIFEST"
  "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH=$CONTRACT_INFO"
  "CARDANO_PROVIDER_URLS=kupmios:http://127.0.0.1:${KUPO_PORT:-1442}|http://127.0.0.1:${OGMIOS_PORT:-1337}"
  "CARDANO_FINALITY_DEPTH=${CARDANO_FINALITY_DEPTH:-2}"
  "DA_LIBP2P_PRIVATE_KEY_SOURCE=$WATCHER_LIBP2P_KEY_SOURCE"
  "DA_SIGNER_INDEX=0"
  "DA_SIGNER_KEY_SOURCE=cardano-seed:$L1_OPERATOR_SEED_PHRASE"
  "DA_THRESHOLD=$DA_THRESHOLD"
  "DA_L1_SUBMISSION_ENABLED=true"
  "L1_SUBMITTER_KEY_SOURCE=$DA_L1_SUBMITTER_KEY_SOURCE"
  "WATCHER_DB_PATH=$WATCHER_DB"
  "WATCHER_API_HOST=127.0.0.1"
  "WATCHER_API_PORT=8787"
  "WATCHER_POLL_INTERVAL_MS=15000"
)
export "${DA_WATCHER_ENV[@]}"

DA_WALLET_PREFLIGHT_LOG="logs/$RUN_ID/da-l1-wallet-preflight.log"
DA_WALLET_PREFLIGHT_STEP="$E2E_STEP_DIR/da-l1-wallet-preflight.json"
node dist/index.js e2e-run-step \
  --id da-l1-wallet-preflight \
  --cwd "$NODE_DIR" \
  --raw-log "$DA_WALLET_PREFLIGHT_LOG" \
  --summary-out "$DA_WALLET_PREFLIGHT_STEP" \
  --timeout-ms 180000 \
  -- \
  node "$DA_NODE_DIR/dist/index.js" l1-wallet-preflight --json
```

Stop if the submitter wallet is not ready. Fund the distinct submitter wallet,
wait for confirmation, and rerun the preflight.

Start the watcher before probing producer-to-committee reachability:

```bash
DA_NODE_LOG="logs/$RUN_ID/da-committee-node.log"
DA_NODE_PID="$DA_NODE_DIR/run/$RUN_ID-watcher.pid"
node dist/index.js e2e-start-service \
  --service da-committee-node \
  --cwd "$DA_NODE_DIR" \
  --raw-log "$DA_NODE_LOG" \
  --pid-file "$DA_NODE_PID" \
  --ready-url http://127.0.0.1:8787/readyz \
  --health-url http://127.0.0.1:8787/healthz \
  --ready-timeout-ms 180000 \
  --poll-interval-ms 5000 \
  pnpm start

# The managed watcher has inherited its environment. Remove the values from
# this shell before starting producer-side commands.
for assignment in "${DA_WATCHER_ENV[@]}"; do
  unset "${assignment%%=*}"
done
unset DA_WATCHER_ENV

DA_LIBP2P_PREFLIGHT_LOG="logs/$RUN_ID/da-libp2p-bind-listen-preflight.log"
DA_LIBP2P_PREFLIGHT_STEP="$E2E_STEP_DIR/da-libp2p-bind-listen-preflight.json"
node dist/index.js e2e-run-step \
  --id da-libp2p-bind-listen-preflight \
  --cwd "$NODE_DIR" \
  --raw-log "$DA_LIBP2P_PREFLIGHT_LOG" \
  --summary-out "$DA_LIBP2P_PREFLIGHT_STEP" \
  --timeout-ms 180000 \
  -- \
  env \
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH=$PRODUCER_PREFLIGHT_MANIFEST" \
  "DA_LIBP2P_PRIVATE_KEY_SOURCE=$PRODUCER_LIBP2P_KEY_SOURCE" \
  node dist/index.js da-libp2p-preflight --mode bind-listen --json
```

Require `passed: true`, a bound producer listener, and reachable committee
signer indexes at or above threshold. If the producer port is already bound,
stop the stale producer and rerun. Do not substitute `dial-only` for fresh
listener bind evidence.

After the producer is running, attach/resume diagnostics may use `dial-only`
with the runtime producer manifest. Label that evidence as reachability only.

## Node, deposit, and L2 activity

### Start the node and prove readiness

```bash
$COMPOSE up -d midgard-node

READY_LOG="logs/$RUN_ID/midgard-node-ready.log"
READY_STEP="$E2E_STEP_DIR/midgard-node-ready.json"
node dist/index.js e2e-run-step \
  --id midgard-node-ready \
  --cwd "$NODE_DIR" \
  --raw-log "$READY_LOG" \
  --summary-out "$READY_STEP" \
  --timeout-ms 180000 \
  -- \
  node --input-type=module -e '
    const deadline = Date.now() + 170000;
    while (Date.now() < deadline) {
      const health = await fetch("http://127.0.0.1:3000/healthz").catch(() => null);
      const ready = await fetch("http://127.0.0.1:3000/readyz").catch(() => null);
      if (health?.ok && ready?.ok) {
        const body = await ready.json();
        console.log(JSON.stringify({ healthz: await health.json(), readyz: body }));
        process.exit(body.ready === true && (body.reasons?.length ?? 0) === 0 ? 0 : 1);
      }
      await new Promise((resolve) => setTimeout(resolve, 5000));
    }
    process.exit(1);
  '
```

### Deposit and project

```bash
USER_L2_ADDRESS="$(node --input-type=module -e '
  import "dotenv/config";
  import { walletFromSeed } from "@lucid-evolution/lucid";
  console.log(walletFromSeed(process.env.USER_SEED_PHRASE, {
    network: process.env.NETWORK,
  }).address);
')"
DEST_A="$(node --input-type=module -e '
  import "dotenv/config";
  import { walletFromSeed } from "@lucid-evolution/lucid";
  console.log(walletFromSeed(process.env.TESTNET_GENESIS_WALLET_SEED_PHRASE_A, {
    network: process.env.NETWORK,
  }).address);
')"
DEST_B="$(node --input-type=module -e '
  import "dotenv/config";
  import { walletFromSeed } from "@lucid-evolution/lucid";
  console.log(walletFromSeed(process.env.TESTNET_GENESIS_WALLET_SEED_PHRASE_B, {
    network: process.env.NETWORK,
  }).address);
')"

DEPOSIT_LOG="logs/$RUN_ID/submit-deposit.log"
DEPOSIT_STEP="$E2E_STEP_DIR/submit-deposit.json"
node dist/index.js e2e-run-step \
  --id submit-deposit \
  --cwd "$NODE_DIR" \
  --raw-log "$DEPOSIT_LOG" \
  --summary-out "$DEPOSIT_STEP" \
  --timeout-ms 1200000 \
  -- \
  env POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433 \
  node dist/index.js submit-deposit \
  --wallet-seed-phrase-env USER_SEED_PHRASE \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000

PROJECT_DEPOSITS_LOG="logs/$RUN_ID/project-deposits.log"
PROJECT_DEPOSITS_STEP="$E2E_STEP_DIR/project-deposits.json"
node dist/index.js e2e-run-step \
  --id project-deposits \
  --cwd "$NODE_DIR" \
  --raw-log "$PROJECT_DEPOSITS_LOG" \
  --summary-out "$PROJECT_DEPOSITS_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js project-deposits-once"

curl -sf "http://127.0.0.1:3000/utxos?address=$USER_L2_ADDRESS"
```

If projection is empty, inspect `deposits_utxos.inclusion_time`. Wait until the
record is due plus a small buffer, then rerun `project-deposits-once`. A deposit
not yet due is not a failure.

### Submit two baseline L2 transfers

Require the watcher to remain ready first:

```bash
curl -sf http://127.0.0.1:8787/healthz
curl -sf http://127.0.0.1:8787/readyz

L2_TRANSFER_A_LOG="logs/$RUN_ID/submit-l2-transfer-a.log"
L2_TRANSFER_A_STEP="$E2E_STEP_DIR/submit-l2-transfer-a.json"
node dist/index.js e2e-run-step \
  --id submit-l2-transfer-a \
  --cwd "$NODE_DIR" \
  --raw-log "$L2_TRANSFER_A_LOG" \
  --summary-out "$L2_TRANSFER_A_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js submit-l2-transfer --wallet-seed-phrase-env USER_SEED_PHRASE --endpoint http://127.0.0.1:3000 --l2-address '$DEST_A' --lovelace 2000000"

L2_TRANSFER_B_LOG="logs/$RUN_ID/submit-l2-transfer-b.log"
L2_TRANSFER_B_STEP="$E2E_STEP_DIR/submit-l2-transfer-b.json"
node dist/index.js e2e-run-step \
  --id submit-l2-transfer-b \
  --cwd "$NODE_DIR" \
  --raw-log "$L2_TRANSFER_B_LOG" \
  --summary-out "$L2_TRANSFER_B_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js submit-l2-transfer --wallet-seed-phrase-env USER_SEED_PHRASE --endpoint http://127.0.0.1:3000 --l2-address '$DEST_B' --lovelace 1500000"
```

Use the `txId` fields from the two step summaries with `/tx-status?tx_hash=`.
Poll until both are `committed`. Do not treat `accepted` as committed or final.

## DA, finality, and automatic merge

For every committed header:

1. Fetch `/da/payload/metadata?header_hash=<hash>`.
2. Save `/da/payload?header_hash=<hash>` as a raw artifact.
3. Query the watcher deployment/header status.
4. Record payload hash/schema, watcher verification, attestation init,
   add-signatures, and apply transaction hashes.
5. Require watcher status `attested` or `merged` before automatic merge.

Derive the deployment fingerprint from the contract manifest ID, not a file
hash:

```bash
DEPLOYMENT_FINGERPRINT="$(node --input-type=module -e '
  import { readFileSync } from "node:fs";
  const manifest = JSON.parse(readFileSync(
    "deploymentInfo/contract-deployment-info.json",
    "utf8",
  ));
  if (manifest.schemaVersion !== "midgard-deployment-manifest-v2"
      || typeof manifest.manifestId !== "string") {
    throw new Error("expected finalized v2 deployment manifest");
  }
  process.stdout.write(manifest.manifestId.toLowerCase());
')"
```

If the watcher reports `root_mismatch`, `malformed_da`, or `conflicted`, stop.
Do not merge. Diagnose payload construction, retained data, and peer identity.

Wait for the running merge fiber to empty the state queue:

```bash
AUTOMATIC_MERGE_LOG="logs/$RUN_ID/await-automatic-merge.log"
AUTOMATIC_MERGE_STEP="$E2E_STEP_DIR/await-automatic-merge.json"
node dist/index.js e2e-run-step \
  --id await-automatic-merge \
  --cwd "$NODE_DIR" \
  --raw-log "$AUTOMATIC_MERGE_LOG" \
  --summary-out "$AUTOMATIC_MERGE_STEP" \
  --timeout-ms 900000 \
  -- \
  bash -lc '
    set -euo pipefail
    deadline=$((SECONDS + 900))
    while [ "$SECONDS" -lt "$deadline" ]; do
      body="$(curl -sf \
        -H "x-midgard-admin-key: ${ADMIN_API_KEY:-localdev-admin}" \
        http://127.0.0.1:3000/stateQueue)"
      printf "%s\n" "$body"
      if node --input-type=module - "$body" <<'"'"'NODE'"'"'
const body = JSON.parse(process.argv[2]);
process.exit(Array.isArray(body.headers) && body.headers.length === 0 ? 0 : 1);
NODE
      then
        exit 0
      fi
      curl -s http://127.0.0.1:3000/readyz || true
      curl -s \
        -H "x-midgard-admin-key: ${ADMIN_API_KEY:-localdev-admin}" \
        http://127.0.0.1:3000/stateQueueMutationLease || true
      sleep 5
    done
    echo "automatic merge fiber did not empty stateQueue" >&2
    exit 1
  '
```

If it times out, inspect readiness, the state queue, mutation lease, unfinished
jobs, scheduler refresh, commit/finality workers, and merge-fiber logs. Use
`recovery.md`; never call `/merge` to manufacture success.

## State-correction and recovery acceptance

The baseline deposit/L2/merge flow is not Q57 acceptance. A fresh final-release
run must also produce one
`midgard-e2e-state-correction-acceptance-v1` aggregate. This aggregate is an
index, not proof: none of its booleans or transaction hashes may become
confirmed evidence on their own. The finalizer must independently load and
reconcile the immutable workflow journals, authenticated terminal L1
observations, raw recovery outputs, deployment manifest, blueprint, catalogue,
parameters, release identity, economics, and final chain/queue observation.
Until all of those independent sources are present and agree, every
state-correction gate remains blocked. An absent, partial, inexact, cross-run,
or incomplete aggregate fails outright.

Each L1 observation must point to the unmodified raw Kupo match response, raw
Ogmios block response, and raw Ogmios tip response used to derive its inclusion
point and confirmation depth, with a recomputable SHA-256 for each file. The
final snapshot must likewise point to the raw Kupo empty-state-queue response,
one raw unspent quantity-one Kupo response for each permanent proof-token
unit/outref, the raw Ogmios tip, and a complete raw node-database export. Kupo,
Ogmios, and the database must agree. These captures are still claims: a
non-artifact authority must re-read the configured live services and approve
the derived facts. Mutually consistent files cannot substitute for that live
read.

Before any live drill, run the deterministic parser/gate rehearsal. It submits
nothing and does not touch the deployment:

```bash
cd "$NODE_DIR"
NODE_ENV=emulator pnpm exec vitest run \
  tests/e2e-state-correction-acceptance.test.ts \
  tests/e2e-state-correction-reconciliation.test.ts \
  tests/e2e-state-correction-local-authority.test.ts
```

Set the artifact path now and preserve it with the run:

```bash
STATE_CORRECTION_EVIDENCE="logs/$RUN_ID/state-correction-acceptance.json"
STATE_CORRECTION_MANIFEST="$CONTRACT_INFO"
STATE_CORRECTION_BLUEPRINT="$REPO_ROOT/onchain/aiken/plutus.json"
STATE_CORRECTION_CATALOGUE="logs/$RUN_ID/state-correction-catalogue.json"
STATE_CORRECTION_PARAMETERS="logs/$RUN_ID/cardano-protocol-parameters.json"
STATE_CORRECTION_RELEASE_EVIDENCE="logs/$RUN_ID/release-evidence.json"
STATE_CORRECTION_FINAL_SNAPSHOT="logs/$RUN_ID/state-correction-final-snapshot.json"
STATE_CORRECTION_WORKFLOW_JOURNAL_LIST="logs/$RUN_ID/state-correction-workflow-journals.txt"
STATE_CORRECTION_L1_OBSERVATION_LIST="logs/$RUN_ID/state-correction-l1-observations.txt"
STATE_CORRECTION_RECOVERY_OBSERVATION_LIST="logs/$RUN_ID/state-correction-recovery-observations.txt"
```

The three list files contain one absolute or run-relative path per line. Keep
workflow journals in canonical family order, authenticated L1 observations in
the order they were captured, and recovery observations in canonical recovery
matrix order. The finalizer validates the semantic identities and exact sets;
the list order does not grant trust.

Use the canonical launch-scope order from the finalized deployment catalogue.
For each family, the production watcher/workflow journal must do all of the
following from public L1+DA only:

1. detect the committed violation and record the violation and selected route;
2. initialize and complete every proof step with mandatory reference scripts;
3. confirm the permanent proof-token mint, its exact reference by the removal
   transaction, and the same unit/outref still retained after removal;
4. confirm state-queue removal and the corrected queue/root;
5. observe the configured operator slash and prover reward, recording expected
   and observed lovelace exactly; and
6. resume verification from the final chain point.

Use one drill instance as Q57, C83, and W45 evidence when it meets all three
claims. Do not submit a second transaction merely to give another task ID its
own hash. If an enabled family has no production watcher/workflow adapter,
stop: the sweep is not runnable and must not be replaced with manual proof CLI
steps or a hand-authored success record.

The same artifact must record a real withdrawal through order, reserve, payout
init, every payout add, and payout conclude. Hash the canonical expected and
observed payout/reserve values independently, require exact destination and
value equality, and retain the final paid chain point. The Q57 value digest is
SHA-256 over UTF-8 canonical JSON of a unit-to-decimal-string object: omit zero
quantities and sort keys lexicographically; use `lovelace` for ADA and the
dotless lowercase `policy_id || asset_name` unit for native assets. The payout
digest covers the exact output at the withdrawal destination. The reserve
digest covers the aggregate value of every currently unspent output at the
manifest-bound reserve validator address. The local authority re-reads the
payout transaction through Ogmios, cross-checks its complete output vector
against Kupo, and reads the current reserve UTxO set from Kupo. It must also
record both forced-classification directions in this order:

- valid block marked invalid, canonically restored to valid; and
- invalid block marked valid, publicly detected and corrected to invalid.

Both directions must be watcher-driven, route through the production workflow,
and bind their evidence/correction transactions and final chain points.

Finally, record the crash/rollback and fail-closed matrix in the exact order
published by
`REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS` in
`e2e-state-correction-acceptance.ts`. It includes the fourteen before/after
durable watcher crash boundaries, pre-finality and within-`k` finalized
rollback paths, configured-source inconsistency, external-provider
disagreement, missing DA, withholding, stale manifest, and the adapter rewind
rehearsal against recorded live chain data. Every case requires:

- zero duplicate submissions, lost evidence, false verified states, and
  unrecoverable workflows;
- fail-closed behavior and no manual repair; and
- watcher readiness and verification resumption only after reconciliation.

Do not manufacture a natural Preprod rollback. The local W44 matrix plus the
recorded-live-data adapter rewind is the required rollback evidence. A
naturally observed rollback is bonus evidence only.

Write the aggregate only from confirmed workflow journal, provider, watcher,
chain-point, manifest, blueprint, catalogue, parameter, release-evidence, and
final-state observations. Preserve every underlying source separately and pass
those immutable sources to the finalizer for its independent derivation. The
aggregate parser requires exact keys and the canonical family/recovery order,
but structural validity alone never satisfies a gate. Do not run the finalizer
until state queue depth, unfinished mutation jobs, and pending finalizations
are zero and watcher verification has resumed.

Load every independent source explicitly. Empty lists are a hard failure:

```bash
mapfile -t STATE_CORRECTION_WORKFLOW_JOURNALS < "$STATE_CORRECTION_WORKFLOW_JOURNAL_LIST"
mapfile -t STATE_CORRECTION_L1_OBSERVATIONS < "$STATE_CORRECTION_L1_OBSERVATION_LIST"
mapfile -t STATE_CORRECTION_RECOVERY_OBSERVATIONS < "$STATE_CORRECTION_RECOVERY_OBSERVATION_LIST"

[ "${#STATE_CORRECTION_WORKFLOW_JOURNALS[@]}" -gt 0 ]
[ "${#STATE_CORRECTION_L1_OBSERVATIONS[@]}" -gt 0 ]
[ "${#STATE_CORRECTION_RECOVERY_OBSERVATIONS[@]}" -gt 0 ]

STATE_CORRECTION_WORKFLOW_ARGS=()
for path in "${STATE_CORRECTION_WORKFLOW_JOURNALS[@]}"; do
  STATE_CORRECTION_WORKFLOW_ARGS+=(--state-correction-workflow-journal "$path")
done
STATE_CORRECTION_L1_ARGS=()
for path in "${STATE_CORRECTION_L1_OBSERVATIONS[@]}"; do
  STATE_CORRECTION_L1_ARGS+=(--state-correction-l1-observation "$path")
done
STATE_CORRECTION_RECOVERY_ARGS=()
for path in "${STATE_CORRECTION_RECOVERY_OBSERVATIONS[@]}"; do
  STATE_CORRECTION_RECOVERY_ARGS+=(--state-correction-recovery-observation "$path")
done
```

## Final evidence

### Extract transaction hashes

The helper prefers structured transaction observations and falls back to a
named JSON field:

```bash
step_tx_hash() {
  node --input-type=module - "$1" "$2" <<'NODE'
import { readFileSync } from "node:fs";
const [summaryPath, selector] = process.argv.slice(2);
const pattern = /^[0-9a-f]{64}$/i;
const summary = JSON.parse(readFileSync(summaryPath, "utf8"));
const observations = Array.isArray(summary.txObservations)
  ? summary.txObservations
  : [];
const observation = observations.find((entry) =>
  typeof entry?.txHash === "string"
  && pattern.test(entry.txHash)
  && (entry.field === selector || entry.field?.endsWith(`.${selector}`)),
);
if (observation) {
  process.stdout.write(observation.txHash.toLowerCase());
  process.exit(0);
}
const find = (value) => {
  if (Array.isArray(value)) {
    for (const entry of value) {
      const found = find(entry);
      if (found) return found;
    }
  } else if (value && typeof value === "object") {
    if (typeof value[selector] === "string" && pattern.test(value[selector])) {
      return value[selector];
    }
    for (const entry of Object.values(value)) {
      const found = find(entry);
      if (found) return found;
    }
  }
};
const hash = find(summary.parsedJson);
if (hash) process.stdout.write(hash.toLowerCase());
NODE
}

HUB_ORACLE_NONCE_TX_HASH="$(step_tx_hash "$HUB_ORACLE_NONCE_STEP" txHash)"
INIT_TX_HASH="$(step_tx_hash "$INIT_STEP" txHash)"
OPERATOR_REGISTRATION_TX_HASH="$(step_tx_hash "$OPERATOR_STEP" registerTxHash)"
OPERATOR_ACTIVATION_TX_HASH="$(step_tx_hash "$OPERATOR_STEP" activateTxHash)"
DEPOSIT_TX_HASH="$(step_tx_hash "$DEPOSIT_STEP" txHash)"
TX_A="$(step_tx_hash "$L2_TRANSFER_A_STEP" txId)"
TX_B="$(step_tx_hash "$L2_TRANSFER_B_STEP" txId)"
```

After automatic merge, obtain the two fresh header-commit transaction hashes
from the fresh database. Fail if the run does not have exactly two distinct,
confirmed header commits; do not invent labels or use unrelated hashes.

```bash
mapfile -t HEADER_COMMIT_TX_HASHES < <(
  $COMPOSE exec -T postgres \
    psql -U postgres -d midgard -At \
    -c "select encode(submitted_tx_hash, 'hex') from pending_block_finalizations where status = 'finalized' and submitted_tx_hash is not null order by created_at"
)
if [ "${#HEADER_COMMIT_TX_HASHES[@]}" -ne 2 ] \
  || [ "${HEADER_COMMIT_TX_HASHES[0]}" = "${HEADER_COMMIT_TX_HASHES[1]}" ]; then
  echo "expected exactly two distinct finalized header commits" >&2
  exit 1
fi
HEADER_COMMIT_A_TX_HASH="${HEADER_COMMIT_TX_HASHES[0]}"
HEADER_COMMIT_B_TX_HASH="${HEADER_COMMIT_TX_HASHES[1]}"
```

### Generate the dashboard

Include every required step summary, including the DA bind/listen preflight.

```bash
NODE_LOG="logs/$RUN_ID/midgard-node.log"
$COMPOSE logs --no-color --since=60m midgard-node > "$NODE_LOG"
rg -i \
  "error|failed|failure|unknownOutput|crashed|abandon|ScriptIntegrityHashMismatch|hash mismatch" \
  "$NODE_LOG" || true

STEP_SUMMARY_ARGS=()
for step in \
  "$HUB_ORACLE_NONCE_STEP" \
  "$REFERENCE_STEP" \
  "$INIT_STEP" \
  "$OPERATOR_STEP" \
  "$DA_WALLET_PREFLIGHT_STEP" \
  "$DA_LIBP2P_PREFLIGHT_STEP" \
  "$READY_STEP" \
  "$DEPOSIT_STEP" \
  "$PROJECT_DEPOSITS_STEP" \
  "$L2_TRANSFER_A_STEP" \
  "$L2_TRANSFER_B_STEP" \
  "$AUTOMATIC_MERGE_STEP"; do
  if [ -f "$step" ]; then
    STEP_SUMMARY_ARGS+=(--step-summary "$step")
  fi
done

TX_ARGS=()
append_tx_arg() {
  [ -z "$2" ] || TX_ARGS+=(--tx "$1:$2:$3:$4")
}
require_committed_l2_tx() {
  local body
  body="$(curl -sf "http://127.0.0.1:3000/tx-status?tx_hash=$1")"
  node --input-type=module - "$body" <<'NODE'
const body = JSON.parse(process.argv[2]);
process.exit(body.status === "committed" ? 0 : 1);
NODE
}
require_committed_l2_tx "$TX_A"
require_committed_l2_tx "$TX_B"
append_tx_arg hub-oracle-nonce "$HUB_ORACLE_NONCE_TX_HASH" confirmed "$HUB_ORACLE_NONCE_STEP"
append_tx_arg init "$INIT_TX_HASH" confirmed "$INIT_STEP"
append_tx_arg operator-registration "$OPERATOR_REGISTRATION_TX_HASH" confirmed "$OPERATOR_STEP"
append_tx_arg operator-activation "$OPERATOR_ACTIVATION_TX_HASH" confirmed "$OPERATOR_STEP"
append_tx_arg deposit "$DEPOSIT_TX_HASH" confirmed "$DEPOSIT_STEP"
append_tx_arg l2-transfer-a "$TX_A" committed "tx-status:$TX_A"
append_tx_arg l2-transfer-b "$TX_B" committed "tx-status:$TX_B"
append_tx_arg header-commit-a "$HEADER_COMMIT_A_TX_HASH" confirmed "postgres:pending_block_finalizations"
append_tx_arg header-commit-b "$HEADER_COMMIT_B_TX_HASH" confirmed "postgres:pending_block_finalizations"

SUMMARY_MODE="${SUMMARY_MODE:-fresh}"
case "$SUMMARY_MODE" in
  fresh|attach|resume) ;;
  *) echo "invalid summary mode: $SUMMARY_MODE" >&2; exit 1 ;;
esac

POSTGRES_HOST=127.0.0.1 \
POSTGRES_PORT=5433 \
ADMIN_API_KEY="${ADMIN_API_KEY:-localdev-admin}" \
node dist/index.js e2e-finalize-summary \
  --mode "$SUMMARY_MODE" \
  --run-id "$RUN_ID" \
  --out-dir "logs/$RUN_ID" \
  --node-log "$NODE_LOG" \
  --state-correction-evidence "$STATE_CORRECTION_EVIDENCE" \
  --state-correction-deployment-manifest "$STATE_CORRECTION_MANIFEST" \
  --state-correction-blueprint "$STATE_CORRECTION_BLUEPRINT" \
  --state-correction-catalogue "$STATE_CORRECTION_CATALOGUE" \
  --state-correction-parameters "$STATE_CORRECTION_PARAMETERS" \
  --state-correction-release-evidence "$STATE_CORRECTION_RELEASE_EVIDENCE" \
  --state-correction-final-snapshot "$STATE_CORRECTION_FINAL_SNAPSHOT" \
  "${STATE_CORRECTION_WORKFLOW_ARGS[@]}" \
  "${STATE_CORRECTION_L1_ARGS[@]}" \
  "${STATE_CORRECTION_RECOVERY_ARGS[@]}" \
  "${STEP_SUMMARY_ARGS[@]}" \
  "${TX_ARGS[@]}"
```

The command constructs its non-artifact authority from `L1_PROVIDER=Kupmios`,
the configured loopback `L1_KUPO_KEY` and `L1_OGMIOS_KEY`, and the live node
database. It forbids provider failover, re-reads every transaction from Kupo and
its canonical Ogmios block, rejects a rollback before the captured tip, and
re-reads the final queue and retained proof tokens. A remote endpoint, a missing
local source, or a callback that merely rereads the evidence directory is a hard
failure.

For opt-in stress, follow `benchmark.md` and append the verified
`--stress-summary` artifact.

### Audit the result

Inspect `logs/$RUN_ID/summary.json` and `summary.md`, not only the CLI exit code.
Confirm every acceptance condition in `SKILL.md`. Also record:

- run mode and reason;
- manifest ID/hash, one-shot, reference-script policy/count;
- all step/log paths and transaction hashes;
- watcher manifest/store/PID/log paths and per-header status;
- endpoint health/readiness and L2 balances;
- scheduler, finalization, DA and automatic merge evidence; and
- any recovered attempt and why clean-run status differs from functional status.

Do not call the run complete while any transaction or required evidence item is
missing or ambiguous.
