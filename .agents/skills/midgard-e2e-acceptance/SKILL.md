---
name: midgard-e2e-acceptance
description: Use when running or diagnosing the Midgard demo node end-to-end acceptance check after code changes, including attach/resume, interrupted fresh deployments, explicit clean redeploys, DA payload/attestation gates, and final health, DB, balance, and log verification.
---

# Midgard E2E Acceptance

Use this skill from the Midgard repository root when asked to verify that
Midgard changes have not broken the real demo-node path.

## Start Here: Choose The Run Mode

Choose a mode before any state-changing command:

1. **Attach/resume existing deployment** when a complete deployment already
   exists and the goal is to run, diagnose, or continue the node. Do not run
   `init`. Verify manifest identity, `HUB_ORACLE_ONE_SHOT_*`, reference-script
   auth policy, provider route, operator status, DB route, `deployment-status`,
   `/healthz`, and `/readyz`, then start `listen` or Docker.
2. **Continue interrupted pre-init deployment** when a fresh deployment was
   intentionally started and failed before `init`. Preserve logs, manifest
   hash, reference-script policy id, and submitted tx hashes. Resume only within
   the same documented identity rules; do not mix an old manifest with a new
   one-shot.
3. **Diagnose post-init ambiguity** when `init` or any later state-changing
   step may have succeeded. Stop and reconcile chain/DB/run-state first.
   Fresh redeploy is an explicit operator decision, not the default response.
4. **Fresh deployment** only when starting a new acceptance identity on purpose:
   fresh funded one-shot, fresh reference-script publication, fresh `init`, and
   local durable state reset that matches the new on-chain identity.
5. **Fresh redeploy recovery** only when state-reset rules require it. If local
   durable state was wiped, do not attach value-submitting flows to an old
   on-chain deployment. Restore matching local state or perform the full
   explicit redeploy.

Provider, wallet, DA, projection, scheduler timing, merge-lease, and evidence
failures are gate failures to fix or wait through. They are not automatic
redeploy triggers.

## Hard Rules

- Read root `AGENTS.md` first and follow the production-grade L2 directive.
- Work in `demo/midgard-node` for the operational run.
- Start with the mode decision above and record the selected mode plus reason
  in the final evidence.
- `listen` and Docker startup are attach operations. `init` is a bootstrap
  operation.
- Do not fresh redeploy on every failure. Use reconciliation commands,
  deployment-status, DB evidence, DA status, and run logs to choose the safe
  next action.
- Never wipe local durable node state unless the run also performs a full fresh on-chain deployment.
- Never combine clean local DBs with an old on-chain deployment manifest or old hub-oracle one-shot outref.
- Use a fresh operator-wallet UTxO for `HUB_ORACLE_ONE_SHOT_TX_HASH` and `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`; do not reuse the current `.env` value.
- Patch the fresh hub-oracle one-shot in `.env` before publishing reference
  scripts. The reference-script manifest and protocol init must be built from
  the same one-shot identity.
- Publish node-runtime reference scripts through `e2e-run-step` wrapping
  `node dist/index.js deploy-reference-script-node-runtime` before `init`.
  `init` now expects the deployment-info manifest to already contain the
  reference-script auth policy and reference-script UTxOs.
- Keep `RUN_GENESIS_ON_STARTUP=false` and run explicit protocol init through
  `e2e-run-step` wrapping
  `node dist/index.js init --contract-deployment-info-output deploymentInfo/contract-deployment-info.json`.
- Do not delete or reset `demo/midgard-node/cardano/db` or `demo/midgard-node/cardano/kupo` for this check. The clean slate is the Midgard node DB, Postgres volume, MPT/local DB files, and deployment manifest.
- Never rely on the CLI's default `USER_WALLET` seed source. Local `.env`
  files may only define `USER_SEED_PHRASE`; pass
  `--wallet-seed-phrase-env USER_SEED_PHRASE` on user-wallet commands.
- DB-backed CLI commands must either run inside the `midgard-node` container or
  be prefixed on the host with `POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433`.
  Host commands fail with the container-only hostname `POSTGRES_HOST=postgres`.
- `/tx-status` takes `tx_hash`, not `txId`.
- Acceptance uses local Docker Kupmios only. `L1_PROVIDER` must be `Kupmios`,
  Kupo/Ogmios must be local endpoints for the execution mode, and
  `L1_PROVIDER_FAILOVER` must be empty or absent. If local Kupmios is unhealthy,
  fix local Kupmios; do not switch acceptance to Blockfrost, Koios, or any
  remote provider.
- Run the demo DA committee node from `demo/da-committee-node` for DA
  attestations. Do not
  use `node dist/index.js attest-state-queue-once` as the acceptance path; if
  the DA node cannot sign, submit, and apply the attestation, diagnose and fix
  that path.
- Do not use manual SQL rewrites, local-only finalization, or transaction completion with local UPLC evaluation disabled to make the check pass.

## Pre-Acceptance Local Feedback Gate

Before live e2e for transaction-preparation changes, run the relevant lower
layers from `demo/midgard-node/docs/TX_PREP_FEEDBACK_LADDER.md` and record the
commands in the live evidence:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo"
pnpm run test:tx-prep:sdk
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
```

If live e2e exposes a deterministic builder, wallet/input-selection, validity,
DA payload, scheduler, or post-submit recovery bug, stop repeated live reruns.
Add a targeted local or emulator regression first, fix it there, then return to
live acceptance.

## Attach/Resume Existing Deployment

Use this mode when contracts are already deployed or a previous run was
interrupted after `init`.

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo/midgard-node"
pnpm build
node dist/index.js deployment-status --json
node dist/index.js reference-script-wallet-status --json
node dist/index.js l1-provider-preflight --json
node dist/index.js reconcile phas-registered --json
node dist/index.js reconcile reference-scripts-complete --scope node-runtime --json
```

Verify:

- manifest path, manifest SHA-256, reference-script auth policy id, and
  node-runtime ref-script completeness;
- one-shot outref in `.env` matches the deployment identity;
- provider route is local Kupmios and failover is empty;
- DB-backed host commands use `POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433`, or
  run inside the `midgard-node` container;
- wallet roles are distinct: operator, merge, reference-script, user, and DA
  submitter addresses or address hashes only, never seed phrases;
- `RUN_GENESIS_ON_STARTUP=false`; and
- startup uses `listen`/Docker, not `init`.

For value-submitting attach flows, start the node, verify `/healthz` and
`/readyz`, then use the deposit/L2/DA/merge gates below. If a milestone is
ambiguous, use the `reconcile ... --json [--repair]` commands instead of
blindly resubmitting.

## Fresh Run Procedure

Start from a rebuilt node image:

```bash
REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT/demo/midgard-node"
pnpm build
COMPOSE="docker compose -f docker-compose.yaml -f docker-compose.kupmios.yaml"
$COMPOSE build midgard-node midgard-node-migrate
RUN_ID="${RUN_ID:-e2e-run-$(date -u +%Y%m%dT%H%M%SZ)}"
E2E_STEP_DIR="logs/$RUN_ID/steps"
mkdir -p "$E2E_STEP_DIR"
```

Use the local Cardano/Ogmios/Kupo compose override for acceptance:

```bash
export CARDANO_NODE_IMAGE_TAG=<current-preprod-compatible-v11+-tag>
COMPOSE="docker compose -f docker-compose.yaml -f docker-compose.kupmios.yaml"
node dist/index.js l1-provider-preflight --json
```

Create a fresh marked hub-oracle one-shot outref through the structured runner:

```bash
HUB_ORACLE_NONCE_LOG="logs/e2e-hub-oracle-nonce-$(date -u +%Y%m%dT%H%M%SZ).log"
HUB_ORACLE_NONCE_STEP="$E2E_STEP_DIR/hub-oracle-nonce.json"
RUN_STATE_PATH="logs/$RUN_ID/deployment-run-state.json"
node dist/index.js e2e-run-step \
  --id hub-oracle-nonce \
  --cwd "$(pwd)" \
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

Patch `.env` with the returned `txHash` and `outputIndex`, and preserve the
same `$RUN_STATE_PATH` for reference-script publication. If nonce preparation
fails due to funding, list operator wallet UTxOs and choose a fresh, funded
outref that is not the current `.env` one-shot:

```bash
node --input-type=module -e 'import "dotenv/config"; import { Kupmios, Lucid, walletFromSeed } from "@lucid-evolution/lucid"; const network=process.env.NETWORK || "Preprod"; const kupo=process.env.L1_KUPO_KEY || `http://127.0.0.1:${process.env.KUPO_PORT || "1442"}`; const ogmios=process.env.L1_OGMIOS_KEY || `http://127.0.0.1:${process.env.OGMIOS_PORT || "1337"}`; const addr=walletFromSeed(process.env.L1_OPERATOR_SEED_PHRASE, { network }).address; const lucid=await Lucid(new Kupmios(kupo, ogmios), network); const utxos=await lucid.utxosAt(addr); console.log(JSON.stringify({addr, provider:"kupmios", utxos: utxos.map(u=>({txHash:u.txHash, outputIndex:u.outputIndex, lovelace:u.assets.lovelace?.toString()??"0", assets:Object.fromEntries(Object.entries(u.assets).map(([k,v])=>[k,v.toString()]))}))}, null, 2));'
```

Patch `.env` with `apply_patch` so `RUN_GENESIS_ON_STARTUP=false`, the fresh
one-shot outref is set, and the intended provider is explicit. If no fresh
operator UTxO exists, stop, fund the operator wallet, wait for confirmation,
and relist UTxOs.

Reset only Midgard local state and the Postgres volume after the fresh outref is selected:

```bash
$COMPOSE down -v --remove-orphans
mkdir -p db deploymentInfo logs
docker run --rm -v "$PWD/deploymentInfo:/mnt/deploymentInfo" -v "$PWD/db:/mnt/db" busybox sh -lc 'rm -rf /mnt/db/* /mnt/deploymentInfo/contract-deployment-info.json && chown -R 1000:1000 /mnt/db /mnt/deploymentInfo'
```

Publish the node-runtime reference scripts before `init` and save the full log:

```bash
REFERENCE_LOG="logs/e2e-reference-scripts-$(date -u +%Y%m%dT%H%M%SZ).log"
REFERENCE_STEP="$E2E_STEP_DIR/reference-scripts.json"
node dist/index.js e2e-run-step \
  --id reference-scripts \
  --cwd "$(pwd)" \
  --raw-log "$REFERENCE_LOG" \
  --summary-out "$REFERENCE_STEP" \
  --timeout-ms 10800000 \
  -- \
  node dist/index.js deploy-reference-script-node-runtime \
  --run-state "$RUN_STATE_PATH" \
  --contract-deployment-info-output deploymentInfo/contract-deployment-info.json \
```

Expect this step to take a long time on preprod. Batch-split warnings such as
`exceeded max tx size; retrying in smaller batches` and funding escalation
warnings such as `retrying with N funding input(s)` are progress, not failure.
Keep the command running while it continues to submit and confirm batches; note
the highest funding-input count in the final report.

If reference-script deployment is interrupted before a complete manifest is
written, do not guess at reuse. Preserve the old log and inspect the run-state
and manifest identity first. A rerun may resume the same reference-script auth
policy only when the run-state or manifest matches the current network,
hub-oracle one-shot outref, manifest path, and policy id. If that comparable
identity is absent before `init`, start a new pre-init reference-script attempt
only with an explicit fresh-redeploy reason. After `init`, any mismatch between
manifest, reference scripts, and one-shot identity requires a full clean local
reset plus a new fresh on-chain deployment identity.

Verify that the node-runtime reference-script scope required by `init` is
complete before moving on:

```bash
node dist/index.js reconcile reference-scripts-complete --scope node-runtime --json
```

This fresh acceptance path publishes and verifies the node-runtime reference
scripts. The manifest may still contain `refScriptUTxO: null` for non-runtime
contract groups that are not part of this flow; do not fail the fresh run solely
because those optional groups are unpublished.

Deploy fresh on-chain protocol state and complete operator lifecycle:

```bash
INIT_LOG="logs/e2e-init-$(date -u +%Y%m%dT%H%M%SZ).log"
INIT_STEP="$E2E_STEP_DIR/init.json"
node dist/index.js e2e-run-step \
  --id init-protocol \
  --cwd "$(pwd)" \
  --raw-log "$INIT_LOG" \
  --summary-out "$INIT_STEP" \
  --timeout-ms 1200000 \
  -- \
  node dist/index.js init \
  --contract-deployment-info-output deploymentInfo/contract-deployment-info.json
OPERATOR_LOG="logs/e2e-operator-lifecycle-$(date -u +%Y%m%dT%H%M%SZ).log"
OPERATOR_STEP="$E2E_STEP_DIR/operator-lifecycle.json"
node dist/index.js e2e-run-step \
  --id operator-lifecycle \
  --cwd "$(pwd)" \
  --raw-log "$OPERATOR_LOG" \
  --summary-out "$OPERATOR_STEP" \
  --timeout-ms 1200000 \
  -- \
  node dist/index.js register-active-operator
$COMPOSE up -d midgard-node
```

If `register-active-operator` is interrupted after registration but before
activation, do not use deregistration or test-only recovery. The combined
`register-active-operator` command now resumes safely when chain state shows
exactly one registered node for the operator and no active node: it skips
registration and proceeds directly to activation. For an explicit activate-only
recovery, use the supported CLI:

```bash
node dist/index.js activate-operator
```

If activation fails, stop and diagnose. Do not force progress with
deregistration, SQL edits, test-only commands, or disabled local evaluation.

Wait for the API to be ready:

```bash
READY_LOG="logs/e2e-midgard-node-ready-$(date -u +%Y%m%dT%H%M%SZ).log"
READY_STEP="$E2E_STEP_DIR/midgard-node-ready.json"
node dist/index.js e2e-run-step \
  --id midgard-node-ready \
  --cwd "$(pwd)" \
  --raw-log "$READY_LOG" \
  --summary-out "$READY_STEP" \
  --timeout-ms 180000 \
  -- \
  node --input-type=module -e 'const deadline=Date.now()+170000; while (Date.now()<deadline) { const r=await fetch("http://127.0.0.1:3000/readyz").catch(()=>null); if (r?.ok) { console.log(JSON.stringify(await r.json())); process.exit(0); } await new Promise((resolve)=>setTimeout(resolve,5000)); } process.exit(1);'
```

Start the copied DA committee node before submitting L2 activity. The default
acceptance path is the demo node's legacy 1-of-1 DA committee derived from
`L1_OPERATOR_SEED_PHRASE`; if `.env` configures a multi-member committee, start
enough DA node instances to meet `DA_THRESHOLD` instead of falling back to the
manual `attest-state-queue-once` CLI. `DA_PAYLOAD_ENDPOINTS` entries are base
URLs such as `http://127.0.0.1:3000`; do not append `/da/payload`.

```bash
REPO_ROOT="${REPO_ROOT:-$(git rev-parse --show-toplevel)}"
DA_NODE_DIR="$REPO_ROOT/demo/da-committee-node"
CONTRACT_INFO="$PWD/deploymentInfo/contract-deployment-info.json"
DA_L1_SUBMITTER_KEY_SOURCE="$PWD/deploymentInfo/preprod-da-2of3/secrets/l1-submitter.seed"
WATCHER_MANIFEST="$DA_NODE_DIR/run/e2e-watcher-manifest.json"
WATCHER_DB="$DA_NODE_DIR/run/e2e-watcher-store.json"
mkdir -p "$DA_NODE_DIR/run" "$DA_NODE_DIR/db" logs
pnpm --dir "$DA_NODE_DIR" install --frozen-lockfile
pnpm --dir "$DA_NODE_DIR" build

set -a
. ./.env
set +a
export CONTRACT_INFO WATCHER_MANIFEST
if [ -z "${DA_COMMITTEE_HEX:-}" ]; then
  unset DA_COMMITTEE_HEX
fi
if [ -z "${DA_THRESHOLD:-}" ]; then
  DA_THRESHOLD=1
  export DA_THRESHOLD
fi
if [ ! -f "$DA_L1_SUBMITTER_KEY_SOURCE" ]; then
  echo "Missing DA L1 submitter key source: $DA_L1_SUBMITTER_KEY_SOURCE" >&2
  exit 1
fi
node --input-type=module <<'NODE'
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

const contractInfoPath = process.env.CONTRACT_INFO;
const manifestPath = process.env.WATCHER_MANIFEST;
if (!contractInfoPath || !manifestPath) {
  throw new Error("CONTRACT_INFO and WATCHER_MANIFEST are required");
}
const network = process.env.NETWORK || "Preprod";
const seed = process.env.L1_OPERATOR_SEED_PHRASE;
if (!seed) {
  throw new Error("L1_OPERATOR_SEED_PHRASE is required for demo DA signing");
}
const contractInfo = readFileSync(contractInfoPath);
const fingerprint = createHash("sha256").update(contractInfo).digest("hex");
const wallet = walletFromSeed(seed, { network });
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const vkey = Buffer.from(privateKey.to_public().to_raw_bytes()).toString("hex");
const manifest = {
  schemaVersion: "midgard-da-deployment-v1",
  deploymentFingerprint: fingerprint,
  network,
  contractDeploymentInfoSha256: fingerprint,
  da: {
    threshold: Number(process.env.DA_THRESHOLD || 1),
    members: [
      {
        index: 0,
        vkey,
        baseUrls: ["http://127.0.0.1:8787"],
        canSubmitL1: true
      }
    ]
  }
};
mkdirSync(dirname(manifestPath), { recursive: true });
writeFileSync(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`);
console.log(JSON.stringify({ manifestPath, fingerprint, signerIndex: 0, vkey }, null, 2));
NODE

if [ "${L1_PROVIDER:-Kupmios}" != "Kupmios" ]; then
  echo "Acceptance requires L1_PROVIDER=Kupmios. Fix local Kupmios instead of switching providers." >&2
  exit 1
fi
CARDANO_PROVIDER_URLS="kupmios:http://127.0.0.1:${KUPO_PORT:-1442}|http://127.0.0.1:${OGMIOS_PORT:-1337}"

DA_NODE_LOG="logs/e2e-da-committee-node-$(date -u +%Y%m%dT%H%M%SZ).log"
env \
  MIDGARD_NETWORK="${NETWORK:-Preprod}" \
  MIDGARD_DEPLOYMENT_MANIFEST_PATH="$WATCHER_MANIFEST" \
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH="$CONTRACT_INFO" \
  CARDANO_PROVIDER_URLS="$CARDANO_PROVIDER_URLS" \
  CARDANO_FINALITY_DEPTH="${CARDANO_FINALITY_DEPTH:-2}" \
  DA_PAYLOAD_ENDPOINTS="http://127.0.0.1:3000" \
  DA_SIGNER_INDEX=0 \
  DA_SIGNER_KEY_SOURCE="cardano-seed:${L1_OPERATOR_SEED_PHRASE}" \
  DA_THRESHOLD="${DA_THRESHOLD:-1}" \
  DA_L1_SUBMISSION_ENABLED=true \
  L1_SUBMITTER_KEY_SOURCE="file:$DA_L1_SUBMITTER_KEY_SOURCE" \
  WATCHER_DB_PATH="$WATCHER_DB" \
  WATCHER_API_HOST=127.0.0.1 \
  WATCHER_API_PORT=8787 \
  WATCHER_POLL_INTERVAL_MS=15000 \
  node dist/index.js e2e-start-service \
    --service da-committee-node \
    --cwd "$DA_NODE_DIR" \
    --raw-log "$DA_NODE_LOG" \
    --pid-file "$DA_NODE_DIR/run/e2e-watcher.pid" \
    --ready-url http://127.0.0.1:8787/readyz \
    --health-url http://127.0.0.1:8787/healthz \
    --ready-timeout-ms 180000 \
    --poll-interval-ms 5000 \
    pnpm start
```

Derive L2 addresses and submit a deposit large enough to fund the following L2 transfers:

```bash
USER_L2_ADDRESS=$(node --input-type=module -e 'import "dotenv/config"; import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.USER_SEED_PHRASE,{network:process.env.NETWORK}).address)')
DEST_A=$(node --input-type=module -e 'import "dotenv/config"; import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.TESTNET_GENESIS_WALLET_SEED_PHRASE_A,{network:process.env.NETWORK}).address)')
DEST_B=$(node --input-type=module -e 'import "dotenv/config"; import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.TESTNET_GENESIS_WALLET_SEED_PHRASE_B,{network:process.env.NETWORK}).address)')
DEPOSIT_LOG="logs/e2e-submit-deposit-$(date -u +%Y%m%dT%H%M%SZ).log"
DEPOSIT_STEP="$E2E_STEP_DIR/submit-deposit.json"
node dist/index.js e2e-run-step \
  --id submit-deposit \
  --cwd "$(pwd)" \
  --raw-log "$DEPOSIT_LOG" \
  --summary-out "$DEPOSIT_STEP" \
  --timeout-ms 1200000 \
  -- \
  env POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433 \
  node dist/index.js submit-deposit \
  --wallet-seed-phrase-env USER_SEED_PHRASE \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000
```

Wait for the deposit to be seen and projected. Prefer running DB-backed CLI
commands inside the container:

```bash
PROJECT_DEPOSITS_LOG="logs/e2e-project-deposits-$(date -u +%Y%m%dT%H%M%SZ).log"
PROJECT_DEPOSITS_STEP="$E2E_STEP_DIR/project-deposits.json"
node dist/index.js e2e-run-step \
  --id project-deposits \
  --cwd "$(pwd)" \
  --raw-log "$PROJECT_DEPOSITS_LOG" \
  --summary-out "$PROJECT_DEPOSITS_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js project-deposits-once"
curl -s "http://127.0.0.1:3000/utxos?address=$USER_L2_ADDRESS"
```

The host equivalent is:

```bash
PROJECT_DEPOSITS_LOG="logs/e2e-project-deposits-$(date -u +%Y%m%dT%H%M%SZ).log"
PROJECT_DEPOSITS_STEP="$E2E_STEP_DIR/project-deposits.json"
node dist/index.js e2e-run-step \
  --id project-deposits \
  --cwd "$(pwd)" \
  --raw-log "$PROJECT_DEPOSITS_LOG" \
  --summary-out "$PROJECT_DEPOSITS_STEP" \
  --timeout-ms 300000 \
  -- \
  env POSTGRES_HOST=127.0.0.1 POSTGRES_PORT=5433 \
  node dist/index.js project-deposits-once
```

If `/utxos` is still empty, check whether the deposit was only reconciled from
L1 but is not due for projection yet:

```bash
$COMPOSE exec -T postgres psql -U postgres -d midgard -c "select now() as db_now, min(inclusion_time) filter (where status = 'awaiting') as next_due_projection, count(*) filter (where status = 'awaiting') as awaiting, count(*) filter (where status = 'projected') as projected, count(*) filter (where status = 'consumed') as consumed from deposits_utxos;"
```

If `next_due_projection` is in the future, wait until that time plus a small
buffer and rerun `project-deposits-once`. Do not count pre-inclusion-time
non-projection as a failure.

Only after the projected user L2 UTxO is visible and the DA committee node is
still ready, submit L2 transfers that spend that projected value:

```bash
curl -sf http://127.0.0.1:8787/readyz
L2_TRANSFER_A_LOG="logs/e2e-submit-l2-transfer-a-$(date -u +%Y%m%dT%H%M%SZ).log"
L2_TRANSFER_A_STEP="$E2E_STEP_DIR/submit-l2-transfer-a.json"
node dist/index.js e2e-run-step \
  --id submit-l2-transfer-a \
  --cwd "$(pwd)" \
  --raw-log "$L2_TRANSFER_A_LOG" \
  --summary-out "$L2_TRANSFER_A_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js submit-l2-transfer --wallet-seed-phrase-env USER_SEED_PHRASE --endpoint http://127.0.0.1:3000 --l2-address $DEST_A --lovelace 2000000"
L2_TRANSFER_B_LOG="logs/e2e-submit-l2-transfer-b-$(date -u +%Y%m%dT%H%M%SZ).log"
L2_TRANSFER_B_STEP="$E2E_STEP_DIR/submit-l2-transfer-b.json"
node dist/index.js e2e-run-step \
  --id submit-l2-transfer-b \
  --cwd "$(pwd)" \
  --raw-log "$L2_TRANSFER_B_LOG" \
  --summary-out "$L2_TRANSFER_B_STEP" \
  --timeout-ms 300000 \
  -- \
  bash -lc "$COMPOSE exec -T midgard-node node dist/index.js submit-l2-transfer --wallet-seed-phrase-env USER_SEED_PHRASE --endpoint http://127.0.0.1:3000 --l2-address $DEST_B --lovelace 1500000"
```

Poll each returned L2 transaction hash until `/tx-status` reports `committed`.
Then wait for the DA committee node to sign, submit, and apply the DA
attestation. The Midgard node merge path should show the queued header's
`da_attestation` as the deployment's DA attestation policy before merge:

```bash
ADMIN_KEY="${ADMIN_API_KEY:-localdev-admin}"
curl -s "http://127.0.0.1:3000/tx-status?tx_hash=$TX_A"
curl -s "http://127.0.0.1:3000/tx-status?tx_hash=$TX_B"
curl -s -H "x-midgard-admin-key: $ADMIN_KEY" http://127.0.0.1:3000/stateQueue
HEADER_HASH="<queued-header-hash>"
DEPLOYMENT_FINGERPRINT=$(node --input-type=module -e 'import { createHash } from "node:crypto"; import { readFileSync } from "node:fs"; process.stdout.write(createHash("sha256").update(readFileSync("deploymentInfo/contract-deployment-info.json")).digest("hex"))')
curl -sf "http://127.0.0.1:3000/da/payload/metadata?header_hash=$HEADER_HASH"
curl -sf "http://127.0.0.1:3000/da/payload?header_hash=$HEADER_HASH" >/tmp/midgard-da-payload.cbor
curl -sf "http://127.0.0.1:8787/v1/deployments/$DEPLOYMENT_FINGERPRINT/headers/$HEADER_HASH/status"
tail -n 80 "$DA_NODE_LOG"
```

Do not run `attest-state-queue-once` if the header remains unattested. Inspect
`$DA_NODE_LOG`, the watcher store, `/da/payload?header_hash=...`, and the
watcher `/v1/deployments/<fingerprint>/headers/<headerHash>/status` endpoint
for the stuck header, then fix the DA node or configuration. Do not merge when
the watcher status reports `root_mismatch`, `malformed_da`, or `conflicted`;
that is a payload construction or endpoint-set blocker. Once payload metadata
and CBOR are available, watcher payload status is `verified`, and watcher header
status is `attested` or `merged`, force admin merges for any mature tail block
that remains because `MIN_QUEUE_LENGTH_FOR_MERGING=2`:

```bash
MERGE_LOG="logs/e2e-merge-tail-$(date -u +%Y%m%dT%H%M%SZ).log"
MERGE_STEP="$E2E_STEP_DIR/merge-tail.json"
node dist/index.js e2e-run-step \
  --id merge-tail \
  --cwd "$(pwd)" \
  --raw-log "$MERGE_LOG" \
  --summary-out "$MERGE_STEP" \
  --timeout-ms 300000 \
  -- \
  curl -sf -H "x-midgard-admin-key: $ADMIN_KEY" http://127.0.0.1:3000/merge
```

Wait and repeat `/merge` only as needed until `stateQueue.headers` is empty and the DB summary is clean.

When investigating `ScriptIntegrityHashMismatch` or other hash mismatch
failures, keep raw logs in a file and report a compact summary only: failing
command or endpoint, tx hash if known, expected hash, actual hash, reference
script policy/outref if shown, and the log filepath. Do not paste huge raw body
logs into the final report; use them as attached evidence for the code fix that
should make the node emit compact summaries with raw bodies available
separately.

## Interrupted Run Triage

For any interrupted step, preserve raw logs first. Then record:

- run mode and reason;
- last submitted tx hashes and whether they are confirmed, unknown, or rejected;
- manifest hash, reference-script auth policy id, and ref-script count;
- one-shot outref, provider route, DB route, and wallet role addresses;
- DA node PID/log path, watcher manifest path, watcher DB path, and per-header
  watcher status when a header exists; and
- classification: safe to retry before submit, reconcile submitted tx before
  rerun, wait for provider/projection visibility, attach/resume, or explicit
  fresh redeploy required by state-reset rules.

Use reconciliation commands before repeating submitted milestones:

```bash
node dist/index.js reconcile phas-registered --json --repair
node dist/index.js reconcile reference-scripts-complete --scope node-runtime --json --repair
node dist/index.js reconcile deposit-projected --event-id <event-id> --json --repair
node dist/index.js reconcile tx-committed --tx-hash <l2-tx-hash> --json
node dist/index.js reconcile da-attested --header-hash <header-hash> --watcher-url http://127.0.0.1:8787 --deployment-fingerprint "$DEPLOYMENT_FINGERPRINT" --json --repair
node dist/index.js reconcile block-committed --header-hash <header-hash> --json
node dist/index.js reconcile merge-complete --header-hash <header-hash> --json --repair
```

## Failure Routing

| Blocker                                                             | Safe next action                                                                                                                         |
| ------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| Local Kupmios unhealthy or failover configured                      | Run `l1-provider-preflight --json`; fix Kupo/Ogmios/Cardano-node locally. Do not switch acceptance to a remote provider.                 |
| Fresh one-shot mismatch                                             | Stop. Use attach/resume if protocol already initialized; otherwise restart fresh deployment with a new one-shot and matching manifest.   |
| Incomplete reference-script manifest before `init`                  | Resume/retry reference-script publication under documented identity rules; preserve old logs.                                            |
| Manifest/reference-script mismatch after `init`                     | Stop and diagnose identity drift. Fresh redeploy only with state-reset rule.                                                             |
| Operator registered but not active                                  | Reconcile operator lifecycle; use documented activate-only recovery only when chain state proves registration already exists.            |
| Deposit not projected                                               | Check `deposits_utxos.inclusion_time`; wait until due, then run projection once.                                                         |
| Submitted deposit confirmation timeout                              | Run `reconcile-deposit-submission --tx-hash <hash> --json`; do not blindly resubmit on `ambiguous`.                                      |
| DA node not ready before L2 activity                                | Stop before L2 transfer/commit. Fix DA wallet/config/provider and restart the DA node.                                                   |
| DA payload missing for committed header                             | Inspect `da_payloads`, pending finalization, local logs, and run `db:backfill-da-payloads --header-hash <hash>` only as repair evidence. |
| DA payload conflict or root mismatch                                | Stop. Do not merge. Diagnose payload construction and endpoint set.                                                                      |
| DA signatures below threshold                                       | Start enough DA signer instances or fix peer propagation. Do not lower threshold for acceptance.                                         |
| Merge skipped by state-queue lease                                  | Inspect `/stateQueueMutationLease` and unfinished local mutation jobs; wait or repair the real stuck job.                                |
| Scheduler/commit timing expired                                     | Rebuild through the worker path with fresh timing; do not reuse expired signed txs or pending rows.                                      |
| Partial deployment or local state wiped under old on-chain identity | Follow `docs/agents/state-reset.md`; fresh redeploy only with clean local state and fresh on-chain identity.                             |

## Stale-Instruction Audit

Before claiming the runbook is current, audit active acceptance docs:

```bash
rg -n "attest-state-queue-once|txId|USER_WALLET|RUN_GENESIS_ON_STARTUP=true|docker compose down -v|pnpm init|/da/payload" .agents/skills/midgard-e2e-acceptance demo/midgard-node/docs docs/agents
```

Every hit must either be a correct current command, a base URL warning, or an
explicit "do not use this as the acceptance path" note.

## Required Final Evidence

Collect and report:

- Run mode (`fresh`, `attach`, `resume`, or `fresh-redeploy`) and reason.
- Lower-layer transaction-preparation commands run before live e2e, when code
  changes touched builders, transaction prep, workers, DA, or recovery.
- Fresh hub-oracle one-shot outref used in `.env`.
- Reference-script deployment duration, log filepath, auth policy id, reference-script count, any batch splits, and highest funding-input count observed.
- Init, operator registration, operator activation, deposit, L2 transfer, commit, scheduler refresh, and merge transaction hashes.
- Whether operator lifecycle used the normal CLI or the test-exposed activate-only recovery path.
- DA committee node log filepath, PID file, generated watcher manifest path,
  watcher DB path, payload endpoint base URL, `/healthz` and `/readyz` status,
  payload metadata/CBOR evidence for every committed header, watcher
  per-header status, and DA attestation `init`, `add_signatures`, and `apply`
  transaction hashes from the watcher store/logs.
- `/healthz` is healthy and `/readyz` is ready with no readiness reasons.
- Both L2 transaction statuses are `committed`.
- User and destination L2 balances reflect the deposit-funded transfers and expected change.
- Scheduler advanced: logs show scheduler witness refresh or ActiveOperator scheduling for the commit window.
- State commitments were submitted and finalized; state queue was merged until empty.
- No unfinished local mutation jobs, no mempool residue, no processed mempool residue, and no unexpected errors in logs.
- For any hash mismatch, a compact summary plus the raw log filepath.

Generate the required dashboard first; use the raw probes after it only for
extra context or failure diagnosis:

```bash
NODE_LOG="logs/e2e-midgard-node-$(date -u +%Y%m%dT%H%M%SZ).log"
$COMPOSE logs --no-color --since=30m midgard-node > "$NODE_LOG"
rg -i "error|failed|failure|unknownOutput|crashed|abandon|ScriptIntegrityHashMismatch|hash mismatch" "$NODE_LOG" || true
step_tx_hash() {
  node --input-type=module - "$1" "${2:-0}" <<'NODE'
import { readFileSync } from "node:fs";
const summary = JSON.parse(readFileSync(process.argv[2], "utf8"));
const index = Number(process.argv[3] ?? 0);
const hash = Array.isArray(summary.observedTxHashes)
  ? summary.observedTxHashes[index]
  : undefined;
if (typeof hash === "string") {
  process.stdout.write(hash);
}
NODE
}
HUB_ORACLE_NONCE_TX_HASH="$(step_tx_hash "${HUB_ORACLE_NONCE_STEP:-}" 0)"
INIT_TX_HASH="$(step_tx_hash "$INIT_STEP" 0)"
OPERATOR_REGISTRATION_TX_HASH="$(step_tx_hash "$OPERATOR_STEP" 0)"
OPERATOR_ACTIVATION_TX_HASH="$(step_tx_hash "$OPERATOR_STEP" 1)"
DEPOSIT_TX_HASH="$(step_tx_hash "$DEPOSIT_STEP" 0)"
TX_A="$(step_tx_hash "$L2_TRANSFER_A_STEP" 0)"
TX_B="$(step_tx_hash "$L2_TRANSFER_B_STEP" 0)"
STEP_SUMMARY_ARGS=()
for step in \
  "$HUB_ORACLE_NONCE_STEP" \
  "$REFERENCE_STEP" \
  "$INIT_STEP" \
  "$OPERATOR_STEP" \
  "$READY_STEP" \
  "$DEPOSIT_STEP" \
  "$PROJECT_DEPOSITS_STEP" \
  "$L2_TRANSFER_A_STEP" \
  "$L2_TRANSFER_B_STEP" \
  "$MERGE_STEP"; do
  if [ -f "$step" ]; then
    STEP_SUMMARY_ARGS+=(--step-summary "$step")
  fi
done
TX_ARGS=()
append_tx_arg() {
  if [ -n "$2" ]; then
    TX_ARGS+=(--tx "$1:$2:$3:$4")
  fi
}
append_tx_arg hub-oracle-nonce "$HUB_ORACLE_NONCE_TX_HASH" confirmed "$HUB_ORACLE_NONCE_STEP"
append_tx_arg init "$INIT_TX_HASH" confirmed "$INIT_STEP"
append_tx_arg operator-registration "$OPERATOR_REGISTRATION_TX_HASH" confirmed "$OPERATOR_STEP"
append_tx_arg operator-activation "$OPERATOR_ACTIVATION_TX_HASH" confirmed "$OPERATOR_STEP"
append_tx_arg deposit "$DEPOSIT_TX_HASH" confirmed "$DEPOSIT_STEP"
append_tx_arg l2-transfer-a "$TX_A" committed "$L2_TRANSFER_A_STEP"
append_tx_arg l2-transfer-b "$TX_B" committed "$L2_TRANSFER_B_STEP"
POSTGRES_HOST=127.0.0.1 \
POSTGRES_PORT=5433 \
ADMIN_API_KEY="${ADMIN_API_KEY:-localdev-admin}" \
node dist/index.js e2e-finalize-summary \
  --mode fresh \
  --run-id "$RUN_ID" \
  --out-dir "logs/$RUN_ID" \
  --node-log "$NODE_LOG" \
  "${STEP_SUMMARY_ARGS[@]}" \
  "${TX_ARGS[@]}"

# Supplemental raw probes when the dashboard is not success:
curl -s http://127.0.0.1:3000/healthz
curl -s http://127.0.0.1:3000/readyz
curl -s http://127.0.0.1:8787/healthz
curl -s http://127.0.0.1:8787/readyz
curl -s -H "x-midgard-admin-key: ${ADMIN_API_KEY:-localdev-admin}" http://127.0.0.1:3000/stateQueue
tail -n 120 "$DA_NODE_LOG"
$COMPOSE logs --no-color --since=30m midgard-node | rg -i "scheduler|Refreshing scheduler|Scheduler refresh transaction submitted|ActiveOperator" || true
$COMPOSE exec -T postgres psql -U postgres -d midgard -c "select 'pending_finalizations' as tbl, status::text as status, count(*) from pending_block_finalizations group by status union all select 'tx_admissions' as tbl, status::text as status, count(*) from tx_admissions group by status union all select 'deposits' as tbl, status::text as status, count(*) from deposits_utxos group by status order by tbl, status; select 'mempool' as tbl, count(*) from mempool union all select 'processed_mempool' as tbl, count(*) from processed_mempool union all select 'blocks' as tbl, count(*) from blocks union all select 'immutable' as tbl, count(*) from immutable union all select 'confirmed_ledger' as tbl, count(*) from confirmed_ledger union all select 'local_mutation_jobs_unfinished' as tbl, count(*) from local_mutation_jobs where status <> 'completed';"
```

Acceptance passes only when:

- There is one consumed deposit and two accepted L2 admissions.
- `pending_block_finalizations` rows are finalized.
- `mempool`, `processed_mempool`, `blocks`, and unfinished local mutation jobs are zero after merges.
- `immutable` and `confirmed_ledger` contain the merged results.
- `stateQueue.headers` is empty after the final merge.
- The final log scan has no unexplained error, failed, abandoned, or crash entries.
- `logs/$RUN_ID/summary.json` and `logs/$RUN_ID/summary.md` exist and report
  `verdict: success` with `nextSafeAction: none_run_complete`.

If any gate fails, classify it with the failure-routing table and choose the
safe next action. Fresh redeploy is required only when the local/on-chain
identity is unsafe under `docs/agents/state-reset.md`; otherwise attach,
reconcile, wait, or fix the specific gate and continue with recorded evidence.
