# Phase 4 isolated process devnet

This stack is exclusively for the destructive pipelined-commit process gate.
Every run gets a unique Compose project, database, filesystem tree, Cardano
chain, Kupo index, and deployment manifest. It refuses known public network
magic values and does not read the repository `.env`.

## Generate without starting services

Create two private dotenv files first. `wallets.env` must define six distinct
funded roles: `L1_OPERATOR_SEED_PHRASE`,
`L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`,
`L1_REFERENCE_SCRIPT_SEED_PHRASE`, `USER_SEED_PHRASE`, and the A/B
`TESTNET_GENESIS_WALLET_SEED_PHRASE_*` values. `node.env` contains the remaining
Midgard configuration but must not select a public network or remote provider.
The unrelated supplemental wallet C is not used or funded by this gate; the
bootstrap and immutable acceptance environment set C=A only when C is absent
so NodeConfig is complete without reading the checkout `.env`.

```bash
export MIDGARD_PHASE4_RUN_DIR=/tmp/midgard-phase4-process-example
export MIDGARD_PHASE4_RUN_ID=example
export MIDGARD_PHASE4_WALLET_ENV_FILE=/absolute/private/wallets.env
devnet/phase4-process/scripts/generate.sh
cp /absolute/private/node.env "$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
chmod 600 "$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
```

Bootstrap deterministically overwrites run-scoped Postgres fields from `run.env` before starting services, collapsing duplicate keys so stale generic values in a copied `node.env` cannot cross runs.
It also pins `MIN_FEE_A=0` and `MIN_FEE_B=0` in both the private node inputs
and immutable acceptance environment. This makes the fixed 50,000-lovelace A/B
funding proof exact; the genesis mutation command rejects any fee or isolated
Postgres-port drift before opening SQL.

Generation uses `cardano-cli latest genesis create-testnet-data` with exactly
one pool and renders Compose configuration, but starts no service. Defaults are
Ogmios `127.0.0.1:2337`, Kupo `127.0.0.1:2442`, and Postgres
`127.0.0.1:5544`. Projects and databases begin with
`midgard_phase4_process_`; signing keys, wallet env files, and run credentials
are mode 0600.

Because Phase 4 is a preprod-matched experiment, generation pins the Shelley
genesis protocol major to 11, matching the target preprod ledger used by the
formal run. This matters beyond enabling Plutus V3: the current Aiken bundle
uses builtins such as `CountSetBits` that major 9 rejects as malformed.
`validate-custom-chain-config.sh` fails closed unless the genesis protocol
major matches exactly and the node config forces Conway at epoch zero. Treat a
target-network protocol upgrade as an explicit fixture update, and never reuse
a run generated under a different major.

Matched snapshots also pin the consensus parameters that bound how long a
frozen chain can be restarted. Shelley computes the stability/forecast window
as `ceil(3k/f)` slots, where `k` is `securityParam` and `f` is
`activeSlotsCoeff`. This devnet uses `k=90000`, `f=1`, and one-second slots, so
the window is `ceil(3*90000/1) * 1s = 270000s = 75h`. The generated
`epochLength=900000` retains the conventional `10k/f` relationship. The
75-hour horizon gives a snapshot reset started up to 72 hours after capture a
three-hour safety margin for the node's approximately one-minute forge startup
and provider catch-up. Snapshots older than 72 hours are outside the supported
reuse contract and should be recaptured rather than relied upon.

Cardano-node 11.0.1, Ogmios v7.0.0, Kupo v2.11.0, and Postgres 15.15 are all
fixed to official immutable image digests. The generated run records the
effective image IDs; reset refuses any image or artifact drift.

The workspace uses Lucid Evolution 0.6, whose provider package natively
supports Ogmios v7's canonical `maxReferenceScriptsSizePerTransaction`
protocol-parameter field. No local provider patch is applied. The provider
normalizes the legacy v6 `maxReferenceScriptsSize` spelling for compatibility
and fails closed when both spellings conflict or the value is missing or
malformed. Verify the released behavior and fail-closed fixtures with:

```bash
pnpm exec vitest run tests/kupmios-ogmios-v7-protocol-parameters.test.ts
```

## Bootstrap and capture one matched snapshot

Only after generation and review:

```bash
export MIDGARD_PHASE4_RUN_DIR=/tmp/midgard-phase4-process-example
devnet/phase4-process/scripts/bootstrap.sh
devnet/phase4-process/scripts/capture-snapshot.sh
```

Bootstrap funds the six test-only wallets from the genesis UTxO, publishes the
node-runtime reference scripts, initializes the protocol, and registers the
operator. It explicitly seeds the complete configured L2 genesis set into the
otherwise-empty run-scoped `mempool_ledger`, byte-matching commit fallback, and
separately proves the A/B gate wallets are funded. It does not enable startup
genesis or submit an L1 deposit. The seed command refuses public networks,
non-loopback services, missing authorization, and partial/non-genesis ledger
state. It also
writes a mode-0600 `secrets/acceptance.env` by combining the
run's node and wallet inputs; this immutable child-env source is distinct from
the repository `.env`, and all Phase 4 node commands disable checkout dotenv
loading. Snapshot capture records a canonical Cardano tip hash
and slot plus the equal Kupo checkpoint before freezing Cardano, Kupo, and
Postgres. Every reset must restore that exact identity. Never mix an archive,
manifest, acceptance env, or snapshot identity from a different run.
The identity binds the source and distribution trees of both the operator
package (`midgard-node`, the node under test) and this tooling package
(`midgard-node-tools`, whose binary runs the acceptance controller, the gated
genesis-ledger seed, and the T1 probe/advance commands).
The snapshot set includes every archive, the optional deployment run-state
when present, the exact `aiken build --env testnet` blueprint checksum, and a
pinned `cardano-cli stake-address-info` proof of the PHAS registration and its
canonical unsigned registration transaction. The mode-0600 transaction-body
envelope is hashed into the deployment manifest and snapshot identity; pinned
`cardano-cli` and the offline verifier both require its transaction ID and its
single stake-registration certificate to name the deployed PHAS script
credential. Signed transaction CBOR and wallet secrets are never retained as
evidence.

## Acceptance reset command

```bash
export MIDGARD_PHASE4_RUN_DIR=/tmp/midgard-phase4-process-example
export MIDGARD_PHASE4_MATCHED_RESET_COMMAND="$PWD/devnet/phase4-process/scripts/reset.sh"
export MIDGARD_PHASE4_T1_RECOVERY_COMMAND="$PWD/devnet/phase4-process/scripts/t1-recover.sh"
export MIDGARD_PHASE4_PROCESS_ENV_FILE="$MIDGARD_PHASE4_RUN_DIR/secrets/acceptance.env"
export MIDGARD_PHASE4_PROCESS_DEPLOYMENT_MANIFEST_PATH="$MIDGARD_PHASE4_RUN_DIR/deploymentInfo/contract-deployment-info.json"
export MIDGARD_PHASE4_PROCESS_TARGET=local-devnet
export MIDGARD_PHASE4_PROCESS_ACCEPTANCE=pipelined-commit-live-v1
export MIDGARD_PHASE4_PROCESS_RUN_DIR="$MIDGARD_PHASE4_RUN_DIR/acceptance"
export MIDGARD_DOTENV_MODE=disabled
pnpm run accept:phase4:pipelined-process
```

The acceptance command supplies `MIDGARD_PHASE4_SCENARIO_LABEL`. Each reset
verifies the snapshot set and canonical identity, restores all durable
participants, restarts the isolated infrastructure, and prints the
`midgard-phase4-local-devnet-reset-attestation-v1` JSON identity. The process
harness receives that frozen identity only after the recreated producer forges
strictly beyond the frozen slot and Kupo indexes strictly beyond it. Because
producer recreation replaces the Cardano socket inode, reset explicitly
restarts Ogmios and then Kupo before checking that progress; an aged snapshot
that cannot resume therefore exits without publishing or emitting a successful
attestation. The process harness uses run-token-bound `/proc` ownership records
for detached groups; it never kills an unvalidated PID. A null or drifted Kupo
checkpoint is rejected rather than guessed or repaired by mutation. PHAS reset
preflight is read-only, runs at the frozen observer tip, and must reproduce the
byte-identical snapshot-bound registration proof before producer resume.
Reset performs every source, distribution, image, configuration, snapshot,
PHAS proof, and transaction-body drift check before stopping services or
writing any durable run tree, so rejected evidence leaves the active run
untouched.

The reviewed `t1-recover.sh` is the only accepted T1 command. The harness first
submits L2 header N and builds a speculative successor while holding the
confirmation fiber, then stops that attempt. The recovery script verifies the
complete matched-snapshot/image/artifact identity, proves N is canonical, and
restores only Cardano and Kupo. Postgres remains running and its complete
pending-finalization journal tables are dumped before and after and compared
byte-for-byte.

After rollback, the script proves N is absent and the canonical L2 tip is N's
original base B. It then uses the gated node command to submit an authenticated
no-op header F through the production commit builder, including its real
scheduler alignment and timing checks. F must link to B, start at B's end time,
advance to at least N's end-time bound, preserve the UTxO root, and commit empty
event/transaction roots and zero counts. L2 header hashes are 28 bytes (56 hex);
Cardano transaction, block, snapshot, and journal SHA-256 hashes are 32 bytes
(64 hex). The two domains are validated separately.

The script emits one snapshot-bound JSON attestation only after Cardano and
Kupo report the exact same checkpoint and F is provider-visible. On restart,
the harness scopes evidence to the new process attempt, requires stale-N
recovery before any match, and proves replacement N' bases on F, retains the
abandoned N transaction IDs and canonical CBOR, leaves the later retained
payload byte-identical, submits, and continues through the speculative
candidate path. A candidate line from an older append-log attempt cannot
satisfy the gate.

Run static tests with:

```bash
node --test devnet/phase4-process/tests/assets.test.mjs
```
