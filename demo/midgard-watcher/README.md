# Midgard Watcher

Status: Active

Last reviewed: 2026-09-12 (forced-submission merge, installed scope, and live
acceptance status; not deployment acceptance).

`midgard-watcher` is the independent verifier and challenger. The DA committee
service is a separate package, `demo/da-committee-node`.

## Runtime and commands

From `demo/midgard-watcher`, run `pnpm run build` and `pnpm run native:build`
before invoking the CLI. Configure `nativeChainSyncBinaryPath` to the resulting
`dist/native/midgard-chain-sync` binary:

```sh
export MALLOC_MMAP_THRESHOLD_=131072
node dist/cli.js authority --config /absolute/path/authority.json
node dist/cli.js start --config /absolute/path/watcher-process.json
node dist/cli.js replay --config /absolute/path/watcher-process.json
```

On glibc/Linux hosts, set `MALLOC_MMAP_THRESHOLD_=131072` **before Node starts**.
The `start`, `replay`, and `test` package scripts set this default; direct Node
invocations, service managers, and `pnpm exec vitest` need the same environment.
A fixed 128 KiB mapping threshold lets glibc return freed large snapshot buffers
to the OS. Dynamic threshold growth can otherwise retain gigabytes of unused
native heap during catch-up and make native query process creation progressively
slower. This setting is ignored by allocators that do not implement it. See the
[GNU allocator documentation](https://sourceware.org/glibc/manual/latest/html_node/Memory-Allocation-Tunables.html)
and [Node.js memory guidance](https://nodejs.org/api/process.html#processmemoryusage).

`authority` runs the trusted-head authority until shutdown. `start` constructs
the watcher runtime and runs until SIGINT/SIGTERM. `replay` constructs the same
runtime, waits for durable catch-up, and closes it; it is not an offline or
transport-free command. Both may drive proof and availability workflows.

Startup requires admitted proof runners, recovered workflows, an accepting
supervisor, and safe proof deadlines before emitting `productionReady: true`.
That field describes this runtime's readiness checks, not public-testnet launch
approval. Invalid arguments exit 64; runtime failures fail closed with exit 70.

Two surfaces report readiness and they prove different things. The
fault-proof application's startup readiness binds the verified deployment and
resolves every installed family's published reference scripts through the
registry roster; it reads no secret, acquires no lease and opens no retained-DA
transport, so it is safe to run repeatedly and proves only that the deployment
is bound and the scripts exist. Operations status (`readiness`,
`readinessReasons`) is the live signal: it reports the supervisor phase,
recovery, launch scope, proof deadlines, L1 source freshness, active alerts,
and the shared retained-DA transport, which starts on the first fault
classification and is reported as `retained_da_transport_failed` if that start
fails. Starting builds the local libp2p node without contacting a peer, so a
failure is local; the classification that needed it fails closed, the process
exits 70, and a restart owns a fresh transport. There is no in-process retry.
Peers are dialed per retrieval request, and a retrieval failure for an attested
header is answered by an availability challenge, not by the transport status.
Dependency health belongs to the live surface, not to startup readiness.
See [launch readiness](../../docs/public_testnet_readiness.md) and the
[challenger runbook](../../docs/fault-proofs/challenger-runbook.md) for acceptance
and operating boundaries.

Non-tail removal, which peels the successor commitments stacked on a
fraudulent block, is coordinated locally: the workflow orchestrator retries a
lost peel against a fresh authenticated L1 view until the removal confirms. The
watcher never talks to a Midgard node and holds no node credentials.

Committed rollback snapshots bind completed validation to its schema, policy,
deployment, blueprint, and authentication key. Restart authenticates that saved
result and its independently protected head without replaying retained history.
Changed bindings or unusable persisted state fail closed for explicit recovery.
Forward progress validates new evidence and state transitions before committing.

SQLite progress markers reference shared records in the existing evidence archive.
Unchanged state, decoded events, and raw block bytes are stored once; derived
positional caches are rebuilt when reading cold state. The same transaction
writes newly referenced records and advances the marker before the independent
head is published. Ordinary event-history restart restores its authenticated
semantic validation and corroborates the exact current native head. Full replay
remains an explicit recovery operation.

The prelaunch database format is replaced in place: earlier development databases
must be redeployed, not migrated. Evidence remains pinned by event, recovery, and
proof dependencies; this change does not delete archives. See the
[persistence record inventory](../../docs/midgard/decisions/watcher-persistence.md)
for consumers and retention rules.

## Configuration and trust boundaries

CLI configuration uses `midgard-watcher-production-process-config-v1`; the
separate authority uses `midgard-watcher-trusted-head-authority-process-config-v1`.
The nested watcher configuration uses `midgard-watcher-config-v1`. A nested
configuration by itself is not a complete CLI process configuration.

The exact schemas and validation rules live in
[src/runtime/process-config.ts](src/runtime/process-config.ts) and
[src/runtime/config.ts](src/runtime/config.ts). Unknown/missing fields fail
closed. Process configuration binds deployment identity, durable storage,
trusted-head authority, transports, proof funding, and operational endpoints.
Keep secrets in the supported environment/file references.

### Local credential file

The CLI loads `./config.yaml` relative to its working directory before parsing
the process configuration. This optional file is a flat mapping of existing
environment variable names to nonempty strings. Quote every value, including
numbers and booleans; nested objects and YAML numeric/boolean values are not
accepted. Values already present in the process environment take precedence.
In the node CLI, the same YAML bootstrap runs before dotenv loading.

| Control                                          | Behavior                                                                                                                                       |
| ------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| No override                                      | Load `./config.yaml` when present; an absent default file is allowed.                                                                          |
| `MIDGARD_CONFIG_FILE=/absolute/path/config.yaml` | Load that file; a missing explicit file is fatal.                                                                                              |
| `MIDGARD_CONFIG_MODE=disabled`                   | Disable YAML loading, including an explicit file.                                                                                              |
| `MIDGARD_DOTENV_MODE=disabled`                   | Disable implicit default YAML loading for isolated harnesses; an explicit `MIDGARD_CONFIG_FILE` still applies unless YAML loading is disabled. |

Set these controls in the launching process environment. Loading happens only
at CLI bootstrap, not when importing the watcher library. YAML errors redact
file contents. Keep the credential file gitignored and readable only by its
owner (`chmod 600 config.yaml`); do not commit real credentials.

YAML supplies secret environment references; it does **not** replace the
required process JSON or its deployment, authority, storage, and funding
configuration. Continue to launch with
`node dist/cli.js start --config /absolute/path/watcher-process.json`.
To use YAML credentials, set `watcherConfig.proverWallet.keySource` to
`{ "kind": "environment", "variable": "WATCHER_PROVER_KEY" }` and
`availability.keySource` to
`{ "kind": "environment", "variable": "WATCHER_AVAILABILITY_KEY" }` in that
JSON, keeping the corresponding nested runtime configuration consistent.

Both variables contain a **raw mnemonic without a `seed:` prefix**, or an
`ed25519_sk...` Bech32 private key. The prover and availability actor require
independent payment keys. These are not DA committee `cardano-seed:` key-source
strings. A credential file neither generates a key nor recovers a missing
signer; required credentials and funding must already exist.

[watcher-process.example.json](watcher-process.example.json) is a complete
`start` configuration: configuration and bundles under `/etc/midgard`, state
under `/var/lib/midgard-watcher`, secrets as files under `/run/secrets`. Copy it
and replace every placeholder (genesis identity, DA peers, history providers)
before use; a unit test keeps it parseable.

Availability actuation requires an independent payment key and a durable journal:

```json
"availability": {
  "keySource": { "kind": "environment", "variable": "WATCHER_AVAILABILITY_KEY" },
  "journalPath": "/var/lib/midgard-watcher/availability.sqlite",
  "minimumFundingLovelace": "20000000000"
}
```

The funding amount is an operator-selected floor, not a protocol constant. Supply
the deployed challenger bond, fees and separate plain-ADA collateral. The actor
prepares the exact bond-plus-open-fee denomination when necessary. Every process
using this payment key must share the same journal. The watcher checks removal
capital while an unanswered challenge is live and rechecks the complete live
queue before timeout takes the correction lock. The timeout references that
queue's tail, so a concurrent append invalidates the quoted removal budget.

Public retrieval failure for an attested header schedules an availability
challenge before fault classification. The actor settles answered or expired
tranches, closes complete responses, and prunes descendants before removing an
unavailable head. Pending availability is not a healthy or faulty classification.
After close, canonical L1 publication history supplies the committed envelope if
the original peers still withhold it. Startup reconciles signed intents before
new actions; rollback revokes actuation immediately. A rollback through finalized
availability state halts the journal and requires authenticated recovery.

The nested wire parser accepts both `local_node` and `external_providers`.
External-provider mode requires independent provider identities. The installed
CLI process parser is narrower: it requires `mode: "acceptance"`,
`targetNetwork` of `"Preprod"` or `"Custom"`, and `local_node` authority, with
confirmation depth and prefinality rollback depth equal to the compiled
deployment profile's `l1_finality.confirmation_depth` (3 for the testing
profiles, 30 for `mainnet` and `preprod-public`), and postfinality recovery
bound 2160. `Custom` admits an explicitly bound isolated devnet, the network the
automatic watcher journeys run against; it is not a relaxation of finality or
rollback policy. The authority process enforces the same policy.

`$.l1.finality.depth` stays the manifest's release depth and governs anchoring:
finalized audit anchors, incident records and evidence stamps. Fault-proof
state-queue observation, classification dispatch and proof-step submission use
fixed authenticated inclusion depth 1. This is not a runtime configuration field
and does not change the release finality policy or manifest digest. On rollback, invalidate cached authority, re-observe canonical
state, reconcile outstanding submissions, then resume under fresh authority.
Reuse suitable signed bytes or rebuild when necessary. Terminal inclusion
releases the execution slot while the existing journal retains reconciliation
state until anchoring. Wallet selection excludes unresolved attempts; it does
not assume all old-fork inputs are available or hold all capital until finality.

Local authority binds the Cardano node socket, node/genesis configuration, and
genesis identity; the native chain-sync process provides ordered chain evidence.
Configured query services are subordinate to that authority. Authenticated
rollback recovery and the independent trusted head protect durable replay.
DA retrieval authenticates the configured peer and payload commitments.
Acceptance of a generic nested configuration does not establish support by the
installed process launcher.

Startup verifies the deployment manifest against the canonical V1 consensus
profile exactly, including `forcedTransactionSourceEncoding`
(`midgard-forced-submission-v1`, added by the forced-submission redesign). A
manifest published before that field existed fails closed with
`canonical_manifest_invalid`; the deployment must be republished, not patched.
Forced submissions are adjudicated per
[the forced-submission decision](../../docs/midgard/decisions/forced-inclusion-submission-verdict.md):
the L1 order authenticates the immutable submission and the operator's
`OperatorVerdictV1` is the sole committed classification.

A watcher that joins late, or restarts after its local state is lost, does not
replay history from genesis. It bootstraps from the `ConfirmedState` datum at
the root of the L1 state queue (confirmed header hash, UTxO root, end time), the
DA payload of that confirmed head, and the DA payloads of every header still
live in the L1 queue, then classifies from the confirmed state to the tip. The
committee and the node never prune those payloads: their retention is exactly
the L1 confirmed head, the headers live in the L1 queue, and payloads still
inside the challengeability horizon. Merged blocks older than the horizon can no
longer be challenged, so a late watcher needs nothing from before the confirmed
state, with one exception: the `missing-native-script-tx` family still builds
its historical native-script corpus by walking retained DA from genesis
(`resolveHistoricalNativeScriptCorpus` in
[historical-native-script-corpus.ts](../midgard-fault-proofs/src/workflow/historical-native-script-corpus.ts)),
so that family depends on payloads outside the retained set until its redesign
lands.

## Running with compose

[compose.yaml](compose.yaml) runs the two processes as two services from one
image: `watcher-authority` (`authority --config /etc/midgard/authority.json`)
and `watcher` (the image's default `start`). The split is a key-custody
boundary: the authority holds the record key that chains trusted-head records,
`start` holds the signing keys, and neither holds the other's. Both services
use `network_mode: host` because the authority endpoint, the operations
endpoint and every L1 query service must be loopback.

Prerequisites on the host:

- A Cardano L1 stack publishing Ogmios on `127.0.0.1:1337` and Kupo on
  `127.0.0.1:1442`, with its node socket directory (default
  `../midgard-node/cardano/ipc`, override with `MIDGARD_L1_IPC_DIR`) and a
  directory holding the node config and Shelley genesis
  (`MIDGARD_L1_CONFIG_DIR`), mounted at `/ipc` and `/cardano-config`.
- `config/watcher-process.json` from
  [watcher-process.example.json](watcher-process.example.json);
  `config/watcher-runtime.json` holding exactly its `watcherConfig` object
  (startup refuses any difference); `config/authority.json` from
  [authority.example.json](authority.example.json). The authority's `policy`
  must be the finality policy `start` derives from the same `watcherConfig`
  and the verified deployment identity; the template's policy is built from
  the start template with placeholder deployment hashes, and a unit test keeps
  the two templates consistent.
- `bundles/` holding the six release artifacts the process config names:
  deployment authority, rule bundle, funding profiles, deployment manifest,
  blueprint and contract deployment info.
- Two disjoint secret sets, as regular files (the loader refuses a symlinked
  secret, so compose `secrets:` are not used), without a trailing newline and
  pairwise distinct. The authority gets the record key and the bearer; `start`
  gets the rollback key, prover key, availability key and the same bearer
  value. Copy [.env.example](.env.example) to `.env` and point each variable at
  its file.

Then `docker compose up -d`. `watcher` starts only once `watcher-authority`
answers `/v1/identity`. Both restart `unless-stopped`: if the authority dies,
`start` fails closed with exit 70, compose restarts it, and it keeps exiting
70 at `/v1/identity` until the authority is healthy again, then re-reads the
trusted head and resumes. Exit 70 is the intended failure signal, so do not
cap restarts. The healthchecks treat a 401 as alive; they carry no bearer.

Known gap (belongs to the shared L1 stack work): on public networks the
Mithril-bootstrapped node image keeps its node config and genesis inside the
image, so nothing on the host produces the genesis file the watcher hashes
(`genesisIdentitySha256`). Until the L1 stack exports them, place them in
`MIDGARD_L1_CONFIG_DIR` yourself. The devnet journeys have the file only
because the harness generates the devnet genesis.

## Source and verification map

- [CLI](src/cli.ts) and [command lifecycle](src/runtime/scaffold.ts).
- [Runtime composition](src/runtime/watcher-runtime.ts).
- [Installed proof categories](../midgard-fault-proofs/src/workflow/family-application-registry.ts):
  the family application registry's keys are the installed set. The
  [watcher application](src/fault-proofs/fault-proof-application.ts) derives
  `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` from them in catalogue order and
  names no family itself; `WATCHER_MISSING_WORKFLOW_CATEGORIES` is the
  catalogue's complement of the registry, currently empty. Installation is
  source scope only, not live acceptance.
- [Catalogue status](../../docs/fault-proofs/catalogue-status.md): source
  inventory, deployment identity, and acceptance boundaries.
- [Automatic watcher journeys](../../docs/fault-proofs/automatic-watcher-journeys.md):
  the real-devnet acceptance harness in
  `demo/midgard-node-tools/devnet/watcher-journeys/`. It launches this package's
  built `dist/cli.js` against a fresh Cardano devnet and drives each
  non-interactive family from committed header to healthy successor. As of
  2026-09-13 ten families have completed that journey on the
  forced-submission-merged code against its 55-family deployment:
  `transitionTrace`, `zeroInput`, `invalidRange`, `invalidSignature`,
  `mintAuthorization`, `minFee`, `spendInputSignerMissing`,
  `protectedOutputSignerMissing`, `observersForbiddenOnUntaggedNetwork`, and
  `inputSetUniqueness`. Those retained passes used per-step finality waits;
  subsequent inclusion-policy passes require their own recorded provenance.
  The other non-interactive families are verified
  locally, in progress on that harness, or still blocked, not live-accepted.
  The interactive `validationTraceDispute` family is installed but outside
  that harness.
- `pnpm run typecheck`, `pnpm run lint`, and `pnpm test` check this package.
  The journey harness runs the built `dist`; rebuild before a live run or the
  child process executes stale code while the test process reads source.

The [verification-boundary decision](../../docs/midgard/decisions/watcher-verification-boundaries.md)
explains the trust and recovery model; the runtime and its tests determine
implemented behavior.
