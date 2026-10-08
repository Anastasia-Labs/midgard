# Midgard Watcher

Status: Active

Last reviewed: 2026-10-02 (unattended lifecycle operating guidance; final
release acceptance remains outstanding).

`midgard-watcher` is the independent verifier and challenger. The DA committee
service is a separate package, `demo/da-committee-node`.

## Runtime and commands

From `demo/midgard-watcher`, run `pnpm run build` before invoking the CLI, and
build the node transport sidecar with
`pnpm --dir ../l1-node-transport run native:build`. Configure
`l1NodeTransportBinaryPath` to the resulting
`../l1-node-transport/dist/native/midgard-l1-node-transport` binary:

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

Use `GET /readyz` for readiness: HTTP 200 carries `ready: true`; HTTP 503
carries `ready: false` and current `reasons`. `GET /v1/status` remains the
detailed runtime status and can return 200 while the watcher is held. The
operations endpoint is loopback and currently has no application bearer gate;
keep it internal. An alive process or authority identity endpoint does not
establish watcher readiness. A held runtime must remain visible for diagnosis
without admitting proof or availability work. Readiness must come from a live
runtime snapshot, never a persisted supervisor status file.

Two surfaces report readiness and they prove different things. The
fault-proof application's startup readiness binds the verified deployment and
resolves every installed family's published reference scripts through the
registry roster; it reads no secret, acquires no lease and opens no retained-DA
transport, so it is safe to run repeatedly and proves only that the deployment
is bound and the scripts exist. Operations status (`readiness`,
`readinessReasons`) is the live signal: it reports the supervisor phase,
recovery, launch scope, proof deadlines, L1 source freshness, active alerts
(except the informational DA bond pool alerts below),
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

The fault decision, proof queue and proof objective journals are tables in
`watcher-journals.sqlite` inside the workflow journal directory. Each record is
one row, authenticated with the rollback key, and each commit is one chained
revision, so a persist writes only the rows it changes. Startup verifies every
row and the latest 64 revisions, and refuses a tampered, deleted, replayed or
reordered record; so does any later read of a row that fails its MAC, does not
parse, or differs from its key. A refusal holds for the rest of the process:
the watcher stays up, `/readyz` reports `journal_integrity`, and `/v1/status`
names the failure in `supervisor.journalIntegrity` until an operator repairs
the journals (below) and restarts the watcher. If the journals cannot be opened at
all (a busy, locked or unreadable file), the watcher stays up, `/readyz`
reports `journal_unavailable` and `supervisor.journalUnavailable` names the
failure, while the watcher retries the open, backing off from 1 s to 30 s; the
reason clears without a restart once an open succeeds. If a write to an open
watcher SQLite file meets another connection's lock past the busy timeout
(SQLITE_BUSY or SQLITE_LOCKED), the write commits nothing and the watcher
stays up: `/readyz` reports `journal_busy` and `supervisor.journalBusy` names
the failure. After the same 1 s to 30 s backoff the watcher drops its
in-memory proof work, rebuilds it from the journals as a restart would, and
lets the next decision pass dispatch it again; the reason clears then. A journal directory
that cannot be used, or a rollback key the journals were not written under, is
a configuration error: the watcher exits before the operations server binds.
An objective whose completion was verified deeper than rollback recovery
reaches is skipped at the next start and pruned with its workflow journal. Only open objectives count toward the cap of 2,048; at the
cap the watcher stays up and `/readyz` reports `journal_capacity` until
objectives complete.

To repair refused journals: stop the watcher, move `watcher-journals.sqlite`
and its `-wal` and `-shm` files aside (never delete them), and start it again.
The new journals start empty. The workflow journals, the funding store and
the L1 follower's facts survive, so proofs and funding reservations may name
fault decisions the new journals do not hold. The watcher never runs such work
again and never exits over it: it holds each item, `/readyz` reports
`journal_decision_missing`, and `supervisor.journalDecisionMissing` in
`/v1/status` names each held objective or reservation with its decision.

- A held proof objective clears once its header leaves the finalized state
  queue: its proof, or another one, landed and is final, or the header merged.
  A fresh detection of the same fault does not restart it.
- A held funding reservation is read again on every follower change. If every
  input is spent at or below the release-final point (`automaticRecoveryMaxDepth
  - 2` blocks deep), it is dropped. If its inputs are unspent there and still
    unspent at the tip, and it holds no signed transaction, its inputs return to
    the wallet. Otherwise it stays held.

Retention handles the rest. Workflow directories, follower pins of objectives
that are never admitted again, and the leases of reservations that hold a
signed transaction stay in place. The moved-aside file is never read again.

Every watcher SQLite file, the journals included, must sit on a local disk,
never a network filesystem (NFS, SMB and the like): those break SQLite's file
locking and its write-ahead log.

The file journals of earlier development formats (`fault-decisions/`,
`fault-proof-queue-v1/` and `fault-proof-completions-v1/` in the workflow
journal directory) are ignored, not refused: nothing imports them, the
journals start fresh, and readiness never waits on them. At start the watcher
logs one `legacy_journal_ignored` warning naming each non-empty one. Ignoring
them neither deletes them nor authorizes discarding an existing deployment's
durable state. Follow [state reset rules](../../docs/agents/state-reset.md). Evidence remains pinned by event, recovery, and
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
the deployed challenger bond, fees and separate plain-ADA collateral. Open spends
one exact coin of challenger bond plus challenge-record lovelace plus the Open fee
ceiling; that coin's out-ref derives the challenge identity, and the actor
prepares it when necessary. A Timeout pays the committee's slash penalty out of
the pooled DA bond as part of its fee, so collateral must cover the protocol
collateral percentage of penalty plus the Timeout fee ceiling, in at most three
plain-ADA coins. Every process using this payment key must share the same
journal. Before every Open, and before preparing its coin, the watcher requires
the bond plus one queue-bounded removal reserve and one Timeout collateral set,
held once per wallet however many challenges are live: removals are serialized
by the correction lock, and each Timeout removes its head and prunes every
descendant. It rechecks the complete live queue before timeout takes the
correction lock.
The timeout references that queue's tail, so a concurrent append invalidates the
quoted removal budget.

Public retrieval failure for an attested header schedules an availability
challenge before fault classification. The commitment an Open needs is read from
the DA attestation that the header's Apply consumed and must hash to the queue
node's `commitment_hash`. An Open is only attempted before the header's Open
deadline (`end_time` plus the challenge window); a withheld header past it
merges unchallenged and is reported under `missedOpenDeadlines`. Each header's
next step is selected independently and due Opens run first, earliest deadline
first, including a withheld descendant of a header already Challenged. The
availability journal keeps one workflow per header, so a live challenge never
holds back another header's Open in the same deployment; it refuses every step
for a different deployment while any challenge is live. An Open the wallet cannot
fund is skipped and reported under `openRefused` (header, required and available
lovelace), and the watcher takes the next step in the same reconciliation, so a
live challenge's settle, close or Timeout is never starved. A Timeout reads the
pool at the source tip; when no authentic pool is found there it is skipped for
that reconciliation and reported under `timeoutsDeferred`, and the next step
runs. A step the journal refuses is reported under `workflowRefused` with the
journal's reason, which names the deployment and header of the live workflow
that blocks it. A terminal step landed by someone else (the committee's Close,
another watcher's Timeout, a prune or removal of the header) releases this
actor's workflow for that header, in any deployment, once a finalized, verified
transaction burns the header's queue node or closes its challenge; the release
is reported under `workflowReleased`, and a failed release check keeps the row
and is reported under `workflowReleaseDeferred`. Until that transaction reaches
`automaticRecoveryMaxDepth` (2160) the release is checked again on every
reconciliation; if it is no longer canonical the row is live again and the
change is reported under `workflowReleaseDeferred`. Our own Timeout keeps the
wallet reserved while descendants remain to be pruned. The actor settles answered or expired tranches, closes complete responses, and
prunes descendants before removing an unavailable head. None of `openRefused`,
`timeoutsDeferred`, `workflowRefused`, `workflowReleased` or
`workflowReleaseDeferred` blocks actuation or readiness. Pending availability is not a healthy or faulty classification. After
close, canonical L1 publication history supplies the committed envelope if the
original peers still withhold it. Startup reconciles signed intents before new
actions; rollback revokes actuation immediately and re-derives from the fork
point. A confirmed intent the new chain contradicts is rewound and its identical
signed bytes rebroadcast; no replacement is signed while it can still land.

A withheld header can be timed out only once it is the queue head. Its Timeout
slashes the DA bond pool once, removes that head and prunes every descendant
without a further slash, so the committee's liability is one DA bond per
withholding episode, not one per withheld block. A descendant the watcher had
also challenged is pruned with its challenge, which strands that challenge's
record and challenger bond; the watcher still opens it, because if the
ancestor's challenge is answered instead, the descendant would otherwise merge
unchallenged. While the queue head is `Challenged`, the state queue refuses
every Append, as it does while the head is `Unattested` past its attestation
timeout, so block production stalls until the head's challenge is closed or
times out.

Timeout is permissionless, so several watchers can race the same header. When
another transaction consumes a pending intent's inputs first, reconciliation
expires the intent once its validity has passed, releases its reservations, and
the watcher moves on to its next step. It expires the intent either when one
normal input is spent while another is still unspent at the same point, or when
a normal input was consumed by another valid canonical transaction at
confirmation depth. That spend counts only after the consuming transaction is
read back and shown to be valid and to list the input. Missing inputs alone
never expire an intent.

The watcher also reads the pooled DA bond on every availability reconcile, in a
read of its own bound to the same finalized point as its availability
snapshots, whether or not a header is pending. `GET /v1/status` serves the latest readout as `daBondPool`:
`state` (`missing`, `bonded` or `withdrawing`), `lovelace` and `backing` (lovelace
above the pool floor), `requiredBacking` (`da_bond_lovelace`), `belowBond`,
`unlockAt` and `unlockable` while withdrawing, `alerts: {underBacked, withdrawing}`
and `observedAtMs`, with amounts as decimal lovelace strings; it is `null` before
the first read. Two alert codes follow it, with the deployment manifest id as
subject: `da_bond_pool_under_backed` fires when the backing is below one DA bond
(after a slash or a withdrawal; a missing pool counts) and clears after a top-up,
and `da_bond_pool_withdrawing` fires on BeginWithdraw and clears on cancel or
completion. Both appear in `activeAlerts` and count in `/v1/metrics`
`activeAlertCount`, but they are informational: they never add a readiness reason,
never make the watcher not ready, and never stop it from opening a challenge. An
alert diagnostic is recorded only when an alert changes. A failed pool read (a
transport error, or a pool output that fails authentication) is reported only:
it never changes the availability phase or the watcher's readiness, and it never
holds back a challenge action. `daBondPool` keeps the last good readout, whose
`observedAtMs` shows its age, and `GET /v1/status` serves the failure as
`daBondPoolReadFailure: {error, failedAtMs}` (the latest failure) until the next
good read sets it back to `null`. Funding, top-up and the owner-quorum withdrawal are operator commands; see
[DA bond pool commands](../midgard-node/docs/da-bond-commands.md).

The nested wire parser accepts both `local_node` and `external_providers`.
External-provider mode requires independent provider identities. The installed
CLI process parser is narrower: it requires `mode: "acceptance"`,
`targetNetwork` of `"Preprod"` or `"Custom"`, and `local_node` authority, with
confirmation depth and prefinality rollback depth equal to the compiled
deployment profile's `l1_finality.confirmation_depth` (10 for the live testing
profiles, 3 for `preprod-emulator-testing`, 30 for `mainnet` and
`preprod-public`), and postfinality recovery
bound 2160. `Custom` admits an explicitly bound isolated devnet, the network the
automatic watcher journeys run against; it is not a relaxation of finality or
rollback policy. The authority process enforces the same policy.

`$.l1.origin` is an optional operator override of the deployment's L1 origin,
`{"slot": <n>, "blockHash": "<64 lowercase hex>"}`: the point immediately
before the block holding the `prepareHubOracleNonce` tx, where the L1 follower
starts. `midgard-l1-follower find-origin` prints it (see
[the follower README](../midgard-l1-follower/README.md#origin)). Absent means
the deployment's own origin applies. It is operator configuration only: it is
not part of any profile or manifest and changes no deployment identity.

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
genesis identity; the node transport sidecar (`demo/l1-node-transport`) provides
ordered chain evidence. The process runs one sidecar on one node connection for
its chain-sync streams and exact-point queries; a sidecar crash, hang or
protocol violation fails every open stream and in-flight query, and the
transport restarts it.
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

Before starting the sidecar, explicitly provision its independently owned authority
volume with `initializeSelectedAuthorityStore` from `midgard-watcher`, supplying
the verified policy, authority record key, a persisted `generation-<UUID>` attempt
identity, and the chosen `liveRecordLimit`. For an existing legacy authority,
stop every old writer and prevent restart, then use `importLegacyAuthorityStore`
with a separate legacy archive directory. Both operations are offline ownership
contracts; their source recheck does not fence an old writer. Preserve the legacy
bytes and the initialization attempt identity. A torn, unparseable final legacy
record can be removed only with the explicit offline
`repairLegacyWatcherTrustedHeadAuthorityFinalRecord` helper: supply the exact
expected prior head, final filename/raw digest, stable repair UUID and reason,
and retain its separate evidence directory. It verifies the complete prior chain,
keeps original torn bytes plus authenticated intent/completion receipts, and resumes
only that exact repair after interruption. Parseable invalid records and interior
corruption remain held. Every old writer must stay quiescent with restart excluded;
this helper does not provide a live fleet fence. Ordinary `authority` startup only
opens the authenticated selected backend. Missing or corrupt state fails closed;
it never initializes, repairs, or falls back to the archive.

`liveRecordLimit` is mandatory, counts authority revisions, and must match the
initialized store. The example's explicit value `1` illustrates the smallest
supported geometry; it is not a production default or a block rollback horizon.
Choose a deployed value from measurements of the intended storage and workload.
The live SQLite store retains the exact current head, one authenticated boundary
checkpoint, and the latest `min(liveRecordLimit, revision + 1)` records. Current
reads verify that entire live suffix. Retired archive edits are detected by
`auditLegacyWatcherTrustedHeadAuthority`, not current reads. Keep the selected
authority volume, selector, SQLite DB/WAL/SHM, and record key outside watcher write
ownership. Whole-volume replay remains outside this independent freshness trust
assumption. Filesystem deletion or replacement concurrent with SQLite access is
outside the supported storage contract.

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

## Unattended lifecycle verification

Rebuild every changed package and the native binary before starting a journey;
record the source revision and artifact hashes. Preserve existing deployment
state on a provider outage. A retry may re-observe a fresh canonical source,
but it cannot substitute guessed history, release unresolved wallet inputs,
clear quarantine on a timer, or sign a replacement while old signed bytes can
still land. Inspect live readiness reasons and follow explicit recovery only
after the cause and retained residue have been verified.

The reliability program's final acceptance requires a fresh deployment,
automatic deposit/transfer/merge/withdrawal and exact payout, then separate
Cardano restart, Kupo stop, Ogmios stop, and public retained-DA loss drills.
The running services must recover without manual commit, merge, or database
repair. Focused tests and provider-only calibration do not close these gates.
The response-budget decision must include actual source/cursor cost, signed
attempt expiry and retry, and the full response workflow; a local timing
measurement does not establish a public-network deadline guarantee. See the
[release checklist](../../docs/public_testnet_readiness.md#release-verification).

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
  The tests drive a scripted fake node transport; the sidecar itself is tested
  in `demo/l1-node-transport`.
  The journey harness runs the built `dist`; rebuild before a live run or the
  child process executes stale code while the test process reads source.

The [verification-boundary decision](../../docs/midgard/decisions/watcher-verification-boundaries.md)
explains the trust and recovery model; the runtime and its tests determine
implemented behavior.
