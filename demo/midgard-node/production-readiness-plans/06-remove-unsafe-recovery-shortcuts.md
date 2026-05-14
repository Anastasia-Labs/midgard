# P0 Blocker 6: Remove Unsafe MPF Recovery Shortcuts

Status: refreshed on 2026-05-13 against the current `demo/midgard-node`
implementation. This is still an implementation plan, but several supporting
renames and safety fixes have landed. The historical file name still contains
`mpt`; current code and new work should use MPF terminology.

## Current Implementation Status

Landed:

- Runtime code uses `src/workers/utils/mpf.ts`, `MidgardMpf`, `makeMpfs`,
  `deleteMpfStore`, and MPF root-marker terminology.
- Config exposes `LEDGER_MPF_DB_PATH` and `TRANSACTIONS_MPF_DB_PATH`.
- Deprecated names `LEDGER_MPT_DB_PATH`, `MEMPOOL_MPT_DB_PATH`, and
  `MEMPOOL_MPF_DB_PATH` are rejected at startup rather than treated as aliases.
- MPF empty-root handling is normalized to the canonical Midgard empty root,
  `SDK.EMPTY_MERKLE_TREE_ROOT`, with unit tests.
- The old HTTP `/reset` route and `src/reset.ts` path are no longer present in
  the current command router.

Still live and unsafe:

- `makeMpfs` opens the persistent ledger MPF and, when `rootIsEmpty()` is true,
  inserts configured `GENESIS_UTXOS`.
- This empty-root branch is not gated by SQL state, L1 state, startup policy,
  durable metadata, or an operator recovery command.
- Startup does not yet run an authoritative local-state classifier before HTTP
  serving or mutating workers.
- `local_state_metadata`, `ledger_mpf_rebuilds`, adoption, rebuild, and MPF
  integrity readiness state are not implemented.

Production standard: an empty ledger MPF must never be interpreted as safe
genesis bootstrap unless every authoritative durable source proves this is a new
deployment and bootstrap was explicitly requested. Otherwise startup must fail
closed or rebuild from verified durable authority through an auditable recovery
path.

## Problem Statement

The block commitment worker opens a persistent ledger MPF and uses it as the
local UTxO root source for block construction. Today, when that root is empty,
the helper silently inserts configured genesis UTxOs. That collapses materially
different states into the same behavior:

- a legitimate first bootstrap of an empty non-mainnet node; and
- a corrupted or data-lost node whose PostgreSQL and/or L1 state shows prior
  history but whose LevelDB-backed ledger MPF is missing, reset, or empty.

For a production L2, this is a state-integrity blocker. A missing local root is
not proof that the ledger returned to genesis. It is evidence that the node has
lost or cannot read part of its durable state. Continuing from reseeded genesis
can create roots that disagree with confirmed SQL state, queued block state,
deposit projection state, pending-finalization state, and the canonical state
queue on L1.

The fix must remove implicit recovery behavior from normal MPF construction. The
node must classify startup as a proven bootstrap, proven healthy restart,
explicit verified rebuild, or integrity failure that refuses to serve.

Existing non-empty deployments without metadata must be handled as an explicit
adoption/recovery operation. Normal startup must not infer or write a local state
instance from ambiguous existing state.

## Current Behavior

### Ledger MPF Initialization

`makeMpfs` opens the transactions and ledger MPFs from configured LevelDB paths:
[`src/workers/utils/mpf.ts` lines 155-168](../src/workers/utils/mpf.ts#L155-L168).
It checks `ledgerMpf.rootIsEmpty()`:
[`src/workers/utils/mpf.ts` line 169](../src/workers/utils/mpf.ts#L169).
If the root is empty, it logs that no previous ledger MPF root was found,
converts every configured genesis UTxO to an insert batch operation, applies the
batch to the ledger MPF, and logs the new root:
[`src/workers/utils/mpf.ts` lines 170-185](../src/workers/utils/mpf.ts#L170-L185).

The branch is not gated by SQL table counts, L1 state-queue evidence,
`RUN_GENESIS_ON_STARTUP`, `local_state_metadata`, an MPF metadata record, or an
operator recovery command. The current conversion path is no longer
`Effect.allSuccesses`; conversion failure should fail the effect, but that does
not make the branch safe because the mutation is still implicit and
authority-free.

`MidgardMpf.create` opens the LevelDB path and reads the persisted root marker:
[`src/workers/utils/mpf.ts` lines 1602-1616](../src/workers/utils/mpf.ts#L1602-L1616).
`rootIsEmpty` compares the current root to the canonical MPF empty root:
[`src/workers/utils/mpf.ts` lines 1737-1738](../src/workers/utils/mpf.ts#L1737-L1738).
It does not distinguish first use from a deleted, truncated, or wrong backing
store.

### Empty Root Normalization

The wrapper now defines `MPF_EMPTY_ROOT_HEX` as `SDK.EMPTY_MERKLE_TREE_ROOT`:
[`src/workers/utils/mpf.ts` lines 40-42](../src/workers/utils/mpf.ts#L40-L42).
Missing root markers open at the canonical Midgard empty root, and persisted
library internal null roots are rejected:
[`src/workers/utils/mpf.ts` lines 63-78](../src/workers/utils/mpf.ts#L63-L78).
Root marker writes normalize internal null-root values back to the canonical
empty root:
[`src/workers/utils/mpf.ts` lines 1865-1874](../src/workers/utils/mpf.ts#L1865-L1874).

Tests cover new-store empty-root initialization, delete-to-empty persistence,
and corrupt root marker failure:
[`tests/mpf.test.ts` lines 71-85](../tests/mpf.test.ts#L71-L85),
[`tests/mpf.test.ts` lines 120-142](../tests/mpf.test.ts#L120-L142), and
[`tests/mpf.test.ts` lines 247-264](../tests/mpf.test.ts#L247-L264).

This landed normalization removes the old empty-root domain mismatch from the
wrapper. It does not solve the authority problem: an empty root still needs
startup classification before it can be trusted.

### Configured Genesis State

The node config includes `LEDGER_MPF_DB_PATH`, `TRANSACTIONS_MPF_DB_PATH`, and
`GENESIS_UTXOS`:
[`src/services/config.ts` lines 72-74](../src/services/config.ts#L72-L74).
Deprecated root-store names are fail-fast rejected, not translated:
[`src/services/config.ts` lines 77-96](../src/services/config.ts#L77-L96).
`RUN_GENESIS_ON_STARTUP` is parsed separately:
[`src/services/config.ts` lines 186-191](../src/services/config.ts#L186-L191).
The MPF paths default to `midgard-ledger-mpf-db` and
`midgard-transactions-mpf-db`:
[`src/services/config.ts` lines 276-283](../src/services/config.ts#L276-L283).
Mainnet maps `GENESIS_UTXOS` to an empty array:
[`src/services/config.ts` lines 408-410](../src/services/config.ts#L408-L410).

The explicit genesis startup policy excludes mainnet and requires the flag:
[`src/commands/startup-policy.ts` lines 3-12](../src/commands/startup-policy.ts#L3-L12).
`runNode` uses that policy only when deciding whether to fork `Genesis.program`
after startup catch-up/projection:
[`src/commands/listen.ts` lines 135-158](../src/commands/listen.ts#L135-L158).
The MPF empty-root reseed branch does not call this policy.

`Genesis.program` inserts configured genesis UTxOs into `mempool_ledger` on
non-mainnet, suppresses duplicate insert errors, and separately submits a
genesis deposit transaction when genesis UTxOs are configured:
[`src/genesis.ts` lines 24-126](../src/genesis.ts#L24-L126).
`GET /init` runs protocol initialization and then invokes `Genesis.program`:
[`src/commands/listen-router.ts` lines 600-645](../src/commands/listen-router.ts#L600-L645).
That endpoint is separate from the ledger MPF empty-root shortcut.

### Startup Order

`runNode` initializes SQL, runs protocol startup checks, hydrates state-queue and
pending-finalization state, and refuses unfinished mutation jobs:
[`src/commands/listen.ts` lines 72-91](../src/commands/listen.ts#L72-L91).
It then runs deposit catch-up, deposit projection, and withdrawal catch-up:
[`src/commands/listen.ts` lines 92-133](../src/commands/listen.ts#L92-L133).
HTTP serving and background fibers are started afterward:
[`src/commands/listen.ts` lines 160-197](../src/commands/listen.ts#L160-L197).

No startup gate opens or verifies the ledger MPF in this sequence. The block
commitment worker later calls `makeMpfs`:
[`src/workers/commit-block-header.ts` lines 1430-1444](../src/workers/commit-block-header.ts#L1430-L1444).
That means the unsafe reseed can occur after startup checks have passed and while
the node runtime is active.

### Confirmed Ledger DB

The confirmed ledger table is `confirmed_ledger`:
[`src/database/confirmedLedger.ts` line 4](../src/database/confirmedLedger.ts#L4).
Its adapter exposes insert, retrieve, delete-by-outref, and clear operations:
[`src/database/confirmedLedger.ts` lines 6-20](../src/database/confirmedLedger.ts#L6-L20).
The generic ledger schema stores `tx_id`, raw `outref`, raw `output`, `address`,
and timestamp with `outref` as the primary key:
[`src/database/utils/ledger.ts` lines 53-60](../src/database/utils/ledger.ts#L53-L60).
Bulk inserts use `ON CONFLICT DO NOTHING`:
[`src/database/utils/ledger.ts` lines 94-107](../src/database/utils/ledger.ts#L94-L107).

Merge finalization now refuses local merge finalization when L1 confirmation
fails:
[`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1417-1435](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1417-L1435).
After confirmation, it deletes spent confirmed UTxOs, inserts produced UTxOs, and
clears the merged block:
[`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1453-1487](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1453-L1487).
`confirmed_ledger` is an important durable source, but it is only authoritative
for rebuild after it is verified against canonical L1 confirmed-state evidence
and any relevant pending merge/finalization recovery state.

### MPF Mutation And Deletion Paths

Commit construction mutates both the transactions MPF and ledger MPF, then reads
the transaction and UTxO roots:
[`src/workers/utils/mpf.ts` lines 1140-1166](../src/workers/utils/mpf.ts#L1140-L1166).
The commit worker wraps the logical operation in a root transaction for the
ledger MPF:
[`src/workers/commit-block-header.ts` lines 1442-1444](../src/workers/commit-block-header.ts#L1442-L1444).

Therefore the persisted ledger MPF is not simply a confirmed-only root. It is the
latest local pre-state root owned by the block commitment pipeline, potentially
ahead of confirmed state and potentially including deferred or not-yet-finalized
payload.

`deleteMpfStore` still exists and removes an MPF LevelDB directory with
`recursive: true` and `force: true`:
[`src/workers/utils/mpf.ts` lines 230-241](../src/workers/utils/mpf.ts#L230-L241).
Wrapper effects exist for transactions and ledger store deletion:
[`src/workers/utils/mpf.ts` lines 243-251](../src/workers/utils/mpf.ts#L243-L251).
These helpers are used by tests and remain dangerous primitives if wired into
runtime reset/recovery paths without explicit local plus on-chain reset
semantics.

The old `/reset` route is not registered in the current router:
[`src/commands/listen-router.ts` lines 1142-1198](../src/commands/listen-router.ts#L1142-L1198).
Admin routes are currently `/init`, `/commit`, `/merge`, `/stateQueue`,
`/logBlocksDB`, and `/logGlobals`:
[`src/commands/listen-utils.ts` lines 7-16](../src/commands/listen-utils.ts#L7-L16).

Local commit finalization no longer deletes the transaction-root store; it
resets the transactions MPF root marker to empty after SQL finalization
succeeds:
[`src/workers/utils/commit-submission.ts` lines 212-258](../src/workers/utils/commit-submission.ts#L212-L258).
If submission fails, the transactions MPF root marker is preserved for recovery
diagnostics:
[`src/workers/utils/commit-submission.ts` lines 476-491](../src/workers/utils/commit-submission.ts#L476-L491).
Losing the transactions MPF while `processed_mempool` or a pending-finalization
journal expects it remains a fail-closed condition unless the transaction root
can be rebuilt from durable transaction bytes and checked against an expected
root.

## Required State Model

Before implementing the classifier, define these semantics in code and operator
documentation:

- `ledger` MPF root is the latest local UTxO root owned by the block commitment
  pipeline. It starts from verified confirmed state and advances through locally
  prepared/submitted blocks and deferred processed payloads. It is a derived
  cache, not authority.
- `transactions` MPF root is the transaction root for the currently in-progress
  or deferred block payload. It is empty only when no durable processed/deferred
  transaction set depends on it.
- `confirmed_ledger` is a rebuild base only after it is verified against
  canonical L1 confirmed-state evidence.
- `blocks`, `immutable`, `pending_block_finalizations`,
  `processed_mempool`, `mempool_tx_deltas`, and deposit/withdrawal projection
  state are durable replay evidence needed to explain roots ahead of confirmed
  state.
- Every future mutation that advances either persisted MPF root must also update
  durable metadata or a transition journal with previous root, new root, source
  table/journal ids, state-queue evidence, and status.

## Target Invariants

1. `makeMpfs` must be a pure open/verify operation. It must never seed genesis,
   rebuild, delete, or otherwise repair durable state implicitly.
2. Empty ledger MPF root is valid only for a proven fresh deployment with
   explicit bootstrap intent and no prior authoritative state.
3. A node that has ever completed bootstrap must have durable local-state
   metadata proving its state instance, network, contract bundle, genesis
   fingerprint, current ledger MPF root, root source, and last verified height or
   state-queue tip.
4. If SQL, L1, metadata, or journals show prior state, an empty ledger MPF root is
   a corruption/data-loss signal. Startup must fail closed before HTTP serving
   and before mutating workers start.
5. The persisted ledger MPF root must be verified against an authoritative
   durable projection before the block commitment worker can build a new block.
6. `confirmed_ledger` can be used as a rebuild source only after verification
   against canonical L1 confirmed-state root and pending merge/finalization
   journals.
7. Rebuild must be deterministic: the same verified source state must produce the
   same root byte-for-byte, independent of row retrieval order.
8. Rebuild must be staged into a temporary MPF directory, verified against an
   expected root, closed, and atomically promoted only after postconditions pass.
9. Genesis UTxO seeding must be tied to explicit bootstrap, durable metadata
   creation, and root verification. It must never be a recovery fallback.
10. Any operator-triggered recovery must create an audit record before mutation,
    preserve the previous MPF directory as evidence, and write the final outcome.
11. Readiness must be false while MPF integrity is unknown, rebuild is active, or
    recovery has failed.
12. No compatibility modes, legacy fallback IDs, or "try genesis if rebuild
    fails" behavior may be added for `demo/midgard-node`.
13. A non-empty `processed_mempool`, non-empty transactions MPF, or active
    pending-finalization record must be represented in metadata/journals before
    startup can report healthy.
14. Existing deployments must be adopted through an explicit one-time maintenance
    command that verifies the current SQL/MPF/L1 tuple and writes metadata, not
    by automatic startup inference.

## Bootstrap Versus Corruption

The implementation must add an explicit classifier before any ledger MPF can be
trusted:

```text
classifyLocalLedgerState:
  read local_state_metadata
  inspect ledger MPF path and root
  inspect transactions MPF path and root
  inspect SQL table cardinalities and active journals
  inspect L1 protocol deployment/state-queue status
  compare configured network/contracts/genesis fingerprint
  return FreshBootstrap | HealthyRestart | NeedsExplicitRebuild | CorruptFailClosed
```

### Fresh Bootstrap

Fresh bootstrap is allowed only when all of these are true:

- `RUN_GENESIS_ON_STARTUP` or an equivalent explicit init command allows
  bootstrap under the existing startup policy.
- The network is not mainnet.
- No `local_state_metadata` row exists.
- The ledger MPF path is absent or has the empty root.
- The transactions MPF path is absent or has the empty root.
- All local durable SQL state that would imply prior execution is empty:
  `confirmed_ledger`, `latest_ledger`, `mempool_ledger`, `mempool`,
  `processed_mempool`, `mempool_tx_deltas`, `immutable`, `blocks`,
  `deposits_utxos`, `deposit_ingestion_cursor`, withdrawal state,
  `pending_block_finalizations`, `tx_admissions`, `tx_rejections`,
  `local_mutation_jobs`, and future mutation/recovery journals.
- L1 protocol state is empty and about to be initialized by the explicit
  bootstrap flow, or already initialized in a way that precisely matches the
  expected genesis/confirmed root for this bootstrap mode.
- The configured genesis fingerprint matches the binary/config being
  bootstrapped.

Fresh bootstrap must create metadata in the same startup phase that seeds
genesis state. The metadata is the durable marker that future restarts are not
fresh. Genesis UTxO conversion and insertion must be exact: no partial success,
no broad catch-all that reports success after partial state creation, and no
duplicate suppression unless existing row bytes are read back and proven
identical.

### Healthy Restart

Healthy restart requires:

- an existing metadata row whose network, contract identifiers, genesis
  fingerprint, and schema version match the current binary/config;
- a non-empty ledger MPF root, unless the authoritative current root is the empty
  root by protocol design and metadata explicitly records that state;
- SQL root reconstruction from the relevant durable projection matches the
  recorded root;
- the recorded root agrees with canonical L1 state-queue evidence or an active
  recovery journal that explains why local state is ahead of confirmed state;
- the transactions MPF root is empty only when `processed_mempool` and any
  deferred payload journal are empty; otherwise its root is reconstructed from
  ordered durable transaction bytes and compared with metadata or the pending
  journal;
- no active recovery job is in `failed`, `prepared`, or `in_progress` status.

### Needs Explicit Rebuild

The classifier may return `NeedsExplicitRebuild` only when the source of truth is
available and complete but the local MPF cache is missing or corrupt. Examples:

- `confirmed_ledger` verifies against canonical confirmed L1 state and there are
  no unmerged local blocks to replay;
- `confirmed_ledger` plus durable `blocks`/`immutable` state and active
  finalization journals deterministically replay to the current state-queue tip;
- verified confirmed or queued state plus durable `processed_mempool` and
  `mempool_tx_deltas` deterministically rebuilds the latest local pre-state root
  and transaction root expected by a pending deferred submission;
- a previous rebuild was prepared in a temporary directory and metadata proves it
  reached a pre-promotion state with a matching expected root.

Normal `runNode` should fail closed with a recovery-required diagnostic rather
than auto-promote a rebuild. The safer first implementation is
operator-triggered rebuild only.

### Corrupt Fail Closed

The classifier must return `CorruptFailClosed` when:

- the ledger MPF is empty but any SQL table, metadata row, pending journal, or L1
  state indicates prior execution;
- the ledger MPF root differs from the root reconstructed from verified durable
  state;
- the transactions MPF is empty or unreadable while durable processed/deferred tx
  state requires a non-empty transaction root;
- `confirmed_ledger` cannot be verified against L1 confirmed-state evidence;
- durable block history is insufficient to replay from confirmed state to latest
  local state;
- metadata network/contracts/genesis fingerprint differs from current config;
- LevelDB open/root read fails in a way that prevents proving state;
- more than one plausible source root exists.

In these cases the node must not run mutating fibers, must not serve `/submit`,
and must expose an operator-visible reason.

## Authoritative Rebuild Flow

The rebuild path must treat the ledger MPF as a derived cache. It can be rebuilt
only from durable state that has been verified against L1 and local journals.

### Source Selection

The rebuild command should support explicit source modes, with no default that
guesses:

```text
--source confirmed-ledger
--source confirmed-ledger-plus-finalized-blocks
--source latest-local-replay
--source prepared-rebuild
```

`confirmed-ledger` is valid only when:

- canonical L1 confirmed state has been fetched;
- the confirmed state's `utxoRoot` equals the deterministic root computed from
  `confirmed_ledger`;
- there are no unmerged local block records that the ledger MPF is expected to
  include;
- pending merge/finalization journals are absent or terminal.

`confirmed-ledger-plus-finalized-blocks` is valid only when:

- the confirmed base root is verified as above;
- each replayed block is represented by durable `blocks` and `immutable` rows;
- block order is derived from canonical state-queue evidence and/or durable
  pending finalization records, not wall-clock time;
- every transaction delta can be deterministically reconstructed or is already
  stored in an immutable delta table introduced by the atomic mutation plan;
- deposit and withdrawal projections included in those blocks are proven by
  durable event state and header assignments.

`latest-local-replay` is valid only when:

- the confirmed base root is verified;
- every unmerged state-queue block between confirmed state and the latest local
  tip is replayed in canonical state-queue order;
- every locally submitted but not yet merged block is represented by `blocks`,
  `immutable`, and `pending_block_finalizations`;
- any deferred, not-yet-submitted payload is represented by
  `processed_mempool` and `mempool_tx_deltas`, with transaction ordering derived
  from durable timestamps plus a deterministic tie-breaker by `tx_id`;
- the replay produces both the expected ledger root and the expected transaction
  root when a non-empty transactions MPF is required.

`prepared-rebuild` is valid only for resuming a rebuild whose audit row records a
temporary directory, source fingerprint, expected root, and pre-promotion status.

### Deterministic Root Construction

The implementation should add shared helpers that compute MPF roots from ordered
durable sources without mutating the live MPF path:

1. Read candidate entries in deterministic order, preferably by raw `outref`
   bytes for ledger roots.
2. Reject duplicate outrefs or conflicting payloads before creating MPF ops.
3. Convert every row to `{ type: "insert", key: outref, value: output }`.
4. Build the MPF in a temporary path, not in memory for large production state.
5. Read the final root and compare it to the required expected root.
6. Close the MPF and ensure the temporary store is durable before promotion.

The existing `keyValueMpfRoot` helper builds a root from supplied keys and values
using a scratch in-memory MPF:
[`src/workers/utils/mpf.ts` lines 1189-1213](../src/workers/utils/mpf.ts#L1189-L1213).
That is useful for tests and small checks, but production rebuild needs a
path-backed builder, deterministic source reads, and explicit failure metadata.

The helper must expose separate builders for:

- ledger roots from ordered UTxO `(outref, output)` entries;
- transaction roots from ordered `(tx_id, tx_cbor)` entries;
- replayed roots that apply an ordered sequence of spent and produced UTxO
  deltas to a verified base root.

The replay builder must reject missing deltas, duplicate transaction ids with
different payloads, and attempts to spend an outref that is absent from the
current replay state unless a prior verified root explicitly proves that state.

### Promotion

Promotion must be journaled and recoverable:

```text
create rebuild audit row: prepared
build temp MPF
verify temp root == expected root
stop or prove absence of mutating workers
rename live ledger MPF dir to quarantine/backup path
rename temp dir to configured LEDGER_MPF_DB_PATH
open promoted MPF and verify root again
update metadata root and rebuild audit row: complete
```

If promotion fails after the live directory is moved, startup must use the audit
row to either complete the exact same promotion or fail closed. It must never
fall back to genesis.

Promotion must never run concurrently with `runNode`. The implementation should
use a durable maintenance lock plus a process-level file lock on the target MPF
directory. PostgreSQL advisory locking alone is insufficient because LevelDB
state is outside PostgreSQL and can be opened by another process.

## Fail-Closed Startup Behavior

Add a mandatory startup gate before HTTP serving and before background fibers:

```ts
yield* verifyLedgerMpfIntegrityOnStartup;
```

This gate should run after schema/migration verification and after enough L1
topology evidence is available, but before deposit reconciliation, block
commitment, merge, tx queue processing, and explicit non-mainnet genesis
bootstrap. In the current `runNode` shape, that means inserting the gate before
the startup catch-up/projection block and before the HTTP/fiber launch:
[`src/commands/listen.ts` lines 72-197](../src/commands/listen.ts#L72-L197).

Startup must produce machine-readable failure reasons such as:

```text
ledger_mpf_empty_with_nonempty_sql
ledger_mpf_root_mismatch
ledger_mpf_metadata_missing_for_nonempty_node
ledger_mpf_rebuild_required
ledger_mpf_rebuild_incomplete
ledger_mpf_leveldb_unreadable
ledger_mpf_genesis_fingerprint_mismatch
transactions_mpf_missing_for_deferred_payload
local_state_metadata_missing_for_existing_node
```

The failure should include:

- configured ledger and transactions MPF paths;
- observed roots or read/open errors;
- expected roots if known;
- SQL table counts used for classification;
- metadata state if present;
- L1 state-queue reference used for verification;
- recovery command suggestion with required flags.

The block commitment worker should also defensively refuse to proceed if
`makeMpfs` returns unverified handles. Startup is the primary gate; worker
defense prevents future call sites and tests from bypassing it.

`makeMpfs` should return handles carrying an integrity token or verified-root
metadata supplied by the startup gate or recovery command. Direct construction of
commit-worker MPF handles without that token should fail in production mode.

## Observability And Readiness

The current readiness model checks database health, worker heartbeats, queue
depth, local-finalization state, and unresolved block submission age:
[`src/commands/readiness.ts` lines 16-26](../src/commands/readiness.ts#L16-L26)
and [`src/commands/readiness.ts` lines 40-85](../src/commands/readiness.ts#L40-L85).
It has no MPF integrity input yet.

Add MPF integrity state to readiness. Readiness must be false when any of these
states is active:

- `ledger_mpf_integrity_unknown`
- `ledger_mpf_rebuild_required`
- `ledger_mpf_rebuild_in_progress`
- `ledger_mpf_rebuild_failed`
- `transactions_mpf_missing_for_deferred_payload`
- `local_state_metadata_missing_for_existing_node`

Emit metrics with stable low-cardinality labels:

- `ledger_mpf_integrity_status{status,reason}` as a gauge where `1` is current.
- `ledger_mpf_rebuild_total{source,status}` as a counter.
- `ledger_mpf_rebuild_duration_ms{source,status}` as a timer or histogram.
- `ledger_mpf_root_verification_total{source,status}` as a counter.
- `transactions_mpf_integrity_status{status,reason}` as a gauge.

Structured logs for startup verification, adoption, rebuild dry-run, rebuild
promotion, and fail-closed decisions must include `state_instance_id`,
`ledger_mpf_path`, `transactions_mpf_path`, observed roots, expected roots when
known, source fingerprint, state-queue tip, and recovery/adoption ids. Do not log
seed phrases, wallet secrets, or full transaction payloads.

## Operator Recovery Command Design

Introduce recovery commands, not runtime fallbacks:

```text
midgard-node recover ledger-mpf \
  --source confirmed-ledger \
  --expected-root <hex> \
  --reason "<ticket-or-incident-id>" \
  --dry-run

midgard-node recover ledger-mpf \
  --source confirmed-ledger \
  --expected-root <hex> \
  --reason "<ticket-or-incident-id>" \
  --write

midgard-node adopt local-state \
  --expected-ledger-root <hex> \
  --expected-transactions-root <hex-or-empty> \
  --expected-state-queue-tip <hex> \
  --reason "<migration-ticket>" \
  --write
```

Required behavior:

- `--dry-run` computes and reports the source fingerprint and root without
  touching the live MPF directory.
- `--write` requires an expected root and refuses to proceed if the computed root
  differs.
- The command acquires a durable recovery lock so only one node or operator can
  rebuild at a time.
- The command refuses to run while the node is serving mutating traffic unless a
  durable maintenance/recovery mode proves workers are stopped.
- The command creates a rebuild audit row before reading source data.
- The command builds into a temporary directory under the same filesystem as the
  target path so rename promotion is atomic.
- The command preserves the previous live MPF directory under a quarantine path
  named with rebuild id and timestamp.
- The command reopens the promoted MPF and verifies root before completing.
- The command updates metadata only after promotion verification.
- Any failure leaves an audit row with status and error details; subsequent
  startup fails closed until the audit row is completed, resumed, or explicitly
  abandoned with evidence.

The one-time `adopt local-state` command is for migrating an existing non-empty
deployment after metadata tables are introduced. It must:

- require maintenance mode and the same recovery locks as rebuild;
- read the existing live ledger and transactions roots without mutating them;
- verify SQL table counts, L1 topology, configured network/contracts, genesis
  fingerprint, state-queue tip, and expected roots supplied by the operator;
- prove that any local-ahead state is explained by `blocks`, `immutable`,
  `pending_block_finalizations`, `processed_mempool`, and durable deltas;
- write `local_state_metadata` and an adoption audit row only after all checks
  pass.

This is an explicit auditable migration, not a compatibility fallback. Normal
startup must still fail when metadata is missing for a non-empty node.

Do not add `--force-genesis`, `--allow-empty-as-genesis`, compatibility flags, or
legacy path fallback behavior. If an operator truly needs a new local instance,
it must be a full, explicit local plus on-chain redeploy/reset or a clearly new
database/MPF path with metadata. Do not combine clean local state with existing
on-chain protocol state.

## Schema And Metadata Changes

Implement these through the versioned migration system rather than ad hoc
startup `CREATE TABLE IF NOT EXISTS`.

### `local_state_metadata`

One row per local node state instance:

- `state_instance_id UUID PRIMARY KEY`.
- `network TEXT NOT NULL`.
- `contract_bundle_hash TEXT NOT NULL`.
- `state_queue_policy_id TEXT NOT NULL`.
- `genesis_fingerprint TEXT NOT NULL`.
- `schema_version TEXT NOT NULL`.
- `ledger_mpf_path TEXT NOT NULL`.
- `ledger_mpf_root BYTEA NOT NULL`.
- `ledger_mpf_root_source TEXT NOT NULL`, for example `bootstrap`,
  `block_commit`, `merge_rebuild`, `operator_rebuild`.
- `transactions_mpf_path TEXT NOT NULL`.
- `transactions_mpf_root BYTEA NOT NULL`.
- `transactions_mpf_root_source TEXT NOT NULL`, for example `empty`,
  `deferred_commit_payload`, `operator_rebuild`.
- `confirmed_state_header_hash BYTEA`.
- `state_queue_tip_hash BYTEA`.
- `latest_local_header_hash BYTEA`.
- `active_pending_finalization_hash BYTEA`.
- `last_verified_at TIMESTAMPTZ NOT NULL`.
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.

The row must be created during fresh bootstrap before mutating workers can run.
On restart, the row must match current config exactly.

For existing deployments, the migration must leave this table empty. The
operator must run `adopt local-state --write` after verifying a snapshot; startup
then succeeds only if the adopted metadata still matches live SQL/MPF/L1 state.

### `ledger_mpf_rebuilds`

Audit table for rebuild attempts:

- `rebuild_id UUID PRIMARY KEY`.
- `state_instance_id UUID NOT NULL`.
- `source TEXT NOT NULL`.
- `source_fingerprint BYTEA NOT NULL`.
- `expected_root BYTEA NOT NULL`.
- `computed_root BYTEA`.
- `status TEXT NOT NULL`, constrained to `prepared`, `building`, `verified`,
  `promoting`, `complete`, `failed`, `abandoned`.
- `temp_path TEXT`.
- `quarantine_path TEXT`.
- `transactions_temp_path TEXT`.
- `transactions_quarantine_path TEXT`.
- `operator_reason TEXT NOT NULL`.
- `started_by TEXT`.
- `error_code TEXT`.
- `error_detail TEXT`.
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`.
- `completed_at TIMESTAMPTZ`.

Only one non-terminal rebuild should be allowed per `state_instance_id`.

Add a separate `local_state_adoptions` audit table or include adoption rows in a
generalized recovery audit table. Adoption records must include
`state_instance_id`, expected ledger and transactions roots, observed ledger and
transactions roots, state-queue tip, source fingerprint, operator reason,
status, error fields, and timestamps.

### Root Verification Helpers

Add strict SQL helpers rather than reusing permissive insert/retrieve behavior:

- count rows in each state table for bootstrap classification;
- retrieve ledger entries ordered by `outref`;
- retrieve transaction entries ordered by durable commit order and `tx_id`
  tie-breaker;
- detect duplicate/conflicting rows before root construction;
- compute a stable source fingerprint over ordered `(outref, output)` pairs;
- compare byte payloads exactly, not semantically normalized forms unless that is
  already the protocol definition.

Because `insertEntries` currently uses `ON CONFLICT DO NOTHING`
([`src/database/utils/ledger.ts` lines 94-107](../src/database/utils/ledger.ts#L94-L107)),
recovery code must not rely on it to prove equality. It must explicitly reject
conflicting existing rows.

## Tests And Fault Injection

### Unit Tests

- `classifyLocalLedgerState` returns `FreshBootstrap` only when metadata is
  absent, MPF roots are empty/absent, SQL tables are empty, L1 state is empty or
  matching bootstrap state, and bootstrap is explicitly enabled.
- Empty ledger MPF plus non-empty `confirmed_ledger` returns
  `CorruptFailClosed`.
- Empty ledger MPF plus non-empty `blocks`, `immutable`, `deposits_utxos`, or
  active pending journal returns `CorruptFailClosed`.
- Missing metadata plus non-empty SQL returns `CorruptFailClosed`.
- Metadata network/contract/genesis mismatch returns `CorruptFailClosed`.
- Non-empty ledger MPF whose root differs from reconstructed SQL root returns
  `CorruptFailClosed`.
- Rebuild root construction is deterministic under randomized SQL row order.
- Rebuild of deferred transaction roots is deterministic under equal timestamps
  by sorting on `tx_id` as the tie-breaker.
- Rebuild rejects duplicate outrefs and conflicting output bytes.
- Rebuild rejects missing `mempool_tx_deltas` for any transaction that must be
  replayed into the ledger root.
- `makeMpfs` no longer inserts genesis UTxOs on empty root.
- `Genesis.program` fails on partial genesis seeding and does not suppress
  non-identical duplicate rows.

### Integration Tests

- Fresh non-mainnet bootstrap creates metadata, seeds genesis only through the
  explicit bootstrap path, and verifies the resulting root.
- Existing non-empty SQL/MPF without metadata fails startup until
  `adopt local-state --write` verifies the tuple and writes metadata.
- Restart after bootstrap with intact SQL/MPF/metadata is healthy and does not
  reseed.
- Delete the ledger MPF directory after creating non-empty SQL state; startup
  fails before HTTP and workers start.
- Truncate or corrupt the LevelDB ledger store; startup fails with
  `ledger_mpf_leveldb_unreadable` or `ledger_mpf_root_mismatch`.
- Insert a conflicting `confirmed_ledger` row; rebuild dry-run fails before temp
  MPF promotion.
- Rebuild dry-run from verified `confirmed_ledger` computes the expected root and
  leaves the live directory unchanged.
- Rebuild write from verified `confirmed_ledger` promotes a temp MPF, reopens it,
  updates metadata, and allows the next startup.
- Rebuild from `latest-local-replay` reconstructs ledger and transactions roots
  when `processed_mempool` contains deferred payload and refuses if any tx delta
  is missing.
- Crash/fault after temp build but before promotion: startup reports
  `ledger_mpf_rebuild_incomplete`.
- Crash/fault after live directory quarantine but before temp promotion: recovery
  resumes or fails closed without reseeding genesis.
- Crash/fault after promotion but before metadata update: startup verifies the
  promoted root against the rebuild audit row and either completes metadata
  update or fails closed.
- `GET /init` and `RUN_GENESIS_ON_STARTUP` both use the same strict bootstrap
  path and cannot run genesis seeding after metadata exists.
- Future reset/new-instance tooling refuses to delete local SQL/MPF state unless
  it also performs a full explicit on-chain redeploy/reset of protocol state.

### Fault Injection Points

Add test-only injection hooks around:

- after classifier reads metadata;
- after source SQL rows are read;
- after temp MPF root is computed;
- after temp MPF close/fsync;
- after live directory is moved to quarantine;
- after temp directory is promoted;
- after metadata update;
- during LevelDB open/root read.

These hooks must be test-only and removed from default behavior or gated behind
explicit test configuration.

## Rollout Steps

1. Add versioned migrations for `local_state_metadata`,
   `ledger_mpf_rebuilds`, and adoption/recovery audit metadata.
2. Add strict SQL inspection helpers for table counts, ordered ledger reads, and
   source fingerprinting.
3. Implement the ledger state classifier with tests for bootstrap, restart,
   rebuild-needed, and corruption cases.
4. Remove genesis seeding from `makeMpfs`; make it open and return verified MPF
   handles only.
5. Move genesis MPF seeding into the explicit bootstrap path, make it strict and
   ordered, and require metadata creation plus root verification.
6. Add `verifyLedgerMpfIntegrityOnStartup` to the mandatory startup gate before
   serving traffic or starting mutating workers.
7. Add defensive worker checks so block commitment cannot use unverified MPF
   handles.
8. Implement `adopt local-state --dry-run/--write` for existing deployments and
   require it before any non-empty node without metadata can start.
9. Implement rebuild dry-run command from verified `confirmed_ledger`.
10. Implement rebuild write command with temp directory, expected-root check,
    quarantine, promotion, metadata update, and audit rows.
11. Implement `latest-local-replay` only after durable replay evidence is
    complete; until then, fail closed for roots ahead of confirmed state.
12. Add recovery/resume handling for incomplete rebuild audit rows.
13. Add readiness input and metrics for MPF integrity/rebuild state.
14. Run unit tests, integration tests, and fault-injection tests.
15. Exercise an emulator flow: bootstrap, deposit, commit, merge, stop, delete
    ledger MPF, verify startup fail-closed, rebuild, restart, and build the next
    block.
16. Document operator runbook commands, expected diagnostics, and evidence to
    preserve.
17. Deploy first to disposable staging state, then to persistent staging with a
    copied database/MPF pair, then to production only after rollback/new-instance
    procedures are documented.

## Risks And Constraints

- The persistent `ledger` MPF role is defined in this plan as the latest local
  block-commitment UTxO root. If implementation discovers code paths that use it
  as a different root, those paths must be reconciled before approval; do not add
  a mode switch.
- `confirmed_ledger` is only authoritative after it is verified against L1
  confirmed-state evidence and relevant local merge/finalization journals.
- The current schema may not retain enough immutable per-transaction delta data
  to replay from confirmed state to latest local state. If not, either add
  durable deltas before supporting replay rebuilds or limit rebuild to
  confirmed-only states.
- L1 provider APIs must expose enough state-queue and confirmed-state evidence to
  verify expected roots. If not, add local durable proof/journal data before
  relying on rebuild.
- Atomic directory promotion is filesystem-dependent. The implementation should
  require temp and live paths on the same filesystem and fail otherwise.
- The live `/reset` route is gone, but future reset tooling must still obey the
  local state reset/on-chain redeploy rule. Deleting SQL/MPF state without a full
  on-chain redeploy/reset creates split-brain state.
- Mainnet genesis configuration is empty, but that alone is not a safety proof.
  Mainnet must still reject empty-root recovery unless all fresh-bootstrap
  conditions are impossible or explicitly disabled.
- Existing tests may assume `makeMpfs` seeds genesis. Those tests should be
  updated to call explicit bootstrap helpers, not preserved through compatibility
  behavior.
- Existing deployments require an explicit adoption window. This is operational
  migration work, but it is safer than auto-generating metadata from ambiguous
  state at startup.

## Concrete Checklist

Completed supporting changes:

- [x] Rename runtime MPF module, helpers, logs, and config fields to MPF naming.
- [x] Reject deprecated `LEDGER_MPT_DB_PATH`, `MEMPOOL_MPT_DB_PATH`, and
      `MEMPOOL_MPF_DB_PATH` names during config load.
- [x] Normalize MPF empty roots to `SDK.EMPTY_MERKLE_TREE_ROOT`.
- [x] Add unit tests for canonical empty root, delete-to-empty normalization, and
      corrupt root markers.
- [x] Remove the old HTTP `/reset` route from the current command router.
- [x] Preserve the transactions MPF root marker on submission failure.

Remaining blocker work:

- [ ] Define and document the semantic owner of the persistent ledger and
      transactions MPF roots as latest local commit-pipeline roots in code and
      operator docs.
- [ ] Add migrations for `local_state_metadata` and `ledger_mpf_rebuilds`.
- [ ] Add adoption/recovery audit metadata for existing non-empty deployments.
- [ ] Store network, contracts, genesis fingerprint, root, and source metadata on
      fresh bootstrap.
- [ ] Store transactions MPF root/source and active pending-finalization
      identifiers in metadata.
- [ ] Implement SQL table-count, ordered ledger source, and ordered tx source
      inspection helpers.
- [ ] Implement deterministic root/source fingerprint computation from ledger
      rows.
- [ ] Implement deterministic tx-root and replay-root computation from durable tx
      bytes and deltas.
- [ ] Implement `classifyLocalLedgerState`.
- [ ] Remove empty-root genesis seeding from `makeMpfs`.
- [ ] Add explicit genesis MPF seeding only to proven fresh bootstrap.
- [ ] Make `Genesis.program`, startup bootstrap, and `GET /init` share one strict
      bootstrap path.
- [ ] Add `adopt local-state --dry-run/--write` for explicit metadata migration.
- [ ] Add mandatory startup MPF integrity verification.
- [ ] Ensure startup fails before HTTP server and mutating workers on unknown,
      corrupt, mismatched, or rebuild-required ledger MPF state.
- [ ] Add defensive block-commitment worker check for verified MPF state.
- [ ] Design and implement `recover ledger-mpf --dry-run`.
- [ ] Design and implement `recover ledger-mpf --write`.
- [ ] Add `latest-local-replay` only when durable replay evidence is complete;
      otherwise fail closed for local-ahead roots.
- [ ] Add temp-dir build, root verification, quarantine, atomic promotion, and
      reopen verification.
- [ ] Add resume/fail-closed handling for incomplete rebuild audit rows.
- [ ] Add readiness reasons and metrics for MPF integrity and rebuild state.
- [ ] Add unit tests for classifier, root construction, metadata mismatch, and
      empty-root corruption.
- [ ] Add integration tests for fresh bootstrap, healthy restart, missing MPF,
      corrupt MPF, dry-run rebuild, write rebuild, and post-rebuild restart.
- [ ] Add fault-injection tests for rebuild crash points.
- [ ] Update operator runbook with diagnostics, required evidence, and recovery
      commands.
- [ ] Verify no path silently reseeds genesis after metadata or SQL state exists.
