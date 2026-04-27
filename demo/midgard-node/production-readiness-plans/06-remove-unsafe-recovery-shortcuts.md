# P0 Blocker 6: Remove Unsafe Recovery Shortcuts

Status: implementation plan only. No code changes are included in this
document.

Owner scope: `demo/midgard-node` startup and recovery handling for the
LevelDB-backed ledger MPT, its relationship to PostgreSQL durable state, and
the configured genesis bootstrap path.

Production standard: an empty ledger MPT must never be interpreted as a safe
genesis bootstrap unless every authoritative durable source proves this is a new
deployment and bootstrap was explicitly requested. Otherwise startup must fail
closed or rebuild from verified durable authority through an auditable recovery
path.

## Problem Statement

The block commitment worker opens a persistent ledger Merkle Patricia Trie and
uses it as the local UTxO root source for block construction. Today, when that
trie root is empty, the helper silently inserts configured genesis UTxOs. That
collapses two materially different states into the same behavior:

- a legitimate first bootstrap of an empty non-mainnet node; and
- a corrupted or data-lost node whose PostgreSQL and/or L1 state shows prior
  history but whose LevelDB ledger trie is missing, reset, or empty.

For a production L2, this is a state-integrity blocker. A missing local root is
not proof that the ledger returned to genesis. It is evidence that the node has
lost or cannot read part of its durable state. Continuing from reseeded genesis
can create roots that disagree with confirmed SQL state, queued block state,
deposit projection state, and the canonical state queue on L1.

The fix must remove implicit recovery behavior from normal MPT construction. The
node must classify startup as either a proven bootstrap, a proven healthy
restart, an explicit verified rebuild, or an integrity failure that refuses to
serve.

Existing non-empty deployments without metadata must be handled as an explicit
adoption/recovery operation. The migration may create metadata tables, but
normal startup must not infer or write a local state instance from ambiguous
existing state.

## Current Behavior

The production readiness report identifies this blocker at
[`PRODUCTION_READINESS_REPORT.md` lines 466-522](../PRODUCTION_READINESS_REPORT.md#L466-L522).
The report's core finding is that empty ledger trie state is interpreted as
"new node, bootstrap from genesis" even though an empty trie can also mean data
loss.

### Ledger MPT Initialization

`makeMpts` opens the mempool and ledger tries from configured LevelDB paths:
[`src/workers/utils/mpt.ts` lines 86-99](../src/workers/utils/mpt.ts#L86-L99).

It then checks whether the ledger root is empty:
[`src/workers/utils/mpt.ts` line 100](../src/workers/utils/mpt.ts#L100).
If the root is empty, it logs that no prior root was found, converts every
configured genesis UTxO to a put operation, ignores per-UTxO conversion failures
through `Effect.allSuccesses`, batches the surviving operations into the trie,
and logs the new root:
[`src/workers/utils/mpt.ts` lines 101-119](../src/workers/utils/mpt.ts#L101-L119).

This branch is not gated by SQL state, L1 state, an MPT metadata record, an
operator recovery command, or `RUN_GENESIS_ON_STARTUP`.

`MidgardMpt.create` opens the LevelDB path and enables root persistence when a
path is provided:
[`src/workers/utils/mpt.ts` lines 671-703](../src/workers/utils/mpt.ts#L671-L703).
`rootIsEmpty` only compares the current trie root to the empty trie constant:
[`src/workers/utils/mpt.ts` lines 774-783](../src/workers/utils/mpt.ts#L774-L783).
It does not distinguish a first use from a deleted or truncated backing store.

### Configured Genesis State

The node config includes `LEDGER_MPT_DB_PATH`, `MEMPOOL_MPT_DB_PATH`, and
`GENESIS_UTXOS`:
[`src/services/config.ts` lines 65-67](../src/services/config.ts#L65-L67).
`RUN_GENESIS_ON_STARTUP` is parsed separately:
[`src/services/config.ts` lines 154-159](../src/services/config.ts#L154-L159).
The MPT paths default to local directories:
[`src/services/config.ts` lines 223-227](../src/services/config.ts#L223-L227).
Testnet genesis UTxOs are constructed from configured seed phrases:
[`src/services/config.ts` lines 229-285](../src/services/config.ts#L229-L285),
and mainnet maps `GENESIS_UTXOS` to an empty array:
[`src/services/config.ts` line 346](../src/services/config.ts#L346).

The explicit genesis startup policy intentionally excludes mainnet and requires
the flag:
[`src/commands/startup-policy.ts` lines 9-12](../src/commands/startup-policy.ts#L9-L12).
`runNode` uses that policy only for forking `Genesis.program`:
[`src/commands/listen.ts` lines 90-108](../src/commands/listen.ts#L90-L108).
The MPT genesis reseed branch does not call this policy.

`Genesis.program` is also best-effort. It inserts configured genesis UTxOs into
`mempool_ledger` on non-mainnet and suppresses duplicate insert errors:
[`src/genesis.ts` lines 24-67](../src/genesis.ts#L24-L67).
It separately submits a genesis deposit transaction when genesis UTxOs are
configured:
[`src/genesis.ts` lines 73-113](../src/genesis.ts#L73-L113).
The two effects are run concurrently and all causes are logged:
[`src/genesis.ts` lines 119-126](../src/genesis.ts#L119-L126).

`GET /init` runs protocol initialization and then invokes `Genesis.program`:
[`src/commands/listen-router.ts` lines 567-568](../src/commands/listen-router.ts#L567-L568).
That endpoint is separate from the ledger MPT empty-root shortcut.

### Startup Order

`runNode` initializes SQL and then runs startup checks before starting HTTP and
background fibers:
[`src/commands/listen.ts` lines 69-72](../src/commands/listen.ts#L69-L72).
Protocol initialization is mandatory and dies on failure:
[`src/commands/listen-startup.ts` lines 91-159](../src/commands/listen-startup.ts#L91-L159).
Pending block finalization hydration is mandatory and dies on failure:
[`src/commands/listen-startup.ts` lines 211-258](../src/commands/listen-startup.ts#L211-L258).

However, the ledger MPT is not opened and checked in this startup gate. It is
opened later by the block commitment worker:
[`src/workers/commit-block-header.ts` lines 1047-1050](../src/workers/commit-block-header.ts#L1047-L1050).
That means the unsafe reseed can occur after the node has already started its
runtime and workers.

### Confirmed Ledger DB

The confirmed ledger table is `confirmed_ledger`:
[`src/database/confirmedLedger.ts` line 4](../src/database/confirmedLedger.ts#L4).
Its adapter exposes insert, retrieve, delete-by-outref, and clear operations:
[`src/database/confirmedLedger.ts` lines 6-20](../src/database/confirmedLedger.ts#L6-L20).

The generic ledger table schema stores `tx_id`, raw `outref`, raw `output`,
`address`, and timestamp with `outref` as the primary key:
[`src/database/utils/ledger.ts` lines 53-60](../src/database/utils/ledger.ts#L53-L60).
Bulk inserts use `ON CONFLICT DO NOTHING`:
[`src/database/utils/ledger.ts` lines 94-107](../src/database/utils/ledger.ts#L94-L107).
Full-table retrieval is available:
[`src/database/utils/ledger.ts` lines 118-125](../src/database/utils/ledger.ts#L118-L125).
Deletes remove rows by outref:
[`src/database/utils/ledger.ts` lines 195-203](../src/database/utils/ledger.ts#L195-L203).

Merge finalization mutates `confirmed_ledger` after a block is merged into
confirmed state by deleting spent outrefs, inserting produced UTxOs, and clearing
the merged block:
[`src/transactions/state-queue/merge-to-confirmed-state.ts` lines 1413-1435](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1413-L1435).
Therefore `confirmed_ledger` is an important durable source, but it is only
authoritative for rebuild after it is itself verified against canonical L1
confirmed-state evidence and any required pending merge recovery state.

### MPT Mutation And Deletion Paths

`processMpts` mutates both the mempool trie and the ledger trie, then reads the
transaction and UTxO roots:
[`src/workers/utils/mpt.ts` lines 411-417](../src/workers/utils/mpt.ts#L411-L417).
The commit worker wraps the logical operation in a checkpoint only for the
ledger trie:
[`src/workers/commit-block-header.ts` lines 1058-1060](../src/workers/commit-block-header.ts#L1058-L1060).

When no confirmed block is available, `databaseOperationsProgram` still calls
`processMpts`, commits the ledger checkpoint, and then defers submission by
moving payload into durable processed-mempool state:
[`src/workers/commit-block-header.ts` lines 880-948](../src/workers/commit-block-header.ts#L880-L948).
Therefore the persisted ledger MPT is not a confirmed-only root. It is the
latest local pre-state root for the block commitment pipeline, potentially ahead
of confirmed state and potentially including deferred-but-not-yet-submitted
transactions.

`deleteMpt` removes an MPT backing directory with `recursive: true` and
`force: true`:
[`src/workers/utils/mpt.ts` lines 156-167](../src/workers/utils/mpt.ts#L156-L167).
`MidgardMpt.delete` closes the backing store and calls `deleteMpt`:
[`src/workers/utils/mpt.ts` lines 706-717](../src/workers/utils/mpt.ts#L706-L717).

Local commit finalization deletes the mempool trie alongside SQL mutations:
[`src/workers/utils/commit-submission.ts` lines 120-152](../src/workers/utils/commit-submission.ts#L120-L152).
The reset path clears many SQL tables and deletes both mempool and ledger MPT
stores:
[`src/reset.ts` lines 233-255](../src/reset.ts#L233-L255).
The reset workflow marks `RESET_IN_PROGRESS` only in memory:
[`src/reset.ts` lines 269-302](../src/reset.ts#L269-L302).

The ledger MPT delete path is not the direct cause of the blocker, but it shows
why empty MPT state must be interpreted through durable metadata rather than the
root value alone.

The mempool MPT also needs integrity handling. It is deleted after local commit
finalization, but while deferred commit payload exists it contributes the
transaction root that must match a later block header or local-finalization
recovery check. Losing the mempool MPT while `processed_mempool` or a
pending-finalization journal expects it is a fail-closed condition unless the tx
root can be rebuilt from durable transaction bytes with an expected root.

## Required State Model

Before implementing the classifier, define the ledger and mempool trie semantics
in code and operator documentation:

- `ledger` MPT root is the latest local UTxO root owned by the block commitment
  pipeline. It starts from verified confirmed state and advances through locally
  prepared/submitted blocks and deferred processed payloads. It is a derived
  cache, not an authority.
- `mempool` MPT root is the transaction root for the currently deferred or
  in-progress block payload. It is empty only when no durable
  processed/deferred transaction set depends on it.
- `confirmed_ledger` is a rebuild base only after it is verified against
  canonical L1 confirmed-state evidence.
- `blocks`, `immutable`, `pending_block_finalizations`,
  `processed_mempool`, `mempool_tx_deltas`, and deposit projection state are the
  durable replay evidence needed to explain roots that are ahead of confirmed
  state.
- Every future mutation that advances either persisted MPT root must also update
  durable metadata or a transition journal with previous root, new root, source
  table/journal ids, state-queue evidence, and status. Until P0 blocker 2
  provides a fully atomic cross-store transition journal, this plan must use the
  existing pending-finalization and processed-mempool records as minimum replay
  evidence and fail closed whenever they cannot explain the persisted roots.

This removes the open question about the persistent ledger MPT owner: the owner
is the local block commitment pipeline, and the authoritative explanation of its
root is durable SQL plus canonical L1 state-queue evidence.

## Target Invariants

1. `makeMpts` must be a pure open/verify operation. It must never seed genesis,
   rebuild, delete, or otherwise repair durable state implicitly.
2. Empty ledger MPT root is valid only for a proven fresh deployment with
   explicit bootstrap intent and no prior authoritative state.
3. A node that has ever completed bootstrap must have durable local-state
   metadata proving its state instance, network, contract bundle, genesis
   fingerprint, current ledger MPT root, root source, and last verified height or
   state-queue tip.
4. If SQL, L1, metadata, or journals show prior state, an empty ledger MPT root is
   a corruption/data-loss signal. Startup must fail closed before HTTP serving
   and before mutating workers start.
5. The persisted ledger MPT root must be verified against an authoritative
   durable projection before the block commitment worker can build a new block.
6. `confirmed_ledger` can be used as a rebuild source only after verification
   against canonical L1 confirmed-state root and pending merge/finalization
   journals.
7. Rebuild must be deterministic: the same verified source state must produce the
   same root byte-for-byte, independent of row retrieval order.
8. Rebuild must be staged into a temporary MPT directory, verified against an
   expected root, fsynced/closed, and atomically promoted only after all
   postconditions pass.
9. Genesis UTxO seeding must be tied to explicit bootstrap, durable metadata
   creation, and root verification. It must never be a recovery fallback.
10. Any operator-triggered recovery must create an audit record before mutation,
    preserve the previous MPT directory as evidence, and write the final outcome.
11. Readiness must be false while MPT integrity is unknown, rebuild is active, or
    recovery has failed.
12. No compatibility modes, legacy fallback IDs, or "try genesis if rebuild fails"
    behavior may be added for `demo/midgard-node`.
13. A non-empty `processed_mempool`, non-empty mempool MPT, or active
    pending-finalization record must be represented in metadata/journals before
    startup can report healthy. Unknown deferred payload state is not recoverable
    by guessing.
14. Existing deployments must be adopted through an explicit one-time
    maintenance command that verifies the current SQL/MPT/L1 tuple and writes
    metadata, not by automatic startup inference.

## Bootstrap Versus Corruption

The implementation must add an explicit classifier before any ledger MPT can be
trusted:

```text
classifyLocalLedgerState:
  read local_state_metadata
  inspect ledger MPT path and root
  inspect mempool MPT path and root
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
- The ledger MPT path is absent or has the empty root.
- The mempool MPT path is absent or has the empty root.
- All local durable SQL state that would imply prior execution is empty:
  `confirmed_ledger`, `latest_ledger`, `mempool_ledger`, `mempool`,
  `processed_mempool`, `mempool_tx_deltas`, `immutable`, `blocks`,
  `deposits_utxos`, `deposit_ingestion_cursor`,
  `pending_block_finalizations`, and future mutation/recovery journals.
- L1 protocol state is either empty and about to be initialized by the explicit
  bootstrap flow, or already initialized in a way that precisely matches the
  expected genesis/confirmed root for this bootstrap mode.
- The configured genesis fingerprint matches the binary/config being bootstrapped.

Fresh bootstrap must create metadata in the same startup phase that seeds genesis
state. The metadata is the durable marker that future restarts are not fresh.
Genesis UTxO conversion and insertion must be exact, not best-effort: no
`Effect.allSuccesses`, no broad catch-all that reports success after partial
state creation, and no duplicate suppression unless the existing row bytes are
read back and proven identical.

### Healthy Restart

Healthy restart requires:

- an existing metadata row whose network, contract identifiers, genesis
  fingerprint, and schema version match the current binary/config;
- a non-empty ledger MPT root, unless the authoritative current root is the empty
  root by protocol design and metadata explicitly records that state;
- SQL root reconstruction from the relevant durable projection matches the
  recorded root;
- the recorded root agrees with canonical L1 state queue evidence or an active
  recovery journal that explains why local state is ahead of confirmed state;
- the mempool MPT root is empty only when `processed_mempool` and any deferred
  payload journal are empty; otherwise its root is reconstructed from ordered
  durable transaction bytes and compared with metadata or the pending journal;
- no active recovery job is in `failed`, `prepared`, or `in_progress` status.

### Needs Explicit Rebuild

The classifier may return `NeedsExplicitRebuild` only when the source of truth is
available and complete but the local MPT cache is missing or corrupt. Examples:

- `confirmed_ledger` verifies against canonical confirmed L1 state and there are
  no unmerged local blocks to replay;
- `confirmed_ledger` plus durable `blocks`/`immutable` state and active
  finalization journals deterministically replay to the current state-queue tip;
- verified confirmed or queued state plus durable `processed_mempool` and
  `mempool_tx_deltas` deterministically rebuilds the latest local pre-state root
  and mempool tx root expected by a pending deferred submission;
- a previous rebuild was prepared in a temporary directory and metadata proves it
  reached a pre-promotion state with a matching expected root.

Normal `runNode` should fail closed with a recovery-required diagnostic rather
than auto-promote a rebuild unless the implementation explicitly scopes a
startup rebuild as deterministic, non-destructive, journaled, and root-verified.
The safer first implementation is operator-triggered rebuild only.

### Corrupt Fail Closed

The classifier must return `CorruptFailClosed` when:

- the ledger MPT is empty but any SQL table, metadata row, pending journal, or L1
  state indicates prior execution;
- the ledger MPT root differs from the root reconstructed from verified durable
  state;
- the mempool MPT is empty or unreadable while durable processed/deferred tx
  state requires a non-empty tx root;
- `confirmed_ledger` cannot be verified against L1 confirmed-state evidence;
- durable block history is insufficient to replay from confirmed state to latest
  local state;
- metadata network/contracts/genesis fingerprint differs from current config;
- LevelDB open/root read fails in a way that prevents proving state;
- more than one plausible source root exists.

In these cases the node must not run mutating fibers, must not serve `/submit`,
and must expose an operator-visible reason.

## Authoritative Rebuild Flow

The rebuild path must treat the ledger MPT as a derived cache. It can be rebuilt
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
- there are no unmerged local block records that the ledger MPT is expected to
  include;
- pending merge/finalization journals are absent or terminal.

`confirmed-ledger-plus-finalized-blocks` is valid only when:

- the confirmed base root is verified as above;
- each replayed block is represented by durable `blocks` and `immutable` rows;
- block order is derived from canonical state-queue evidence and/or durable
  pending finalization records, not from wall-clock time;
- every transaction delta can be deterministically reconstructed or is already
  stored in an immutable delta table introduced by the atomic mutation plan;
- deposit projections included in those blocks are proven by durable deposit
  state and header assignments.

`latest-local-replay` is valid only when:

- the confirmed base root is verified;
- every unmerged state-queue block between confirmed state and the latest local
  tip is replayed in canonical state-queue order;
- every locally submitted but not yet merged block is represented by `blocks`,
  `immutable`, and `pending_block_finalizations`;
- any deferred, not-yet-submitted payload is represented by
  `processed_mempool` and `mempool_tx_deltas`, with transaction ordering derived
  from durable timestamps plus a deterministic tie-breaker by `tx_id`;
- the replay produces both the expected ledger root and the expected mempool tx
  root when a non-empty mempool MPT is required.

`prepared-rebuild` is valid only for resuming a rebuild whose audit row records a
temporary directory, source fingerprint, expected root, and pre-promotion status.

### Deterministic Root Construction

The implementation should add a shared helper that computes MPT roots from a set
of ledger entries without mutating the live MPT path:

1. Read candidate entries in a deterministic order, preferably by raw `outref`
   bytes.
2. Reject duplicate outrefs or conflicting payloads before creating MPT ops.
3. Convert every row to `{ type: "put", key: outref, value: output }`.
4. Build the trie in a temporary path, not in memory for large production state.
5. Read the final root and compare it to the required expected root.
6. Close the trie and ensure the temporary store is durable before promotion.

The existing `keyValueMptRoot` helper builds a root from supplied keys and values
using an in-memory trie:
[`src/workers/utils/mpt.ts` lines 435-457](../src/workers/utils/mpt.ts#L435-L457).
That shape is useful for tests and small checks, but production rebuild needs a
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
build temp trie
verify temp root == expected root
stop or prove absence of mutating workers
rename live ledger MPT dir to quarantine/backup path
rename temp dir to configured LEDGER_MPT_DB_PATH
open promoted trie and verify root again
update metadata root and rebuild audit row: complete
```

If promotion fails after the live directory is moved, startup must use the audit
row to either complete the exact same promotion or fail closed. It must never
fall back to genesis.

Promotion must never run concurrently with `runNode`. The implementation should
use a durable maintenance lock plus a process-level file lock on the target MPT
directory. PostgreSQL advisory locking alone is insufficient because LevelDB
state is outside PostgreSQL and can be opened by another process.

## Fail-Closed Startup Behavior

Add a mandatory startup gate before HTTP serving and before background fibers:

```ts
yield* verifyLedgerMptIntegrityOnStartup;
```

This gate should run after schema/migration verification and protocol topology
verification, and before deposit reconciliation, block commitment, merge, or tx
queue processing. In the current `runNode` shape, that places it after
`ensureProtocolInitializedOnStartup` and before `seedLatestLocalBlockBoundaryOnStartup`
or immediately after boundary seeding if the implementation needs the canonical
state-queue tip for root verification:
[`src/commands/listen.ts` lines 69-72](../src/commands/listen.ts#L69-L72).

Startup must produce machine-readable failure reasons such as:

```text
ledger_mpt_empty_with_nonempty_sql
ledger_mpt_root_mismatch
ledger_mpt_metadata_missing_for_nonempty_node
ledger_mpt_rebuild_required
ledger_mpt_rebuild_incomplete
ledger_mpt_leveldb_unreadable
ledger_mpt_genesis_fingerprint_mismatch
mempool_mpt_missing_for_deferred_payload
local_state_metadata_missing_for_existing_node
```

The failure should include:

- configured ledger MPT path;
- observed root or read/open error;
- expected root if known;
- SQL table counts used for classification;
- metadata state if present;
- L1 state-queue reference used for verification;
- recovery command suggestion with required flags.

The block commitment worker should also defensively refuse to proceed if
`makeMpts` returns an unverified ledger trie. Startup is the primary gate; worker
defense prevents future call sites and tests from bypassing it.

`makeMpts` should return handles carrying an integrity token or verified-root
metadata supplied by the startup gate or recovery command. Direct construction of
commit-worker MPT handles without that token should fail in production mode.

## Observability And Readiness

Add MPT integrity state to the readiness model in
[`src/commands/readiness.ts`](../src/commands/readiness.ts). Readiness must be
false when any of these states is active:

- `ledger_mpt_integrity_unknown`
- `ledger_mpt_rebuild_required`
- `ledger_mpt_rebuild_in_progress`
- `ledger_mpt_rebuild_failed`
- `mempool_mpt_missing_for_deferred_payload`
- `local_state_metadata_missing_for_existing_node`

Emit metrics with stable low-cardinality labels:

- `ledger_mpt_integrity_status{status,reason}` as a gauge where `1` is current.
- `ledger_mpt_rebuild_total{source,status}` as a counter.
- `ledger_mpt_rebuild_duration_ms{source,status}` as a timer or histogram.
- `ledger_mpt_root_verification_total{source,status}` as a counter.
- `mempool_mpt_integrity_status{status,reason}` as a gauge.

Structured logs for startup verification, adoption, rebuild dry-run, rebuild
promotion, and fail-closed decisions must include `state_instance_id`,
`ledger_mpt_path`, `mempool_mpt_path`, observed roots, expected roots when
known, source fingerprint, state-queue tip, and recovery/adoption ids. Do not log
seed phrases, wallet secrets, or full transaction payloads.

## Operator Recovery Command Design

Introduce a recovery command, not a runtime fallback:

```text
midgard-node recover ledger-mpt \
  --source confirmed-ledger \
  --expected-root <hex> \
  --reason "<ticket-or-incident-id>" \
  --dry-run

midgard-node recover ledger-mpt \
  --source confirmed-ledger \
  --expected-root <hex> \
  --reason "<ticket-or-incident-id>" \
  --write

midgard-node adopt local-state \
  --expected-ledger-root <hex> \
  --expected-mempool-root <hex-or-empty> \
  --expected-state-queue-tip <hex> \
  --reason "<migration-ticket>" \
  --write
```

Required behavior:

- `--dry-run` computes and reports the source fingerprint and root without
  touching the live MPT directory.
- `--write` requires an expected root and refuses to proceed if the computed root
  differs.
- The command acquires a PostgreSQL advisory lock or equivalent recovery lock so
  only one node or operator can rebuild at a time.
- The command refuses to run while the node is serving mutating traffic unless a
  durable maintenance/recovery mode proves workers are stopped.
- The command creates a rebuild audit row before reading source data.
- The command builds into a temporary directory under the same filesystem as the
  target path so rename promotion is atomic.
- The command preserves the previous live MPT directory under a quarantine path
  named with rebuild id and timestamp.
- The command reopens the promoted trie and verifies root before completing.
- The command updates metadata only after promotion verification.
- Any failure leaves an audit row with status and error details; subsequent
  startup fails closed until the audit row is completed, resumed, or explicitly
  abandoned with evidence.

The one-time `adopt local-state` command is for migrating an existing non-empty
deployment after the metadata tables are introduced. It must:

- require maintenance mode and the same recovery locks as rebuild;
- read the existing live ledger and mempool roots without mutating them;
- verify SQL table counts, L1 topology, configured network/contracts, genesis
  fingerprint, state-queue tip, and expected roots supplied by the operator;
- prove that any local-ahead state is explained by `blocks`, `immutable`,
  `pending_block_finalizations`, `processed_mempool`, and durable deltas;
- write `local_state_metadata` and an adoption audit row only after all checks
  pass.

This is an explicit auditable migration, not a compatibility fallback. Normal
startup must still fail when metadata is missing for a non-empty node.

Do not add `--force-genesis`, `--allow-empty-as-genesis`, or compatibility flags.
If an operator truly needs a new local instance, it should be an explicit reset or
new database/MPT path with clear metadata, not recovery of an existing instance.

## Schema And Metadata Changes

Implement these through the versioned migration system from P0 blocker 5 rather
than ad hoc startup `CREATE TABLE IF NOT EXISTS`.

### `local_state_metadata`

One row per local node state instance:

- `state_instance_id UUID PRIMARY KEY`.
- `network TEXT NOT NULL`.
- `contract_bundle_hash TEXT NOT NULL`.
- `state_queue_policy_id TEXT NOT NULL`.
- `genesis_fingerprint TEXT NOT NULL`.
- `schema_version TEXT NOT NULL`.
- `ledger_mpt_path TEXT NOT NULL`.
- `ledger_mpt_root BYTEA NOT NULL`.
- `ledger_mpt_root_source TEXT NOT NULL`, for example `bootstrap`,
  `block_commit`, `merge_rebuild`, `operator_rebuild`.
- `mempool_mpt_path TEXT NOT NULL`.
- `mempool_mpt_root BYTEA NOT NULL`.
- `mempool_mpt_root_source TEXT NOT NULL`, for example `empty`,
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
then succeeds only if the adopted metadata still matches the live SQL/MPT/L1
state.

### `ledger_mpt_rebuilds`

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
- `mempool_temp_path TEXT`.
- `mempool_quarantine_path TEXT`.
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
`state_instance_id`, expected ledger and mempool roots, observed ledger and
mempool roots, state-queue tip, source fingerprint, operator reason, status,
error fields, and timestamps.

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
  absent, MPT roots are empty/absent, SQL tables are empty, L1 state is empty or
  matching bootstrap state, and bootstrap is explicitly enabled.
- Empty ledger MPT plus non-empty `confirmed_ledger` returns
  `CorruptFailClosed`.
- Empty ledger MPT plus non-empty `blocks`, `immutable`, `deposits_utxos`, or
  active pending journal returns `CorruptFailClosed`.
- Missing metadata plus non-empty SQL returns `CorruptFailClosed`.
- Metadata network/contract/genesis mismatch returns `CorruptFailClosed`.
- Non-empty ledger MPT whose root differs from reconstructed SQL root returns
  `CorruptFailClosed`.
- Rebuild root construction is deterministic under randomized SQL row order.
- Rebuild of deferred transaction roots is deterministic under equal timestamps
  by sorting on `tx_id` as the tie-breaker.
- Rebuild rejects duplicate outrefs and conflicting output bytes.
- Rebuild rejects missing `mempool_tx_deltas` for any transaction that must be
  replayed into the ledger root.
- `makeMpts` no longer inserts genesis UTxOs on empty root.
- `Genesis.program` fails on partial genesis seeding and does not suppress
  non-identical duplicate rows.

### Integration Tests

- Fresh non-mainnet bootstrap creates metadata, seeds genesis only through the
  explicit bootstrap path, and verifies the resulting root.
- Existing non-empty SQL/MPT without metadata fails startup until `adopt
  local-state --write` verifies the tuple and writes metadata.
- Restart after bootstrap with intact SQL/MPT/metadata is healthy and does not
  reseed.
- Delete the ledger MPT directory after creating non-empty SQL state; startup
  fails before HTTP and workers start.
- Truncate or corrupt the LevelDB ledger store; startup fails with
  `ledger_mpt_leveldb_unreadable` or `ledger_mpt_root_mismatch`.
- Insert a conflicting `confirmed_ledger` row; rebuild dry-run fails before temp
  trie promotion.
- Rebuild dry-run from verified `confirmed_ledger` computes the expected root and
  leaves the live directory unchanged.
- Rebuild write from verified `confirmed_ledger` promotes a temp trie, reopens it,
  updates metadata, and allows the next startup.
- Rebuild from `latest-local-replay` reconstructs ledger and mempool roots when
  `processed_mempool` contains deferred payload and refuses if any tx delta is
  missing.
- Crash/fault after temp build but before promotion: startup reports
  `ledger_mpt_rebuild_incomplete`.
- Crash/fault after live directory quarantine but before temp promotion: recovery
  resumes or fails closed without reseeding genesis.
- Crash/fault after promotion but before metadata update: startup verifies the
  promoted root against the rebuild audit row and either completes metadata update
  or fails closed.
- `GET /init` and `RUN_GENESIS_ON_STARTUP` both use the same strict bootstrap
  path and cannot run genesis seeding after metadata exists.
- `resetDatabases` creates an auditable reset/new-instance boundary before
  deleting ledger MPT and SQL state, or is blocked until reset semantics are made
  production-safe.

### Fault Injection Points

Add test-only injection hooks around:

- after classifier reads metadata;
- after source SQL rows are read;
- after temp trie root is computed;
- after temp trie close/fsync;
- after live directory is moved to quarantine;
- after temp directory is promoted;
- after metadata update;
- during LevelDB open/root read.

These hooks must be test-only and removed from default behavior or gated behind
explicit test configuration.

## Rollout Steps

1. Add versioned migrations for `local_state_metadata`,
   `ledger_mpt_rebuilds`, and adoption/recovery audit metadata.
2. Add strict SQL inspection helpers for table counts, ordered ledger reads, and
   source fingerprinting.
3. Implement the ledger state classifier with tests for bootstrap, restart,
   rebuild-needed, and corruption cases.
4. Remove genesis seeding from `makeMpts`; make it open and return verified MPT
   handles only.
5. Move genesis MPT seeding into the explicit bootstrap path, make it strict and
   ordered, and require metadata creation plus root verification.
6. Add `verifyLedgerMptIntegrityOnStartup` to the mandatory startup gate before
   serving traffic or starting mutating workers.
7. Add defensive worker checks so block commitment cannot use an unverified
   ledger trie.
8. Implement `adopt local-state --dry-run/--write` for existing deployments and
   require it before any non-empty node without metadata can start.
9. Implement rebuild dry-run command from verified `confirmed_ledger`.
10. Implement rebuild write command with temp directory, expected-root check,
   quarantine, promotion, metadata update, and audit rows.
11. Implement `latest-local-replay` only after durable replay evidence is
    complete; until then, fail closed for roots ahead of confirmed state.
12. Add recovery/resume handling for incomplete rebuild audit rows.
13. Add readiness input and metrics for MPT integrity/rebuild state.
14. Run unit tests, integration tests, and fault-injection tests.
15. Exercise an emulator flow: bootstrap, deposit, commit, merge, stop, delete
   ledger MPT, verify startup fail-closed, rebuild, restart, and build the next
   block.
16. Document operator runbook commands, expected diagnostics, and evidence to
   preserve.
17. Deploy first to disposable staging state, then to persistent staging with a
   copied database/MPT pair, then to production only after rollback/new-instance
   procedures are documented.

## Risks And Constraints

- The persistent `ledger` MPT role is defined in this plan as the latest local
  block-commitment UTxO root. If implementation discovers code paths that use it
  as a different root, those paths must be reconciled before approval; do not add
  a mode switch.
- `confirmed_ledger` is only authoritative after the merge confirmation blocker is
  fixed. If local merge finalization can still run without L1 confirmation, the
  rebuild command must refuse to trust `confirmed_ledger`.
- The current schema may not retain enough immutable per-transaction delta data
  to replay from confirmed state to latest local state. If not, either add
  durable deltas before supporting replay rebuilds or limit rebuild to
  confirmed-only states.
- L1 provider APIs must expose enough state-queue and confirmed-state evidence to
  verify expected roots. If not, add a local durable proof/journal before relying
  on rebuild.
- Atomic directory promotion is filesystem-dependent. The implementation should
  require temp and live paths on the same filesystem and fail otherwise.
- `resetDatabases` currently deletes SQL and MPT state together. Production reset
  semantics need a separate explicit plan so reset cannot look like corruption or
  bootstrap on the next start.
- Mainnet genesis configuration is empty, but that alone is not a safety proof.
  Mainnet must still reject empty-root recovery unless all fresh-bootstrap
  conditions are impossible or explicitly disabled.
- Existing tests may assume `makeMpts` seeds genesis. Those tests should be
  updated to call explicit bootstrap helpers, not preserved through compatibility
  behavior.
- Existing deployments require an explicit adoption window. This is operational
  migration work, but it is safer than auto-generating metadata from ambiguous
  state at startup.

## Concrete Checklist

- [ ] Define and document the semantic owner of the persistent ledger and mempool
      MPT roots as the latest local commit-pipeline roots.
- [ ] Add migrations for `local_state_metadata` and `ledger_mpt_rebuilds`.
- [ ] Add adoption/recovery audit metadata for existing non-empty deployments.
- [ ] Store network, contracts, genesis fingerprint, root, and source metadata on
      fresh bootstrap.
- [ ] Store mempool MPT root/source and active pending-finalization identifiers
      in metadata.
- [ ] Implement SQL table-count, ordered ledger source, and ordered tx source
      inspection helpers.
- [ ] Implement deterministic root/source fingerprint computation from ledger
      rows.
- [ ] Implement deterministic tx-root and replay-root computation from durable tx
      bytes and deltas.
- [ ] Implement `classifyLocalLedgerState`.
- [ ] Remove empty-root genesis seeding from `makeMpts`.
- [ ] Add explicit genesis MPT seeding only to proven fresh bootstrap.
- [ ] Make `Genesis.program`, startup bootstrap, and `GET /init` share one strict
      bootstrap path.
- [ ] Add `adopt local-state --dry-run/--write` for explicit metadata migration.
- [ ] Add mandatory startup MPT integrity verification.
- [ ] Ensure startup fails before HTTP server and mutating workers on unknown,
      corrupt, mismatched, or rebuild-required ledger MPT state.
- [ ] Add defensive block-commitment worker check for verified MPT state.
- [ ] Design and implement `recover ledger-mpt --dry-run`.
- [ ] Design and implement `recover ledger-mpt --write`.
- [ ] Add `latest-local-replay` only when durable replay evidence is complete;
      otherwise fail closed for local-ahead roots.
- [ ] Add temp-dir build, root verification, quarantine, atomic promotion, and
      reopen verification.
- [ ] Add resume/fail-closed handling for incomplete rebuild audit rows.
- [ ] Add readiness reasons and metrics for MPT integrity and rebuild state.
- [ ] Add unit tests for classifier, root construction, metadata mismatch, and
      empty-root corruption.
- [ ] Add integration tests for fresh bootstrap, healthy restart, missing MPT,
      corrupt MPT, dry-run rebuild, write rebuild, and post-rebuild restart.
- [ ] Add fault-injection tests for rebuild crash points.
- [ ] Update operator runbook with diagnostics, required evidence, and recovery
      commands.
- [ ] Verify no path silently reseeds genesis after metadata or SQL state exists.
