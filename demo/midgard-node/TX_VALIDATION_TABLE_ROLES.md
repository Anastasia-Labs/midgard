# Tx-Validation Table Roles

This document describes the tables that exist on the current `tx-validation`
branch but were not present on `staging`.

The goal is to make their current roles explicit for developers working on
the node state machine. This is descriptive documentation for the current
implementation. Where a table is only partially wired, or where production
readiness plans identify missing invariants, those gaps are called out
separately.

## Scope

Covered tables:

- `blocks`
- `processed_mempool`
- `mempool_tx_deltas`
- `tx_rejections`
- `deposit_ingestion_cursor`
- `pending_block_finalizations`
- `pending_block_finalization_deposits`
- `pending_block_finalization_txs`
- `tx_admissions`
- `local_mutation_jobs`
- `schema_migrations`
- `schema_migration_events`

## High-Level Model

These tables mostly support four changes from the staging model:

- durable transaction admission and validation tracking;
- durable pending block submission and local-finalization recovery;
- explicit migration metadata and fail-closed schema verification;
- local mutation accountability after L1 submission or merge side effects.

The current branch no longer relies on startup-time `CREATE TABLE IF NOT
EXISTS` as the production schema path. The long-running node verifies that
explicit migrations have already installed the exact expected schema
([init.ts](src/database/init.ts#L6)). The migration manifest lists the
application tables and indexes that must exist
([migrations/index.ts](src/database/migrations/index.ts#L51)).

The initial schema creates most of the application tables
([0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L11)).
Durable transaction admissions are added in migration 2
([0002_durable_tx_admissions.sql](src/database/migrations/sql/0002_durable_tx_admissions.sql#L13)).
Local mutation jobs are added in migration 3
([0003_local_mutation_jobs.sql](src/database/migrations/sql/0003_local_mutation_jobs.sql#L1)).
The migration runner creates `schema_migrations` and
`schema_migration_events` as metadata tables
([runner.ts](src/database/migrations/runner.ts#L64)).

## Table Map

| Table | Primary role |
| --- | --- |
| `blocks` | Local index from submitted state-queue header hash to included L2 tx ids. |
| `processed_mempool` | Durable holding area for tx payloads already incorporated into the mempool MPT while waiting for a later submit/finalization path. |
| `mempool_tx_deltas` | Per-tx spent/produced delta cache used to build MPT roots from accepted mempool txs. |
| `tx_rejections` | Durable rejection evidence for validation, malformed preprocessing, and tx-status responses. |
| `deposit_ingestion_cursor` | Intended durable stable-L1 deposit scan cursor. Currently schema and adapter exist, but production ingestion does not use it. |
| `pending_block_finalizations` | Single-active journal for submitted or potentially submitted block finalization and recovery. |
| `pending_block_finalization_deposits` | Ordered deposit-event membership for a pending block journal. |
| `pending_block_finalization_txs` | Ordered tx-id membership for a pending block journal. |
| `tx_admissions` | Durable `/submit` admission queue and validation state ledger. |
| `local_mutation_jobs` | Durable record of local side-effect jobs that must not be silently lost after L1 submission or merge. |
| `schema_migrations` | Ledger of successfully applied schema migrations. |
| `schema_migration_events` | Audit trail of migration attempts and outcomes. |

## `blocks`

### Purpose

`blocks` is the local membership index for L2 transactions included in a
submitted state-queue block. It links a Midgard block header hash to the L2
transaction ids that were locally finalized for that header.

The merge flow uses this table to find the tx payloads for the oldest queued
state-queue block before updating `confirmed_ledger`
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts#L521)).
After the merge side effects update `confirmed_ledger`, the block links for
that header are removed
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts#L1475)).

### Stored Information

Schema:

- `height SERIAL PRIMARY KEY`
- `header_hash BYTEA NOT NULL`
- `tx_id BYTEA NOT NULL UNIQUE`
- `time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Indexes:

- `idx_blocks_header_hash` on `header_hash`
- `idx_blocks_tx_id` on `tx_id`

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L11).

The database adapter rejects inserting a tx id under a different header if the
tx id is already present
([blocks.ts](src/database/blocks.ts#L33)). Inserts de-duplicate input tx ids
and use `ON CONFLICT (tx_id) DO NOTHING` only after checking that an existing
row is linked to the same header
([blocks.ts](src/database/blocks.ts#L89)).

### Writers

`blocks` is written during local commit finalization, in the same SQL
transaction that inserts accepted txs into `immutable` and clears the
corresponding `mempool` rows
([commit-submission.ts](src/workers/utils/commit-submission.ts#L157),
[commit-submission.ts](src/workers/utils/commit-submission.ts#L177)).

### Readers

Readers include:

- merge replay, through `fetchFirstBlockTxs`
  ([transactions/utils.ts](src/transactions/utils.ts#L472));
- the listen router block lookup path
  ([listen-router.ts](src/commands/listen-router.ts#L555));
- the `audit-blocks-immutable` command, which validates `blocks` to
  `immutable` linkage
  ([audit-blocks-immutable.ts](src/commands/audit-blocks-immutable.ts#L59)).

### Lifecycle

1. A commit transaction is submitted or recovered.
2. Local block finalization inserts included tx payloads into `immutable`.
3. The same finalization path inserts `(header_hash, tx_id)` links into
   `blocks`.
4. The merge worker later fetches the first queued block's tx ids from
   `blocks`, fetches their payloads from `immutable`, and applies their effects
   to `confirmed_ledger`.
5. After successful local merge finalization, rows for that `header_hash` are
   removed.

### Relationships

- `blocks.tx_id` should correspond to an `immutable.tx_id`.
- `blocks.header_hash` corresponds to a state-queue block header hash.
- `blocks` is the SQL bridge from the L1 state queue to local confirmed-state
  replay.

### Invariants And Gaps

Expected invariants:

- each row should have exactly one matching immutable tx payload;
- the same tx id must not be linked to two different headers;
- after a merge finalizes locally, rows for the merged header should be gone.

Known gaps:

- `blocks` has no per-header `ordinal`. `height` is insertion order, not an
  explicit block-local ordering primitive.
- `retrieveTxHashesByHeaderHash` has no `ORDER BY`
  ([blocks.ts](src/database/blocks.ts#L123)), so block tx order is not
  durably represented by this table.
- Production readiness notes call out the need for explicit ordinals and
  deterministic retrieval
  ([02-atomic-recoverable-ledger-mutations.md](production-readiness-plans/02-atomic-recoverable-ledger-mutations.md#L972)).

## `processed_mempool`

### Purpose

`processed_mempool` stores tx CBOR for accepted txs that have already been
processed into the persistent mempool MPT, but cannot yet be submitted or
locally finalized because the previous state-queue block is still awaiting the
right confirmation/finalization boundary.

It is a deferred payload holding table. It is not the live admission pre-state;
that role belongs to `mempool` and `mempool_ledger`.

### Stored Information

Schema:

- `tx_id BYTEA PRIMARY KEY`
- `tx BYTEA NOT NULL`
- `time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Index:

- `idx_processed_mempool_time_stamp_tz` on `time_stamp_tz`

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L95).
The adapter is a thin wrapper around the generic tx table helpers
([processedMempool.ts](src/database/processedMempool.ts#L6)).

### Writers

`processed_mempool` is written by `skippedSubmissionProgram`, which transfers
processed txs from `mempool` when a commit cannot yet be submitted because no
confirmed predecessor block is available
([commit-submission.ts](src/workers/utils/commit-submission.ts#L289)).

The main commit worker reaches this path after `processMpts` has already
applied the txs to the persistent mempool MPT and discovered that
`availableConfirmedBlock === ""`
([commit-block-header.ts](src/workers/commit-block-header.ts#L922),
[commit-block-header.ts](src/workers/commit-block-header.ts#L958)).

### Readers

Readers include:

- `establishEndTimeFromTxRequests`, which uses the oldest deferred payload when
  there are no live `mempool` rows
  ([commit-block-header.ts](src/workers/commit-block-header.ts#L203));
- local block finalization, which folds deferred payloads into successful
  commit batches
  ([commit-submission.ts](src/workers/utils/commit-submission.ts#L148));
- `/tx-status`, which reports whether a tx is in processed mempool
  ([listen-router.ts](src/commands/listen-router.ts#L355)).

### Lifecycle

1. Accepted txs enter `mempool` and are reflected in `mempool_ledger`.
2. The commitment worker processes them into the persistent mempool MPT.
3. If the node cannot submit a new block yet, the tx rows are moved from
   `mempool` into `processed_mempool`.
4. On a later successful submission/local finalization path, deferred rows are
   combined with current mempool rows and inserted into `immutable`/`blocks`.
5. `processed_mempool` is cleared after local finalization
   ([commit-submission.ts](src/workers/utils/commit-submission.ts#L186)).

### Relationships

- Related to the persistent mempool MPT root. The MPT contains the transaction
  root effects, while `processed_mempool` stores SQL-visible tx payloads needed
  for later local finalization.
- Related to `mempool`: rows are moved from live mempool to processed mempool.
- Related to `immutable` and `blocks`: deferred rows eventually become
  immutable tx payloads and block membership rows.

### Invariants And Gaps

Expected invariants:

- non-empty `processed_mempool` means there is deferred local state that must be
  reconciled before treating the node as clean;
- the persistent mempool MPT root should be consistent with the deferred
  payloads.

Known gaps:

- There is no per-header or root fingerprint in `processed_mempool`.
- The finalization path clears the whole table rather than only planned tx ids.
- Readiness plans call out the need to compare persistent mempool trie state
  with `processed_mempool`
  ([04-startup-fail-closed-integrity.md](production-readiness-plans/04-startup-fail-closed-integrity.md#L592)).

## `mempool_tx_deltas`

### Purpose

`mempool_tx_deltas` stores the spent inputs and produced outputs for accepted
mempool transactions. It lets the commitment worker construct MPT roots from
already-validated tx effects instead of re-decoding every tx from CBOR.

### Stored Information

Schema:

- `tx_id BYTEA PRIMARY KEY`
- `spent_cbor BYTEA NOT NULL`
- `produced_cbor BYTEA NOT NULL`

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L127).

`spent_cbor` encodes an array of outref bytes. `produced_cbor` encodes an array
of `[outref, output]` byte pairs. The adapter decodes CBOR strictly and rejects
trailing bytes, undefined values, indefinite encoding, and duplicate map keys
([mempoolTxDeltas.ts](src/database/mempoolTxDeltas.ts#L12),
[mempoolTxDeltas.ts](src/database/mempoolTxDeltas.ts#L40)).

### Writers

`MempoolDB.insert` and `MempoolDB.insertMultiple` write deltas in the same SQL
transaction that inserts the tx into `mempool`, updates `mempool_ledger`, marks
consumed deposits, and writes address history
([mempool.ts](src/database/mempool.ts#L29),
[mempool.ts](src/database/mempool.ts#L61)).

### Readers

`processMpts` reads deltas for the mempool tx ids it is about to include in an
MPT build
([mpt.ts](src/workers/utils/mpt.ts#L297)). If a delta is missing or invalid,
the commit path has to fall back to resolving the tx effect, or reject the tx
depending on the decoding outcome
([mpt.ts](src/workers/utils/mpt.ts#L307)).

### Lifecycle

1. Validation accepts a tx and produces its spent/produced effect.
2. The tx is inserted into `mempool`.
3. Its delta is upserted into `mempool_tx_deltas`.
4. The commitment worker reads the delta to apply MPT batch operations.
5. When txs are cleared from `mempool`, their deltas are also cleared
   ([mempool.ts](src/database/mempool.ts#L138)).
6. A full `mempool` clear also clears all deltas
   ([mempool.ts](src/database/mempool.ts#L146)).

### Relationships

- `mempool_tx_deltas.tx_id` should correspond to a tx lifecycle owner:
  `mempool`, a deferred processed state, or an immutable/committed state.
- It is directly related to the persistent ledger and mempool MPT roots because
  its entries drive MPT batch operations.

### Invariants And Gaps

Expected invariants:

- a delta should never exist without an explainable tx lifecycle owner;
- decoded deltas must match the transaction payload originally validated;
- spent and produced sets must be deterministic and canonical for MPT root
  computation.

Known gaps:

- There is no foreign key to `mempool`, `processed_mempool`, or `immutable`.
- When a tx is transferred into `processed_mempool`, the delta is cleared
  because the MPT already contains the tx effects. That leaves no SQL delta
  evidence for replay unless it can be reconstructed from payloads.
- Production readiness plans classify orphan deltas as ambiguous/corrupt state
  ([06-empty-ledger-mpt-authority-revision-plan.md](production-readiness-plans/06-empty-ledger-mpt-authority-revision-plan.md#L337)).

## `tx_rejections`

### Purpose

`tx_rejections` stores durable evidence that a tx was rejected. It supports
validation diagnostics, malformed tx handling during commitment preprocessing,
and `/tx-status` responses.

### Stored Information

Schema from migration 1:

- `tx_id BYTEA NOT NULL`
- `reject_code TEXT NOT NULL`
- `reject_detail TEXT`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Indexes:

- `idx_tx_rejections_tx_id`
- `idx_tx_rejections_created_at`

Migration 2 adds a unique index on `tx_id`
([0002_durable_tx_admissions.sql](src/database/migrations/sql/0002_durable_tx_admissions.sql#L75)).

Defined initially in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L134).
The adapter supports insert, insertMany, lookup by tx id, pruning, and clear
([txRejections.ts](src/database/txRejections.ts#L52)).

### Writers

Writers include:

- `TxAdmissionsDB.markRejected`, which writes `tx_rejections` and transitions
  `tx_admissions` in one transaction
  ([txAdmissions.ts](src/database/txAdmissions.ts#L371));
- commitment preprocessing for malformed mempool txs
  ([mpt.ts](src/workers/utils/mpt.ts#L399));
- local submit tooling that records immediate local rejection evidence
  ([submit-l2-transfer.ts](src/commands/submit-l2-transfer.ts#L888)).

### Readers

`/tx-status` reads `tx_rejections` first to report rejection metadata
([listen-router.ts](src/commands/listen-router.ts#L355)).

The retention sweeper prunes old rejection rows
([retention-sweeper.ts](src/fibers/retention-sweeper.ts#L26)).

### Lifecycle

1. A tx is rejected during durable validation, malformed preprocessing, or local
   submit handling.
2. A row records `tx_id`, a stable reject code, optional detail, and creation
   time.
3. `/tx-status` exposes that evidence.
4. Retention may prune old rows according to the retention policy.

### Relationships

- Usually paired with `tx_admissions.status='rejected'`.
- Must not contradict terminal accepted state in `immutable` or live accepted
  state in `mempool`.

### Invariants And Gaps

Expected invariants:

- rejection evidence should be deterministic for a tx id;
- a rejected admission row should have matching rejection metadata;
- accepted/immutable txs should not also look rejected.

Known gaps:

- `TxAdmissionsDB.markRejected` has stricter idempotent conflict semantics than
  direct `TxRejectionsDB.insertMany` callers. Direct callers can hit uniqueness
  conflicts unless their call path already prevents duplicates.
- Startup readiness plans call contradictory rejection state an integrity
  problem
  ([04-startup-fail-closed-integrity.md](production-readiness-plans/04-startup-fail-closed-integrity.md#L516)).

## `deposit_ingestion_cursor`

### Purpose

`deposit_ingestion_cursor` is intended to be the durable cursor for stable L1
deposit discovery. It records which stable L1 view and scan upper bound were
used, so deposit ingestion can be audited and resumed deterministically.

Current implementation note: the table and adapter exist, but the production
deposit fetch path does not currently read or advance this cursor. Deposit
ingestion reconciles visible deposits directly
([fetch-and-insert-deposit-utxos.ts](src/fibers/fetch-and-insert-deposit-utxos.ts#L213)).

### Stored Information

Schema:

- `cursor_name TEXT PRIMARY KEY`
- `stable_tip_hash TEXT NOT NULL`
- `stable_tip_slot BIGINT NOT NULL`
- `stable_tip_time_ms BIGINT NOT NULL`
- `scan_upper_bound_time_ms BIGINT NOT NULL`
- `last_scanned_event_id BYTEA NOT NULL`
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L147).
The adapter defines the default cursor name `stable_l1_deposits`, supports
lookup, and upserts cursor advancement
([depositIngestionCursor.ts](src/database/depositIngestionCursor.ts#L10),
[depositIngestionCursor.ts](src/database/depositIngestionCursor.ts#L72)).

### Writers

No production writer was found in the current branch. The `advance` function is
available but not called by the deposit fetch fiber or commit-time deposit
barrier.

### Readers

No production reader was found in the current branch.

### Lifecycle

Intended lifecycle:

1. Resolve a stable L1 view for deposits.
2. Fetch deposits up to a stable scan bound.
3. Insert or reconcile deposit rows.
4. Advance the cursor in the same auditable operation.
5. Use the cursor for startup/recovery/readiness checks.

Current lifecycle:

1. The fetcher reconciles visible deposit UTxOs into `deposits_utxos`.
2. The commit-time barrier fetches visible deposits up to the requested upper
   bound and returns that upper bound
   ([fetch-and-insert-deposit-utxos.ts](src/fibers/fetch-and-insert-deposit-utxos.ts#L233)).
3. No cursor row is advanced.

### Relationships

- Intended to relate stable L1 deposit discovery to `deposits_utxos`.
- Should eventually support deposit ingestion readiness and fail-closed startup.

### Invariants And Gaps

Expected future invariants:

- cursor advancement should be monotonic;
- cursor advancement should happen only after deposits for that stable view have
  been inserted or byte-compared;
- startup should verify the cursor's stable L1 evidence if present.

Known gaps:

- Currently unused by production code.
- Existing deposit projection docs describe the desired cursor-driven model, but
  implementation is not fully wired
  ([exact-once-deposit-projection.md](docs/exact-once-deposit-projection.md#L35)).
- Production readiness plans call out missing cursor persistence and stable L1
  verification
  ([04-startup-fail-closed-integrity.md](production-readiness-plans/04-startup-fail-closed-integrity.md#L340)).

## `pending_block_finalizations`

### Purpose

`pending_block_finalizations` is the durable journal for a block that has been
prepared, submitted, observed, finalized, or abandoned. It is the main SQL
state machine tying together:

- local block construction;
- L1 submission and confirmation;
- local SQL/MPT finalization;
- crash recovery after submission or confirmation.

At most one active pending-finalization record may exist at a time
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L41)).

### Stored Information

Schema:

- `header_hash BYTEA PRIMARY KEY`
- `submitted_tx_hash BYTEA UNIQUE`
- `block_end_time TIMESTAMPTZ NOT NULL`
- `status TEXT NOT NULL`
- `observed_confirmed_at_ms BIGINT`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Allowed statuses:

- `pending_submission`
- `submitted_local_finalization_pending`
- `submitted_unconfirmed`
- `observed_waiting_stability`
- `finalized`
- `abandoned`

Indexes:

- partial unique index `uniq_pending_block_finalizations_single_active` for
  active statuses;
- `idx_pending_block_finalizations_status`.

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L157).

### Writers And Transitions

The adapter owns the status transitions:

- `preparePendingSubmission`: creates a pending record before submission and
  inserts ordered deposit/tx members
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L226));
- `markSubmitted`: `pending_submission` to
  `submitted_local_finalization_pending`
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L294));
- `markLocalFinalizationComplete`:
  `submitted_local_finalization_pending` to `submitted_unconfirmed`
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L324));
- `markObservedWaitingStability`: active states to
  `observed_waiting_stability`
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L356));
- `reviveAbandonedCanonical`: abandoned to observed when L1 proves it canonical
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L399));
- `markFinalized`: submitted/observed to `finalized`
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L432));
- `markAbandoned`: active states to `abandoned`
  ([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L463)).

Commit construction prepares the journal before signing/submission
([commit-block-header.ts](src/workers/commit-block-header.ts#L537),
[commit-block-header.ts](src/workers/commit-block-header.ts#L641)).
Submission success marks it submitted
([commit-block-header.ts](src/workers/commit-block-header.ts#L721)).
Local finalization marks it locally complete
([commit-submission.ts](src/workers/utils/commit-submission.ts#L232)).
Confirmation observes, finalizes, or abandons it
([block-confirmation.ts](src/fibers/block-confirmation.ts#L90)).

### Readers

Readers include:

- confirmation, which passes the active pending block to the worker
  ([block-confirmation.ts](src/fibers/block-confirmation.ts#L198));
- startup, which hydrates active recovery state and revives canonical abandoned
  rows where needed
  ([listen-startup.ts](src/commands/listen-startup.ts#L95));
- readiness, indirectly through local-finalization globals and active recovery
  state.

### Lifecycle

Typical successful tx-backed path:

1. `preparePendingSubmission` records `header_hash`, `block_end_time`, and
   members.
2. Submit succeeds or is recovered from L1.
3. `markSubmitted` stores `submitted_tx_hash`.
4. Local finalization inserts `immutable`/`blocks`, clears relevant mempool
   state, marks deposits projected by header, and calls
   `markLocalFinalizationComplete`.
5. Confirmation observes the header on the L1 state queue.
6. The journal moves to `observed_waiting_stability` or `finalized`, depending
   on whether local finalization still requires recovery.
7. A stale or non-canonical submission can be marked `abandoned`.

### Relationships

- Parent table for `pending_block_finalization_deposits`.
- Parent table for `pending_block_finalization_txs`.
- References L1 state queue through `header_hash` and `submitted_tx_hash`.
- Coordinates local SQL state in `immutable`, `blocks`, `deposits_utxos`,
  `mempool`, and `processed_mempool`.

### Invariants And Gaps

Expected invariants:

- only one active pending-finalization record can exist;
- status transitions must be monotonic and explicit;
- confirmation must match the persisted journal header before treating a block
  as resolved;
- abandoned deposit assignments must be cleared only for the matching header.

Known gaps:

- The journal is a partial lifecycle record, not a complete mutation plan.
  Production readiness notes call for richer fingerprints and replay/adoption
  evidence
  ([02-atomic-recoverable-ledger-mutations.md](production-readiness-plans/02-atomic-recoverable-ledger-mutations.md#L175)).
- It does not by itself prove MPT/SQL root consistency.

## `pending_block_finalization_deposits`

### Purpose

`pending_block_finalization_deposits` stores the deposit event ids included in a
pending block. It gives the pending-finalization journal explicit deposit
membership and ordering.

### Stored Information

Schema:

- `header_hash BYTEA NOT NULL`
- `member_id BYTEA NOT NULL`
- `ordinal INTEGER NOT NULL`
- primary key `(header_hash, member_id)`
- unique `(header_hash, ordinal)`

Foreign keys:

- `header_hash` references `pending_block_finalizations(header_hash)` with
  `ON DELETE CASCADE`;
- `member_id` references `deposits_utxos(event_id)` with `ON DELETE RESTRICT`.

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L175).

### Writers

Rows are inserted by `preparePendingSubmission` from `depositEventIds`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L266)).

### Readers

Rows are read through `retrieveRecord`, which loads member ids ordered by
`ordinal`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L75)).

Deposit retention checks this table to avoid deleting deposit rows that are
still referenced by pending-finalization membership
([deposits.ts](src/database/deposits.ts#L637)).

### Lifecycle

1. A block is prepared with a deposit set.
2. Member ids are inserted with deterministic ordinals.
3. Confirmation, startup, and recovery can inspect the deposit membership.
4. Rows remain with the parent journal. Deleting the parent cascades deletion of
   members, but normal terminal status changes do not delete them.

### Relationships

- Child of `pending_block_finalizations`.
- References `deposits_utxos`.
- Used by deposit assignment and abandonment recovery.

### Invariants And Gaps

Expected invariants:

- each deposit event appears at most once per header;
- ordinals are unique per header;
- referenced deposits cannot be pruned while membership exists.

Known gaps:

- Deposit-only blocks may have deposit members but no `blocks` rows, so terminal
  retention cannot infer deposit lifecycle from `blocks`.
- Retention planning explicitly calls out active pending-finalization membership
  as a pruning blocker
  ([08-state-aware-retention.md](production-readiness-plans/08-state-aware-retention.md#L695)).

## `pending_block_finalization_txs`

### Purpose

`pending_block_finalization_txs` stores tx ids included in a pending block
journal. It is membership evidence for recovery and confirmation paths.

### Stored Information

Schema:

- `header_hash BYTEA NOT NULL`
- `member_id BYTEA NOT NULL`
- `ordinal INTEGER NOT NULL`
- primary key `(header_hash, member_id)`
- unique `(header_hash, ordinal)`

Foreign key:

- `header_hash` references `pending_block_finalizations(header_hash)` with
  `ON DELETE CASCADE`.

There is intentionally no database foreign key to `mempool`,
`processed_mempool`, or `immutable`.

Defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L183).

### Writers

Rows are inserted by `preparePendingSubmission` from `mempoolTxIds`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L275)).
Tx-backed commit preparation passes ids from `processedMempoolTxs`
([commit-block-header.ts](src/workers/commit-block-header.ts#L641)).

### Readers

Rows are read through `retrieveRecord`, ordered by `ordinal`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts#L75)).

### Lifecycle

1. A tx-backed commit is prepared.
2. Tx ids planned for the block are inserted as ordered member rows.
3. Confirmation and startup can use the parent record and members to decide
   whether local finalization recovery is required.
4. Rows remain with the parent journal.

### Relationships

- Child of `pending_block_finalizations`.
- Semantically related to `mempool`, `processed_mempool`, `immutable`, and
  `blocks`, but not enforced by foreign keys.

### Invariants And Gaps

Expected invariants:

- each tx id appears at most once per header;
- ordinals are unique per header;
- member rows should match the block payload that local finalization inserts
  into `immutable`/`blocks`.

Known gaps:

- No FK to the tx payload's current lifecycle table.
- Already-deferred `processed_mempool` rows can be part of the payload source
  without being a complete replay plan in this table.
- The current `blocks` table lacks ordinals, so this ordered membership is not
  mirrored in the eventual merge membership table.

## `tx_admissions`

### Purpose

`tx_admissions` is the durable `/submit` admission queue and validation status
ledger. It replaced the older in-memory-only submission queue for production
reliability.

It answers questions such as:

- was this tx submitted before;
- are the submitted bytes identical to the previous request;
- is the tx queued, validating, accepted, or rejected;
- who currently owns the validation lease;
- when should validation retry.

### Stored Information

Migration 2 creates enum `tx_admission_status` and table `tx_admissions`
([0002_durable_tx_admissions.sql](src/database/migrations/sql/0002_durable_tx_admissions.sql#L13)).

Columns:

- `tx_id BYTEA PRIMARY KEY CHECK (octet_length(tx_id) = 32)`
- `tx_cbor BYTEA NOT NULL`
- `tx_cbor_sha256 BYTEA NOT NULL`
- `arrival_seq BIGSERIAL UNIQUE NOT NULL`
- `status tx_admission_status NOT NULL`
- `first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `last_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `validation_started_at TIMESTAMPTZ`
- `terminal_at TIMESTAMPTZ`
- `lease_owner TEXT`
- `lease_expires_at TIMESTAMPTZ`
- `attempt_count INTEGER NOT NULL DEFAULT 0`
- `next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `reject_code TEXT`
- `reject_detail TEXT`
- `submit_source TEXT NOT NULL`
- `request_count BIGINT NOT NULL DEFAULT 1`

Statuses:

- `queued`
- `validating`
- `accepted`
- `rejected`

Submit sources:

- `native`
- `cardano-converted`
- `backfill`

Indexes:

- `idx_tx_admissions_dequeue` on `(next_attempt_at, arrival_seq)` for queued
  or validating rows;
- `idx_tx_admissions_status_updated` on `(status, updated_at)`;
- `idx_tx_admissions_lease` on `lease_expires_at` for validating rows.

Important check constraints enforce lease/status consistency, terminal status
timestamps, and rejection metadata consistency
([0002_durable_tx_admissions.sql](src/database/migrations/sql/0002_durable_tx_admissions.sql#L20)).

### Writers

`/submit` writes admissions through `TxAdmissionsDB.admit`
([listen-router.ts](src/commands/listen-router.ts#L1056)).
`admit` is idempotent for the same tx id and same normalized bytes, increments
`request_count` on duplicates, and rejects same tx id with different bytes
([txAdmissions.ts](src/database/txAdmissions.ts#L127)).

The tx queue processor:

- requeues expired leases
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L368));
- claims ordered batches with a lease
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L412));
- marks accepted txs and writes them to `mempool`
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L522),
  [txAdmissions.ts](src/database/txAdmissions.ts#L325));
- marks rejected txs and writes `tx_rejections`
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L506),
  [txAdmissions.ts](src/database/txAdmissions.ts#L371));
- releases leased rows for retry after processor failure
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L549)).

### Readers

Readers include:

- queue processor backlog and oldest queued age metrics
  ([txAdmissions.ts](src/database/txAdmissions.ts#L456));
- readiness, which reports durable admission backlog and oldest age
  ([listen-router.ts](src/commands/listen-router.ts#L455));
- `/tx-status`, which includes admission status
  ([listen-router.ts](src/commands/listen-router.ts#L356)).

### Lifecycle

1. `/submit` inserts a row with `status='queued'`.
2. The queue processor claims a batch, moving rows to `validating` under a lease.
3. Phase A and Phase B validation run.
4. Accepted rows transition to `accepted`, and their txs are inserted into
   `mempool`.
5. Rejected rows transition to `rejected`, with rejection metadata also written
   to `tx_rejections`.
6. If validation infrastructure fails, rows are released back to `queued` with
   retry backoff.
7. Accepted/rejected rows are terminal admission history.

### Relationships

- Accepted rows should correspond to txs in `mempool`, `processed_mempool`,
  `immutable`, or later committed history depending on lifecycle stage.
- Rejected rows should correspond to `tx_rejections`.
- Readiness uses `tx_admissions`, not the legacy in-memory Effect queue, as the
  durable backlog source.

### Invariants And Gaps

Expected invariants:

- same `tx_id` with different normalized bytes is a hard conflict;
- only validating rows can have leases;
- only terminal rows can have `terminal_at`;
- rejected rows must have reject metadata;
- accepted/rejected rows are durable status evidence and should not be pruned
  casually.

Known gaps:

- Some local submission paths can still bypass durable admission and write
  directly to mempool/rejection tables
  ([submit-l2-transfer.ts](src/commands/submit-l2-transfer.ts#L856)).
- Production readiness planning says admission history should not be generically
  pruned
  ([01-durable-submission-admission.md](production-readiness-plans/01-durable-submission-admission.md#L213)).

## `local_mutation_jobs`

### Purpose

`local_mutation_jobs` records local mutation jobs that happen after an L1
submission or merge has reached a point where local side effects must not be
silently skipped. It gives readiness and recovery code durable evidence that a
local side effect is running, completed, or failed.

Current job kinds:

- `local_block_finalization`
- `confirmed_merge_finalization`

### Stored Information

Schema:

- `job_id TEXT PRIMARY KEY`
- `kind TEXT NOT NULL`
- `status TEXT NOT NULL`
- `plan_hash BYTEA`
- `payload JSONB NOT NULL DEFAULT '{}'::jsonb`
- `attempts INTEGER NOT NULL DEFAULT 0`
- `last_error TEXT`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `completed_at TIMESTAMPTZ`

Allowed statuses:

- `running`
- `completed`
- `failed`

Index:

- `idx_local_mutation_jobs_status_updated` on `(status, updated_at)`

Defined in
[0003_local_mutation_jobs.sql](src/database/migrations/sql/0003_local_mutation_jobs.sql#L1).

The adapter supports start, complete, fail, retrieve unfinished, and count
unfinished
([mutationJobs.ts](src/database/mutationJobs.ts#L52)).
`start` is idempotent for the same incomplete job id and increments attempts
([mutationJobs.ts](src/database/mutationJobs.ts#L52)).

### Writers

Local block finalization is wrapped in a local mutation job
([commit-submission.ts](src/workers/utils/commit-submission.ts#L46)).

Confirmed merge finalization starts, completes, or fails a merge finalization
job around the local `confirmed_ledger` and `blocks` updates
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts#L1440),
[merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts#L1487)).

### Readers

Readiness reports unfinished jobs
([listen-router.ts](src/commands/listen-router.ts#L456)).
Startup checks unfinished jobs before serving
([listen.ts](src/commands/listen.ts#L74)).

### Lifecycle

1. A local side-effect section starts and calls `MutationJobsDB.start`.
2. The job is marked `running`, with payload and attempt count.
3. If the local mutation succeeds, the job is marked `completed`.
4. If it fails, it is marked `failed` with truncated error text.
5. Readiness treats unfinished jobs as a reason not to report ready.

### Relationships

- `local_block_finalization` jobs relate to `immutable`, `blocks`, `mempool`,
  `processed_mempool`, deposits, and pending-finalization status.
- `confirmed_merge_finalization` jobs relate to `confirmed_ledger` and
  `blocks`.

### Invariants And Gaps

Expected invariants:

- unfinished jobs indicate local side effects that must be adopted, retried, or
  explicitly resolved before a clean serving state;
- completed jobs must have `completed_at`;
- completed jobs are not overwritten by `start`.

Known gaps:

- `plan_hash` exists in the schema but is not populated by the adapter.
- There is no automated replay/adoption executor for unfinished jobs.
- Production readiness plans call unfinished mutation jobs a blocker for
  clean-empty startup classification
  ([06-empty-ledger-mpt-authority-revision-plan.md](production-readiness-plans/06-empty-ledger-mpt-authority-revision-plan.md#L337)).

## `schema_migrations`

### Purpose

`schema_migrations` is the append-only ledger of successfully applied explicit
schema migrations. It is part of the production fail-closed schema model.

The node does not repair application schema at startup. It checks this ledger,
checks migration checksums, checks expected version, and verifies the expected
application table/index shape
([runner.ts](src/database/migrations/runner.ts#L767)).

### Stored Information

Created by the migration runner metadata setup
([runner.ts](src/database/migrations/runner.ts#L64)).

Columns:

- `version INTEGER PRIMARY KEY CHECK (version > 0)`
- `name TEXT NOT NULL`
- `checksum_sha256 TEXT NOT NULL`
- `manifest_hash_sha256 TEXT NOT NULL`
- `applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `app_version TEXT NOT NULL`
- `execution_ms INTEGER NOT NULL`
- `applied_by TEXT NOT NULL`

Unique constraint:

- `(version, checksum_sha256)`

### Writers

`db:migrate` applies pending migrations under an advisory lock and inserts a
`schema_migrations` row after a migration's SQL succeeds
([runner.ts](src/database/migrations/runner.ts#L575),
[runner.ts](src/database/migrations/runner.ts#L598)).

### Readers

The runner reads applied rows ordered by version
([runner.ts](src/database/migrations/runner.ts#L160)).
`migrate`, `getStatus`, and startup compatibility verification all use the
ledger
([runner.ts](src/database/migrations/runner.ts#L650),
[runner.ts](src/database/migrations/runner.ts#L758)).

### Lifecycle

1. Metadata tables are created if needed.
2. The runner reads existing applied migrations.
3. It refuses to migrate an unversioned database that already has application
   tables.
4. It validates contiguous versions and known checksums.
5. It applies pending migrations.
6. It inserts successful migration rows.
7. It verifies exact expected schema shape.

### Relationships

- Tied to the static `MIGRATIONS` manifest and manifest hash
  ([migrations/index.ts](src/database/migrations/index.ts#L17)).
- Used by startup schema compatibility checks.

### Invariants And Gaps

Expected invariants:

- versions are contiguous from 1;
- database version must not be ahead of the binary;
- checksum must match the compiled migration manifest;
- application tables and indexes must match the expected shape.

Known gaps:

- Append-only behavior is enforced by code and operational discipline, not by a
  database trigger.
- The production readiness plan explicitly warns not to mutate this ledger by
  hand except under audited recovery
  ([05-versioned-schema-migrations.md](production-readiness-plans/05-versioned-schema-migrations.md#L554)).

## `schema_migration_events`

### Purpose

`schema_migration_events` is the audit log for migration attempts. It records
starts, successes, failures, and an allowed `verification_failed` event type.

It is separate from `schema_migrations` because `schema_migrations` records only
successful migrations.

### Stored Information

Created by the migration runner metadata setup
([runner.ts](src/database/migrations/runner.ts#L75)).

Columns:

- `id BIGSERIAL PRIMARY KEY`
- `version INTEGER`
- `name TEXT`
- `checksum_sha256 TEXT`
- `event_type TEXT NOT NULL`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `app_version TEXT NOT NULL`
- `actor TEXT NOT NULL`
- `details JSONB NOT NULL DEFAULT '{}'::jsonb`

Allowed event types:

- `started`
- `succeeded`
- `failed`
- `verification_failed`

### Writers

The runner inserts a `started` event before executing a migration
([runner.ts](src/database/migrations/runner.ts#L585)).
On success it inserts `succeeded` with the `schema_migrations` row
([runner.ts](src/database/migrations/runner.ts#L615)).
On failure it records failure details after rollback
([runner.ts](src/database/migrations/runner.ts#L629)).

### Readers

There is no dedicated application command that exposes the full event history.
Operators can inspect the table directly when diagnosing migration attempts.
`db:status` reports migration ledger state, expected/pending versions, checksum
mismatches, and application shape, but not the event log itself
([runner.ts](src/database/migrations/runner.ts#L803)).

### Lifecycle

1. A migration attempt begins and records `started`.
2. If SQL and ledger insertion succeed, `succeeded` is recorded.
3. If migration application fails, `failed` is recorded with JSON details.
4. Events are retained as audit evidence.

### Relationships

- Complements `schema_migrations`.
- Carries the actor, app version, migration checksum, and manifest details
  needed to audit schema changes.

### Invariants And Gaps

Expected invariants:

- every successful `schema_migrations` row should have corresponding migration
  events;
- failed attempts should leave audit evidence even though they do not write a
  successful migration row.

Known gaps:

- `verification_failed` is allowed by schema but is not currently emitted by the
  verification path.
- `db:status` does not expose event history directly.

## Cross-Table Integrity Themes

### Durable tx lifecycle

The durable tx lifecycle spans:

- `tx_admissions`
- `mempool`
- `mempool_ledger`
- `mempool_tx_deltas`
- `processed_mempool`
- `immutable`
- `blocks`
- `tx_rejections`

The intended invariant is that a tx id has one explainable lifecycle at any
time. It may be queued/validating, accepted into mempool, deferred in
processed mempool, committed into immutable/block history, or rejected. Startup
integrity plans call contradictory placements a fail-closed condition
([04-startup-fail-closed-integrity.md](production-readiness-plans/04-startup-fail-closed-integrity.md#L201)).

### Pending block recovery

The pending block recovery model spans:

- `pending_block_finalizations`
- `pending_block_finalization_deposits`
- `pending_block_finalization_txs`
- `blocks`
- `immutable`
- `deposits_utxos`
- persistent MPT state
- L1 state queue headers

`pending_block_finalizations` records lifecycle status, but it is not a complete
mutation plan. Current production readiness plans call for stronger fingerprints
and replay/adoption metadata
([02-atomic-recoverable-ledger-mutations.md](production-readiness-plans/02-atomic-recoverable-ledger-mutations.md#L521)).

### MPT relationship

SQL tables store transaction payloads, deltas, ledger rows, journals, and
membership. The persistent MPTs store roots used for block commitments. Several
gaps are about proving that SQL evidence and persistent MPT roots agree:

- `processed_mempool` should explain non-empty mempool MPT state.
- `mempool_tx_deltas` should have lifecycle owners.
- `blocks`/`immutable` should replay to expected state-queue roots.
- future `latest_ledger` work may add an auditable submitted-tip SQL cache, but
  it must remain derived and root-checked.

### Migration integrity

`schema_migrations` and `schema_migration_events` support the production rule
that schema changes must be explicit, auditable, and reproducible. Startup does
not apply migrations implicitly; it verifies compatibility and fails closed if
the database is missing, unversioned, behind, ahead, or drifted.
