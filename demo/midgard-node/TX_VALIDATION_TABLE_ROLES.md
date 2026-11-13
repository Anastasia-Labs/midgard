# Tx-Validation Table Roles

This document describes the core transaction-validation, pending-finalization,
and migration tables in the current node schema. It is a focused map rather
than a complete list of every table in the consolidated initial schema.

The goal is to make their current roles explicit for developers working on the
node state machine. Where a table is only partially wired, the gap is called
out directly and linked to current code instead of retired implementation-plan
documents.

## Scope

Covered tables:

- `blocks`
- `mempool`
- `processed_mempool`
- `mempool_tx_deltas`
- `tx_rejections`
- `tx_rejection_causes`
- `pending_block_finalizations`
- `pending_block_finalization_deposits`
- `pending_block_finalization_txs`
- `tx_admissions`
- `tx_admission_payloads`
- `schema_migrations`
- `schema_migration_events`

## High-Level Model

These tables support four runtime concerns:

- durable transaction admission and validation tracking;
- durable pending block submission, applied locally once the block lands;
- explicit migration metadata and fail-closed schema verification;
- local mutation accountability after L1 submission or merge side effects.

The current node no longer relies on startup-time `CREATE TABLE IF NOT
EXISTS` as the production schema path. The long-running node verifies that
explicit migrations have already installed the exact expected schema
([init.ts](src/database/init.ts)). The migration manifest lists the
application tables and indexes that must exist
([migrations/index.ts](src/database/migrations/index.ts)).

The application schema is deliberately squashed into one migration
([0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql)).
It includes durable transaction admissions, immutable admission payloads,
withdrawal and forced-transaction events, pending finalization journals, state-queue leases, and DA tables. The authoritative
manifest is `MIGRATIONS` in
[`migrations/index.ts`](src/database/migrations/index.ts); documentation must
not refer to pre-squash migration files that are no longer shipped.
The migration runner creates `schema_migrations` and
`schema_migration_events` as metadata tables
([runner.ts](src/database/migrations/runner.ts)).

## Table Map

| Table                                 | Primary role                                                                                                                       |
| ------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `blocks`                              | Local index from submitted state-queue header hash to included L2 tx ids.                                                          |
| `mempool`                             | Live accepted-tx membership with the accepted tx's canonical bytes.                                                                |
| `processed_mempool`                   | Durable holding area for tx payloads already incorporated into the mempool MPT while waiting for a later submit/finalization path. |
| `mempool_tx_deltas`                   | Per-tx spent/produced delta cache used to build MPT roots from accepted mempool txs.                                               |
| `tx_rejections`                       | Durable rejection evidence for validation, malformed preprocessing, and tx-status responses.                                       |
| `tx_rejection_causes`                 | The rejected transactions a dependent or batch rejection follows from, so a revived journal can delete the rejections it caused.   |
| `pending_block_finalizations`         | Single-active journal for submitted or potentially submitted block finalization and recovery.                                      |
| `pending_block_finalization_deposits` | Ordered deposit-event membership for a pending block journal.                                                                      |
| `pending_block_finalization_txs`      | Ordered tx-id membership for a pending block journal.                                                                              |
| `tx_admissions`                       | Durable `/submit` admission queue and validation state ledger.                                                                     |
| `tx_admission_payloads`               | Canonical bytes of a queued or validating admission; the terminal mark deletes the row.                                            |
| `schema_migrations`                   | Ledger of successfully applied schema migrations.                                                                                  |
| `schema_migration_events`             | Audit trail of migration attempts and outcomes.                                                                                    |

## `blocks`

### Purpose

`blocks` is the local membership index for L2 transactions included in a
submitted state-queue block. It links a Midgard block header hash to the L2
transaction ids that were locally finalized for that header.

The merge flow uses this table to find the tx payloads for the oldest queued
state-queue block before updating `confirmed_ledger`
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts)).
After the merge side effects update `confirmed_ledger`, the block links for
that header are removed
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts)).

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
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).

The database adapter rejects inserting a tx id under a different header if the
tx id is already present
([blocks.ts](src/database/blocks.ts)). Inserts use `ON CONFLICT (tx_id) DO NOTHING` only after checking that an existing
row is linked to the same header
([blocks.ts](src/database/blocks.ts)).

### Writers

`blocks` is written during local commit finalization, in the same SQL
transaction that inserts accepted txs into `immutable` and clears the
corresponding `mempool` rows
([commit-submission.ts](src/workers/utils/commit-submission.ts),
[commit-submission.ts](src/workers/utils/commit-submission.ts)).

### Readers

Readers include:

- merge replay, through `fetchFirstBlockTxs`
  ([transactions/utils.ts](src/transactions/utils.ts));
- the listen router block lookup path
  ([listen-router.ts](src/commands/listen-router.ts));
- the `audit-blocks-immutable` command, which validates `blocks` to
  `immutable` linkage
  ([audit-blocks-immutable.ts](src/commands/audit-blocks-immutable.ts)).

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
  ([blocks.ts](src/database/blocks.ts)), so block tx order is not
  durably represented by this table. A future schema change needs explicit
  per-header ordinals and deterministic retrieval.

## `mempool`

### Purpose

`mempool` is the live membership set for accepted L2 transactions awaiting a
commit path. Durable admission acceptance copies the canonical bytes from
`tx_admission_payloads` into `mempool.tx` and deletes the payload row in the
same transaction, so every membership row carries its own bytes. Acceptance is
fail-closed: membership insertion, the terminal admission update and the
payload deletion each verify exact affected-row counts inside that
transaction.

### Stored Information

- `tx_id BYTEA PRIMARY KEY`
- `tx BYTEA`, the canonical bytes
- `time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT NOW()`

The keyset index is `(time_stamp_tz, tx_id)`.

### Relationships And Invariants

- A membership row carries `mempool.tx`; its admission row, if any, has no
  payload row left.
- An admission row whose tx has a `mempool` or `processed_mempool` row is never
  deleted by retention: the mempool page order, the replay order and the peer
  catch-up read its `arrival_seq`.
- Accepted membership, ledger effects, consumed-deposit updates, and terminal
  admission state remain one synchronous SQL transaction. Derived delta and
  address-history projection use the bounded write-behind path.

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
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).
The adapter is a thin wrapper around the generic tx table helpers
([processedMempool.ts](src/database/processedMempool.ts)).

### Writers

`processed_mempool` is written by `skippedSubmissionProgram`, which transfers
processed txs from `mempool` when a commit cannot yet be submitted because no
confirmed predecessor block is available
([commit-submission.ts](src/workers/utils/commit-submission.ts)).

The main commit worker reaches this path after `processMpts` has already
applied the txs to the persistent mempool MPT and discovered that
`availableConfirmedBlock === ""`
([commit-block-header.ts](src/workers/commit-block-header.ts),
[commit-block-header.ts](src/workers/commit-block-header.ts)).

### Readers

Readers include:

- `establishEndTimeFromTxRequests`, which uses the oldest deferred payload when
  there are no live `mempool` rows
  ([commit-block-header.ts](src/workers/commit-block-header.ts));
- local block finalization, which folds deferred payloads into successful
  commit batches
  ([commit-submission.ts](src/workers/utils/commit-submission.ts));
- `/tx-status`, which reports whether a tx is in processed mempool
  ([listen-router.ts](src/commands/listen-router.ts)).

### Lifecycle

1. Accepted txs enter `mempool` and are reflected in `mempool_ledger`.
2. The commitment worker processes them into the persistent mempool MPT.
3. If the node cannot submit a new block yet, the tx rows are moved from
   `mempool` into `processed_mempool`.
4. On a later successful submission, deferred rows are combined with current
   mempool rows in the block; landed-block processing inserts them into
   `immutable`/`blocks` once the block lands.
5. `processed_mempool` is cleared once the block's own effects are applied
   ([own-blocks.ts](src/landed-blocks/own-blocks.ts)).

### Relationships

- Related to the persistent mempool MPT root. The MPT contains the transaction
  root effects, while `processed_mempool` stores SQL-visible tx payloads needed
  until the block lands and its own effects are applied.
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
- Startup integrity still needs to compare persistent mempool trie state with
  `processed_mempool` explicitly.

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
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).

`spent_cbor` encodes an array of outref bytes. `produced_cbor` encodes an array
of `[outref, output]` byte pairs. The adapter decodes CBOR strictly and rejects
trailing bytes, undefined values, indefinite encoding, and duplicate map keys
([mempoolTxDeltas.ts](src/database/mempoolTxDeltas.ts),
[mempoolTxDeltas.ts](src/database/mempoolTxDeltas.ts)).

### Writers

In a running node these rows come only from durable admission. For each
validated batch the tx queue processor calls `TxAdmissionsDB.markAccepted`, which, in one
transaction under the active validation lease, inserts mempool membership from
the durable admission payloads, applies the net ledger effects and
consumed-deposit state, and marks the admissions accepted. Only after that
commit does it enqueue deltas and address history through
`MempoolDB.enqueueAcceptedWriteBehind`. Auxiliary projections are bounded
write-behind work, with inline overflow handling and graceful-shutdown
draining; a crash may lose the unflushed window
([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts),
[txAdmissions.ts](src/database/txAdmissions.ts),
[mempool.ts](src/database/mempool.ts)).

### Readers

`processMpfs` reads deltas for the mempool tx ids it is about to include in an
MPF build
([process.ts](src/mpf/process.ts)). If a delta is missing or invalid,
the commit path has to fall back to resolving the tx effect, or reject the tx
depending on the decoding outcome
([commit-rejection.ts](src/mpf/commit-rejection.ts)).

### Lifecycle

1. Validation accepts a tx and produces its spent/produced effect.
2. The tx is inserted into `mempool`.
3. Its delta is upserted into `mempool_tx_deltas`.
4. The commitment worker reads the delta to apply MPT batch operations.
5. When txs are cleared from `mempool`, their deltas are also cleared
   ([mempool.ts](src/database/mempool.ts)).
6. A full `mempool` clear also clears all deltas
   ([mempool.ts](src/database/mempool.ts)).

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
- Orphan deltas remain ambiguous state and should fail closed during integrity
  checking.

## `tx_rejections`

### Purpose

`tx_rejections` stores durable evidence that a tx was rejected. It supports
validation diagnostics, malformed tx handling during commitment preprocessing,
and `/tx-status` responses.

### Stored Information

Schema from the consolidated initial migration:

- `tx_id BYTEA NOT NULL`
- `reject_code TEXT NOT NULL`
- `reject_detail TEXT`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Indexes:

- `idx_tx_rejections_tx_id`
- `idx_tx_rejections_created_at`

The unique `uniq_tx_rejections_tx_id` index and the base table are defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).
The adapter supports insert, insertMany, lookup by tx id, pruning, and clear
([txRejections.ts](src/database/txRejections.ts)).

### Writers

Writers include:

- `TxAdmissionsDB.markRejected`, which writes `tx_rejections` and transitions
  `tx_admissions` in one transaction
  ([txAdmissions.ts](src/database/txAdmissions.ts));
- commitment preprocessing for malformed mempool txs
  ([commit-rejection.ts](src/mpf/commit-rejection.ts)).

### Readers

`/tx-status` reads rejection metadata, but `resolveTxStatus` prioritizes
immutable, processed-mempool, and accepted-mempool placement before rejection
([listen-router.ts](src/commands/listen-router.ts)).

The retention sweeper prunes old rejection rows
([retention-sweeper.ts](src/fibers/retention-sweeper.ts)).

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
- A dependent or batch rejection names the rejections it follows from in
  `tx_rejection_causes`, below.

### Traced causes: `tx_rejection_causes`

[0019_tx_rejection_causes.sql](src/database/migrations/sql/0019_tx_rejection_causes.sql)
adds one row per cause of a rejection:

- `tx_id BYTEA NOT NULL`, referencing `tx_rejections (tx_id)` with
  `ON DELETE CASCADE`;
- `cause_tx_id BYTEA NOT NULL`: the producer of a rejected output the
  transaction spends, or a rejected member of the batch it was accepted in;
- primary key `(tx_id, cause_tx_id)` and index
  `idx_tx_rejection_causes_cause` on `cause_tx_id`.

The working-ledger recompute's reject closure writes the causes in the same
transaction as the rejections (`TxRejectionsDB.insertCauses`,
[working-ledger-recompute.reject-closure.ts](src/services/working-ledger-recompute.reject-closure.ts)).
A rejection with no row here is not traced to another transaction.

When the rebase applies a landed own block, dead or not, its members'
rejections are deleted, and so is every rejection whose recorded causes are
all deleted ones, transitively (`TxRejectionsDB.deleteWithTracedRejections`,
called from `applyOwnBlocks` in
[own-blocks.ts](src/landed-blocks/own-blocks.ts)). A rejection that also
has a cause still rejected stays. A cause row lives as long as its rejection:
it is deleted with it, by that application or by retention pruning.

### Invariants And Gaps

Expected invariants:

- rejection evidence should be deterministic for a tx id;
- a rejected admission row should have matching rejection metadata;
- accepted/immutable txs should not also look rejected.

Known gaps:

- `TxAdmissionsDB.markRejected` has stricter idempotent conflict semantics than
  direct `TxRejectionsDB.insertMany` callers. Direct callers can hit uniqueness
  conflicts unless their call path already prevents duplicates.
- Contradictory accepted/rejected placement is an integrity error and should
  block a clean startup classification.

## `pending_block_finalizations`

### Purpose

`pending_block_finalizations` is the durable journal of a block this node
committed: what it built (header, base, expected roots, members and ledger
delta) and the signed commit it intends to submit. After its commit a
journal records only when its landing was taken in; its status is derived
from the facts, never stored
(`ownBlockStatusSql`,
[pendingBlockFinalizations.derived-status.ts](src/database/pendingBlockFinalizations.derived-status.ts)):

- `folded`: it is the confirmed-ledger frontier or a retained fold, or,
  once its fold was pruned, the state queue merged it and no landed row
  holds it;
- `applied`: a processed landed row holds it and the working ledger took it
  in;
- `landed`: a processed landed row holds it, not applied yet;
- `unlanded`: none of these; a rollback that takes its landing off the
  chain makes it unlanded again.

An unlanded journal is dead when its block can no longer land
(`deadJournals`): a landed transaction removed it from the state queue, its
commit anchor left the chain, its base left the landed chain, another block
took the slot after its base, or its signed commit's intent was pruned or is
dead. The node's live journal is its newest unlanded journal that is not
dead; at most one exists.

### Stored Information

Principal columns:

- `header_hash BYTEA PRIMARY KEY`
- `header_cbor BYTEA NOT NULL`
- `prepared_tx_hash` and `intended_tx_hash`: the commit transaction built
  and the one signed (its bytes are in the intent journal, `l1_intents`)
- `commit_anchor_hash`, `commit_anchor_height`, `commit_anchor_slot`: the
  follower block the commit was built at
- `state_queue_lease_token`, base snapshot/tail identity, and base root columns
- expected UTxO, forced-transaction, transaction, deposit, withdrawal,
  transition-trace, and event-to-step roots
- expected per-source, total-event, and transition-step counts
- `ledger_delta_spent`, `ledger_delta_produced`
- `block_start_time TIMESTAMPTZ NOT NULL`
- `block_end_time TIMESTAMPTZ NOT NULL`
- `observed_confirmed_at_ms BIGINT`: when the rebase that applied the
  block's landing ran, the landed time the throughput metrics read once the
  landed row is folded away
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`

The complete table is defined by the migrations in `MIGRATIONS`
([migrations/index.ts](src/database/migrations/index.ts)); `0037` derived
the status and dropped the stored one.

### Writers

- `preparePendingSubmission` creates the journal before signing and inserts
  the full ordered source-event, L2 transaction, UTxO, transition-trace, and
  event-to-step snapshot
  ([pendingBlockFinalizations.prepare-pending-submission.ts](src/database/pendingBlockFinalizations.prepare-pending-submission.ts));
- `recordSignedIntent` records the signed commit in the intent journal and
  its hash in `intended_tx_hash`
  ([pendingBlockFinalizations.assert-canonical-event-members.ts](src/database/pendingBlockFinalizations.assert-canonical-event-members.ts));
- `discardUnsignedJournals` deletes an unsigned journal a later lease
  holder finds: it was never broadcast
  ([pendingBlockFinalizations.discard-unsigned-journal.ts](src/database/pendingBlockFinalizations.discard-unsigned-journal.ts));
- `pruneFoldedBeyondChallengeability` deletes folded journals past the
  challenge window, from the retention sweeper
  ([retention-sweeper.ts](src/fibers/retention-sweeper.ts)).

Landed-block processing applies an own block once it lands, recording
when in `observed_confirmed_at_ms`
([own-blocks.ts](src/landed-blocks/own-blocks.ts)), and disposes of a dead
journal's effects ([own-journals.ts](src/landed-blocks/own-journals.ts)),
which does not write the journal.

### Readers

Readers include:

- landed-block processing, which takes an own block's delta, events and base
  from its journal ([journal.ts](src/landed-blocks/journal.ts));
- the commit worker, which never builds on a base while a live journal
  exists, and resubmits only the live journal's signed commit;
- `/pipeline-status`, which counts journals by derived status and reports
  the live count and the oldest journal neither applied, folded nor dead.

### Lifecycle

1. `preparePendingSubmission` records `header_hash`, `block_end_time`, and
   members: the journal is live.
2. The signed commit is recorded and submitted through the intent journal.
3. Once the block lands, landed-block processing inserts its landed row and
   the rebase applies it (`applied`); the fold folds it (`folded`).
4. A journal whose block can no longer land is dead; the rebase disposes of
   its effects, and its members are pending again. If its block lands
   anyway, it is applied like any other.
5. The retention sweeper deletes folded journals past the challenge window.

### Relationships

- Parent table for the deposit, withdrawal, forced-transaction, L2 transaction,
  UTxO, transition-trace, and event-to-step journal tables.
- References the L1 state queue through `header_hash`, and the intent
  journal through `intended_tx_hash`.
- Its status reads `node_landed_blocks`, `node_confirmed_ledger_frontier`,
  `node_confirmed_merges` and the queue terminals.

### Invariants And Gaps

Expected invariants:

- at most one live journal exists;
- no journal row is written after its commit: landing, a rollback and its
  death are derived;
- a signed journal has its commit's intent until the intent is pruned;
- a dead journal's deposit assignments must be cleared only for its header.

Known gaps:

- The journal carries base/expected roots, the full header, source payload
  hashes, UTxOs, transition records, and an explicit replay discriminator.
  Architecture G additionally binds the owner binary, event log and digest,
  ordered event roots, and base/candidate roots. Recovery consumes these fields;
  it does not reconstruct the block from the current mempool.
- SQL persistence and native MPF promotion remain separate durability boundaries.
  Their consistency depends on the recovery and promotion checks, not merely on
  the existence of the SQL row.

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
- `payload_cbor BYTEA NOT NULL`
- `payload_sha256 BYTEA NOT NULL`
- `source_table TEXT NOT NULL`
- `source_id BYTEA NOT NULL`
- `source_time_stamp_tz TIMESTAMPTZ NOT NULL`
- primary key `(header_hash, member_id)`
- unique `(header_hash, ordinal)`

Foreign keys:

- `header_hash` references `pending_block_finalizations(header_hash)` with
  `ON DELETE CASCADE`;
- `member_id` references `deposits_utxos(event_id)` with `ON DELETE RESTRICT`.

The membership and immutable payload/source snapshot columns are defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).

### Writers

Rows are inserted by `preparePendingSubmission` from `depositEventIds`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts)).

### Readers

Rows are read through `retrieveRecord`, which loads member ids ordered by
`ordinal`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts)).

Deposit retention checks this table to avoid deleting deposit rows that are
still referenced by pending-finalization membership
([deposits.ts](src/database/deposits.ts)).

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
- Active pending-finalization membership is therefore a pruning blocker.

## `pending_block_finalization_txs`

### Purpose

`pending_block_finalization_txs` stores tx ids included in a pending block
journal. It is membership evidence for recovery and confirmation paths.

### Stored Information

Schema:

- `header_hash BYTEA NOT NULL`
- `member_id BYTEA NOT NULL`
- `ordinal INTEGER NOT NULL`
- `payload_cbor BYTEA NOT NULL`
- `payload_sha256 BYTEA NOT NULL`
- `source_table TEXT NOT NULL`
- `source_id BYTEA NOT NULL`
- `source_time_stamp_tz TIMESTAMPTZ NOT NULL`
- primary key `(header_hash, member_id)`
- unique `(header_hash, ordinal)`

Foreign key:

- `header_hash` references `pending_block_finalizations(header_hash)` with
  `ON DELETE CASCADE`.

There is intentionally no database foreign key to `mempool`,
`processed_mempool`, or `immutable`.

The membership and immutable payload/source snapshot columns are defined in
[0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql).

### Writers

Rows are inserted by `preparePendingSubmission` from `mempoolTxIds`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts)).
Tx-backed commit preparation passes ids from `processedMempoolTxs`
([commit-block-header.ts](src/workers/commit-block-header.ts)).

### Readers

Rows are read through `retrieveRecord`, ordered by `ordinal`
([pendingBlockFinalizations.ts](src/database/pendingBlockFinalizations.ts)).

### Lifecycle

1. A tx-backed commit is prepared.
2. Tx ids planned for the block are inserted as ordered member rows.
3. Landed-block processing applies the block from the parent record and
   members once the block lands.
4. Rows remain with the parent journal.

### Relationships

- Child of `pending_block_finalizations`.
- Semantically related to `mempool`, `processed_mempool`, `immutable`, and
  `blocks`, but not enforced by foreign keys.

### Invariants And Gaps

Expected invariants:

- each tx id appears at most once per header;
- ordinals are unique per header;
- member rows should match the block payload that landed-block processing
  inserts into `immutable`/`blocks`.

Known gaps:

- No FK to the tx payload's current lifecycle table.
- Already-deferred `processed_mempool` rows can be part of the payload source
  without being a complete replay plan in this table.
- The current `blocks` table lacks ordinals, so this ordered membership is not
  mirrored in the eventual merge membership table.

## `tx_admissions`

### Purpose

`tx_admissions` is the durable `/submit` admission queue and validation status
ledger. The transaction bytes of a queued or validating row live in
`tx_admission_payloads`, so queued → validating lease updates do not rewrite
the large payload; the terminal mark deletes the payload row in the same
transaction.
Together they replaced the older in-memory-only submission queue for
production reliability.

It answers questions such as:

- was this tx submitted before;
- are the submitted bytes identical to the previous request;
- is the tx queued, validating, accepted, or rejected;
- who currently owns the validation lease;
- when should validation retry.

### Stored Information

The consolidated initial migration creates enum `tx_admission_status`, table
`tx_admissions`, and the separate immutable `tx_admission_payloads` table
([0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql)).

Columns:

- `tx_id BYTEA PRIMARY KEY CHECK (octet_length(tx_id) = 32)`
- `arrival_seq BIGSERIAL NOT NULL`
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
- `submit_source TEXT NOT NULL` (`native` from `/submit`, `peer` from another
  operator's `/peer/txs`; a peer row whose verdict is `E_INPUT_NOT_FOUND` or
  `E_DEPENDS_ON_REJECTED_TX` is deleted instead of rejected)
- `request_count BIGINT NOT NULL DEFAULT 1`
- `payload_bytes INTEGER NOT NULL CHECK (payload_bytes > 0)` (length of the
  canonical bytes, so the backlog byte total needs no payload read)
- `tx_full_hash_v1 BYTEA NOT NULL CHECK (octet_length(tx_full_hash_v1) = 32)`
  (moved from `tx_admission_payloads` by
  [0034_admission_payload_bytes.sql](src/database/migrations/sql/0034_admission_payload_bytes.sql);
  a resubmission is a duplicate when its full hash matches this column)

After applying the [Flat-script migration](src/database/migrations/sql/0029_drop_cek_program_material.sql),
`tx_admission_payloads` stores canonical transaction bytes (their full hash
lives on `tx_admissions` since migration 34).
Script witnesses remain inside those transaction bytes. The migration also
removes the separate CEK material tables and sidecar columns.

This migration is limited to an unbound database with no retained protocol
state. It refuses a deployment binding, canonical payloads, receipts, ledgers,
block or DA state, recovery journals, pending finalization, or previously used
follower state. Refusal preserves those bytes and rolls back any schema work
performed by migration 28. Earlier successful migrations and the failure audit
event can still be committed.

Stop every process using the database before running the migration. The runner
also acquires the node and follower writer locks before starting its migration
transaction. Table locks and fresh reads guard against a write committed while
the migration was waiting; they do not establish that an old process whose
lock session was lost has stopped. An initialized deployment requires an
explicit coordinated protocol transition. No state reset, commitment rewrite,
or old-format compatibility conversion is performed by this migration.

`tx_admission_payloads` columns:

- `tx_id BYTEA PRIMARY KEY REFERENCES tx_admissions(tx_id) ON DELETE CASCADE`
- `tx_canonical_cbor BYTEA NOT NULL`

Statuses:

- `queued`
- `validating`
- `accepted`
- `rejected`

Submit sources:

- `native`
- `backfill`

Current indexes:

- `idx_tx_admissions_queued_arrival` on `(arrival_seq, tx_id)` for queued rows;
- `idx_tx_admissions_lease` on `lease_expires_at` for validating rows;
- `idx_tx_admissions_terminal` on `(terminal_at, tx_id)` for accepted and
  rejected rows, the retention sweep's walk
  ([0035_admission_terminal_retention.sql](src/database/migrations/sql/0035_admission_terminal_retention.sql));
- the primary-key index on `tx_id`.

Important check constraints enforce lease/status consistency, terminal status
timestamps, and rejection metadata consistency
([0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql)).

### Writers

`/submit` and `/peer/txs` hand each checked transaction to the admission
writer ([admission-writer.ts](src/services/admission-writer.ts)) and answer
before the row exists: the writer keeps it in a bounded in-memory pending set
and writes it with `TxAdmissionsDB.admitReservedBatch` in per-lane group
commits. A crash loses the acknowledged rows not yet committed. The batch
insert is idempotent for the same tx id and full hash (it adds the request
count the writer gathered), keeps the stored row for the same tx id with
different bytes, retries transient failures, and drops a row that fails on
its own (`admission_write_poison_total`)
([txAdmissions.ts](src/database/txAdmissions.ts)). Duplicates answered from
the pending set or the writer's recent-id cache never reach this table.

The tx queue processor:

- requeues expired leases
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts));
- claims ordered batches with a lease
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts));
- marks accepted txs and writes them to `mempool`
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts),
  [txAdmissions.ts](src/database/txAdmissions.ts));
- marks rejected txs and writes `tx_rejections`
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts),
  [txAdmissions.ts](src/database/txAdmissions.ts));
- releases leased rows for retry after processor failure
  ([tx-queue-processor.ts](src/fibers/tx-queue-processor.ts)).

### Readers

Readers include:

- queue processor backlog and oldest queued age metrics
  ([txAdmissions.ts](src/database/txAdmissions.ts));
- readiness, which reports durable admission backlog and oldest age
  ([listen-router.ts](src/commands/listen-router.ts));
- `/tx-status`, which includes admission status
  ([listen-router.ts](src/commands/listen-router.ts)).

### Lifecycle

1. `/submit` inserts a row with `status='queued'`.
2. The queue processor claims a batch, moving rows to `validating` under a lease.
3. Phase A and Phase B validation run.
4. Accepted rows transition to `accepted`, and a membership row carrying the
   canonical bytes is inserted into `mempool`; the payload row is deleted in
   the same transaction.
5. Rejected rows transition to `rejected`, with rejection metadata also written
   to `tx_rejections`; the payload row is deleted in the same transaction.
6. If validation infrastructure fails, rows are released back to `queued` with
   retry backoff.
7. Accepted/rejected rows are terminal admission history. The admission
   retention fiber (`src/fibers/admission-retention.ts`) deletes one once its
   `terminal_at` is older than `ADMISSION_TERMINAL_RETENTION_MS` (default one
   hour; 0 keeps them) and neither `mempool`, `processed_mempool` nor
   `tx_rejections` names its tx. It walks in bounded batches and skips locked
   rows. `/tx-status` never reads a terminal admission row, so deleting one
   changes no answer; a resubmission of that tx is admitted again and
   validation decides it again.

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
- a terminal row with a pending or rejection row naming its tx is never
  deleted; the rejection row's own retention releases it.

Known gaps:

- A rejected row lives as long as its `tx_rejections` row (`RETENTION_DAYS`),
  so rejected admissions grow with rejection evidence under junk load.

## `schema_migrations`

### Purpose

`schema_migrations` is the append-only ledger of successfully applied explicit
schema migrations. It is part of the production fail-closed schema model.

The node does not repair application schema at startup. It checks this ledger,
checks migration checksums, checks expected version, and verifies the expected
application table/index shape
([runner.ts](src/database/migrations/runner.ts)).

### Stored Information

Created by the migration runner metadata setup
([runner.ts](src/database/migrations/runner.ts)).

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
([runner.ts](src/database/migrations/runner.ts),
[runner.ts](src/database/migrations/runner.ts)).

### Readers

The runner reads applied rows ordered by version
([runner.ts](src/database/migrations/runner.ts)).
`migrate`, `getStatus`, and startup compatibility verification all use the
ledger
([runner.ts](src/database/migrations/runner.ts),
[runner.ts](src/database/migrations/runner.ts)).

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
  ([migrations/index.ts](src/database/migrations/index.ts)).
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
- Do not mutate this ledger by hand except under an explicit audited recovery.

## `schema_migration_events`

### Purpose

`schema_migration_events` is the audit log for migration attempts. It records
starts, successes, failures, and an allowed `verification_failed` event type.

It is separate from `schema_migrations` because `schema_migrations` records only
successful migrations.

### Stored Information

Created by the migration runner metadata setup
([runner.ts](src/database/migrations/runner.ts)).

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
([runner.ts](src/database/migrations/runner.ts)).
On success it inserts `succeeded` with the `schema_migrations` row
([runner.ts](src/database/migrations/runner.ts)).
On failure it records failure details after rollback
([runner.ts](src/database/migrations/runner.ts)).

### Readers

There is no dedicated application command that exposes the full event history.
Operators can inspect the table directly when diagnosing migration attempts.
`db:status` reports migration ledger state, expected/pending versions, checksum
mismatches, and application shape, but not the event log itself
([runner.ts](src/database/migrations/runner.ts)).

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
time. It may be queued/validating, accepted into mempool, deferred in processed
mempool, committed into immutable/block history, or rejected. Contradictory
placements are a fail-closed integrity condition.

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

`pending_block_finalizations` records lifecycle status and exact replay inputs.
The native replay variant binds the event log, event roots, owner identity, and
base/candidate roots; the delta variant carries its ledger delta. Recovery uses
these persisted inputs and checks the durable marker. A populated journal alone
is not evidence that every crash boundary has been exercised.

### MPT relationship

SQL tables store transaction payloads, deltas, ledger rows, journals, and
membership. The persistent MPTs store roots used for block commitments. Several
gaps are about proving that SQL evidence and persistent MPT roots agree:

- `processed_mempool` should explain non-empty mempool MPT state.
- `mempool_tx_deltas` should have lifecycle owners.
- `blocks`/`immutable` should replay to expected state-queue roots.

### Migration integrity

`schema_migrations` and `schema_migration_events` support the production rule
that schema changes must be explicit, auditable, and reproducible. Startup does
not apply migrations implicitly; it verifies compatibility and fails closed if
the database is missing, unversioned, behind, ahead, or drifted.
