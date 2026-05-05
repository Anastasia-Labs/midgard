# Staging-Only Table Roles

This document describes the tables that exist on `staging` but not on the
current `tx-validation` branch:

- `unsubmitted_blocks`
- `blocks_txs`
- `transaction_order_utxos`
- `withdrawal_order_utxos`

These tables belong to the older staging state machine. They are documented
here so developers can evaluate which concepts, if any, should be carried
forward into the current production-oriented `tx-validation` architecture.

Staging created these tables imperatively during startup through
`DBInitialization.program`; the current branch instead uses explicit migrations
and startup schema verification. In staging, table creation is visible in
`staging:demo/midgard-node/src/database/init.ts:32` through
`staging:demo/midgard-node/src/database/init.ts:42`. In the current branch,
startup verifies migrations through [init.ts](src/database/init.ts#L6), and the
current schema table list is migration-owned in
[migrations/index.ts](src/database/migrations/index.ts#L51).

## Table Map

| Table | Staging role | Current branch status |
| --- | --- | --- |
| `unsubmitted_blocks` | Queue of locally built, signed state-queue block commit transactions waiting for L1 submission. Also stores event-window metadata and serialized wallet/produced UTxOs. | Removed. Replaced by direct build-submit flow plus `pending_block_finalizations`, persistent MPTs, `processed_mempool`, `blocks`, and `immutable`. |
| `blocks_txs` | Mapping from submitted block header hash to included L2 tx ids for merge replay. | Replaced by current `blocks` table. |
| `transaction_order_utxos` | L1 tx-order event log fetched from the tx-order contract and included in block windows. | Removed. Current branch uses durable `/submit` admission through `tx_admissions` and native tx validation. |
| `withdrawal_order_utxos` | L1 withdrawal-order event log fetched from the withdrawal contract and applied as ledger spends. | Removed. Current branch does not carry this staging withdrawal-order event pipeline. |

## `unsubmitted_blocks`

### Purpose In Staging

`unsubmitted_blocks` was staging's durable queue of locally built block
commitments. The block commitment worker computed the next local L2 state,
built and signed the L1 state-queue append transaction, and stored the signed
transaction in this table before a separate submission fiber submitted it.

The table therefore combined several concerns:

- block ordering;
- event window boundaries;
- header hash;
- signed L1 commit transaction CBOR;
- operator wallet continuation UTxOs from Lucid `.chain()`;
- state-queue UTxOs produced by the commit tx;
- event counts and payload size metrics;
- a coarse block status.

The staging database wrapper defines the table name at
`staging:demo/midgard-node/src/database/blocks.ts:23`.

### Schema And Stored Information

Staging schema, from `staging:demo/midgard-node/src/database/blocks.ts:97`:

- `height BIGSERIAL PRIMARY KEY`
- `header_hash BYTEA NOT NULL UNIQUE`
- `event_start_time TIMESTAMPTZ NOT NULL`
- `event_end_time TIMESTAMPTZ NOT NULL`
- `new_wallet_utxos BYTEA NOT NULL`
- `l1_cbor BYTEA NOT NULL`
- `produced_utxos BYTEA NOT NULL`
- `deposits_count INTEGER NOT NULL`
- `tx_requests_count INTEGER NOT NULL`
- `tx_orders_count INTEGER NOT NULL`
- `withdrawals_count INTEGER NOT NULL`
- `total_events_size INTEGER NOT NULL`
- `status INTEGER NOT NULL DEFAULT 0`
- `time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Status values were defined at `staging:demo/midgard-node/src/database/blocks.ts:63`:

- `0`: `UNSUBMITTED`
- `1`: `SUBMITTED`
- `2`: `CONFIRMED`
- `3`: `MERGED`

The current staging call sites primarily used `UNSUBMITTED` and `SUBMITTED`;
the table had broader status values than the implemented lifecycle used.

### Writers

The staging block commitment worker wrote this table.

On first run, if the table was empty, it attempted to seed from the latest
committed state-queue UTxO on-chain and inserted a synthetic submitted entry
(`staging:demo/midgard-node/src/workers/block-commitment.ts:31` through
`staging:demo/midgard-node/src/workers/block-commitment.ts:80`).

For normal block construction:

1. It loaded the latest local block entry.
2. It fetched events in `(latest.event_end_time, now]` from deposits,
   tx-orders, withdrawals, and mempool txs.
3. It applied those events into the ledger MPT.
4. It built a new state-queue header and signed L1 commit tx.
5. It stored the new block entry and updated `mempool_ledger` in one SQL
   transaction.

The normal upsert is at
`staging:demo/midgard-node/src/workers/block-commitment.ts:153` through
`staging:demo/midgard-node/src/workers/block-commitment.ts:170`.

`buildNewBlockEntry` constructs the table payload at
`staging:demo/midgard-node/src/workers/utils/block-commitment.ts:334`. It
serializes `new_wallet_utxos`, `produced_utxos`, and signed `l1_cbor`, and sets
status to `UNSUBMITTED`.

### Readers And Mutators

The staging block-submission fiber read the earliest unsubmitted row, submitted
the stored signed L1 CBOR, then applied local SQL effects and marked the entry
submitted:

- read earliest unsubmitted:
  `staging:demo/midgard-node/src/fibers/block-submission.ts:234`;
- submit stored `l1_cbor`:
  `staging:demo/midgard-node/src/fibers/block-submission.ts:240`;
- process events for the block's event window:
  `staging:demo/midgard-node/src/fibers/block-submission.ts:246`;
- update `latest_ledger`, transfer mempool txs to `immutable`/`blocks_txs`,
  and mark the block submitted:
  `staging:demo/midgard-node/src/fibers/block-submission.ts:316` through
  `staging:demo/midgard-node/src/fibers/block-submission.ts:323`.

The staging adapter also exposed deletion helpers:

- delete by header hashes:
  `staging:demo/midgard-node/src/database/blocks.ts:295`;
- delete up to and including a block:
  `staging:demo/midgard-node/src/database/blocks.ts:315`.

### Lifecycle In Staging

1. The commitment worker seeded from chain if no local entry existed.
2. The commitment worker built the next block from a time-bounded event window.
3. It stored a fully signed L1 transaction as `UNSUBMITTED`.
4. A separate submission fiber submitted the stored signed tx.
5. After submit, it updated `latest_ledger`, moved tx payloads to
   `immutable`/`blocks_txs`, and marked the block `SUBMITTED`.
6. Merge used `blocks_txs`, not `unsubmitted_blocks`, to replay txs into
   confirmed state.

### Relationship To Current Branch

The current branch does not store a durable queue of signed, unsubmitted L1
commit transactions. Instead:

- the commit worker builds roots from `mempool`, `processed_mempool`, deposits,
  and persistent MPT state;
- it prepares a `pending_block_finalizations` journal before submission;
- it marks the journal submitted after a successful submit or deterministic
  recovery;
- local finalization inserts into `immutable` and current `blocks`;
- `processed_mempool` carries deferred tx payloads when the node cannot yet
  submit another block.

Relevant current references:

- pending journal creation in
  [commit-block-header.ts](src/workers/commit-block-header.ts#L537) and
  [commit-block-header.ts](src/workers/commit-block-header.ts#L641);
- submit success to pending journal transition in
  [commit-block-header.ts](src/workers/commit-block-header.ts#L721);
- local finalization of `immutable` and `blocks` in
  [commit-submission.ts](src/workers/utils/commit-submission.ts#L157);
- deferred payload transfer into `processed_mempool` in
  [commit-submission.ts](src/workers/utils/commit-submission.ts#L289).

### Gaps In The Staging Design

Staging `unsubmitted_blocks` is useful because it provides durable evidence of
a built commit transaction before submission. But as implemented it has
production weaknesses:

- It stores signed L1 CBOR and wallet continuation state without a complete
  recovery/adoption plan.
- It does not link to a durable local mutation job.
- It does not include a submitted tx hash column.
- It does not fully use its `CONFIRMED` and `MERGED` statuses.
- It selects block contents by wall-clock event windows and in-memory fetch
  cursors.
- It stores event counts but not a complete deterministic mutation plan or root
  fingerprint for every local SQL effect.
- It is tightly coupled to the older `latest_ledger`, tx-order, and withdrawal
  event model.

## `blocks_txs`

### Purpose In Staging

`blocks_txs` mapped a submitted state-queue block header hash to the L2 tx ids
included in that block. Merge used it to fetch the tx payloads from
`immutable`, replay the tx effects into `confirmed_ledger`, and then clear the
block's membership rows.

The table name is defined at
`staging:demo/midgard-node/src/database/blocksTxs.ts:11`.

### Schema And Stored Information

Staging schema, from `staging:demo/midgard-node/src/database/blocksTxs.ts:35`:

- `height SERIAL PRIMARY KEY`
- `header_hash BYTEA NOT NULL`
- `tx_id BYTEA NOT NULL UNIQUE`
- `time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT NOW()`

Indexes:

- `idx_blocks_header_hash` on `header_hash`
- `idx_blocks_tx_id` on `tx_id`

### Writers

The block-submission fiber inserted rows after successful L1 commit submission
while moving submitted mempool txs into `immutable`:

`staging:demo/midgard-node/src/fibers/block-submission.ts:285` through
`staging:demo/midgard-node/src/fibers/block-submission.ts:300`.

### Readers And Mutators

Readers:

- merge replay fetched tx hashes by header hash:
  `staging:demo/midgard-node/src/transactions/utils.ts:138` through
  `staging:demo/midgard-node/src/transactions/utils.ts:155`;
- the listen server exposed block lookups and logging routes:
  `staging:demo/midgard-node/src/commands/listen.ts:241` and
  `staging:demo/midgard-node/src/commands/listen.ts:462`.

Mutators:

- merge cleared rows for a merged header:
  `staging:demo/midgard-node/src/transactions/state-queue/merge-to-confirmed-state.ts:231`;
- reset cleared the table:
  `staging:demo/midgard-node/src/reset.ts:153`.

### Lifecycle In Staging

1. A block was submitted to L1.
2. Included mempool txs were inserted into `immutable`.
3. `blocks_txs` linked the submitted header to those tx ids.
4. Merge found the first queued block on L1, converted it to a header hash, read
   matching tx ids from `blocks_txs`, and fetched payloads from `immutable`.
5. After local confirmed-ledger mutation, merge cleared `blocks_txs` rows for
   that header.

### Relationship To Current Branch

Current `tx-validation` has effectively renamed and hardened this concept as
`blocks`, not to be confused with staging's `unsubmitted_blocks`.

Current `blocks` has the same core shape: `height`, `header_hash`, unique
`tx_id`, and timestamp
([0001_initial_schema.sql](src/database/migrations/sql/0001_initial_schema.sql#L11)).
It is inserted during local finalization
([commit-submission.ts](src/workers/utils/commit-submission.ts#L177)), read by
merge
([transactions/utils.ts](src/transactions/utils.ts#L496)), and cleared after
merge
([merge-to-confirmed-state.ts](src/transactions/state-queue/merge-to-confirmed-state.ts#L1475)).

Current `blocks` also adds a pre-insert check that refuses to link a tx id to a
different header
([blocks.ts](src/database/blocks.ts#L33)).

### Gaps Shared By Staging And Current

The old staging `blocks_txs` and current `blocks` both lack explicit per-header
ordinal ordering. Retrieval by header does not enforce deterministic order.
Production readiness plans call out the need to add explicit block-local
ordering for replay
([02-atomic-recoverable-ledger-mutations.md](production-readiness-plans/02-atomic-recoverable-ledger-mutations.md#L972)).

## `transaction_order_utxos`

### Purpose In Staging

`transaction_order_utxos` stored L1 tx-order event UTxOs. These were not the
same as HTTP `/submit` mempool transactions. They were user event UTxOs fetched
from the tx-order contract and included in block windows alongside deposits,
withdrawals, and mempool tx requests.

The table name is defined at
`staging:demo/midgard-node/src/database/txOrders.ts:6`.

### Schema And Stored Information

The table used the generic staging user-event schema from
`staging:demo/midgard-node/src/database/utils/user-events.ts:25`:

- `event_id BYTEA PRIMARY KEY`
- `event_info BYTEA NOT NULL`
- `asset_name TEXT NOT NULL`
- `l1_utxo_cbor BYTEA NOT NULL`
- `inclusion_time TIMESTAMPTZ NOT NULL`

`event_info` held the tx-order payload, and in the staging pipeline that
payload was treated as tx CBOR for ledger application.

### Writers

The staging user-event sync fiber fetched tx-order UTxOs from L1 and inserted
them:

- fetch config for tx orders:
  `staging:demo/midgard-node/src/fibers/sync-user-events.ts:42`;
- convert event UTxOs to generic user-event rows:
  `staging:demo/midgard-node/src/fibers/sync-user-events.ts:64`;
- insert tx-order rows:
  `staging:demo/midgard-node/src/fibers/sync-user-events.ts:116`.

The generic insert used `ON CONFLICT DO NOTHING`
(`staging:demo/midgard-node/src/database/utils/user-events.ts:66` through
`staging:demo/midgard-node/src/database/utils/user-events.ts:79`).

### Readers

The staging block commitment worker read tx-order events for the next event
window through `BlocksDB.retrieveEvents`
(`staging:demo/midgard-node/src/database/blocks.ts:161` through
`staging:demo/midgard-node/src/database/blocks.ts:180`).

It applied tx-order events to the ledger MPT and tx root in
`applyTxOrdersToLedger`
(`staging:demo/midgard-node/src/workers/utils/block-commitment.ts:144`).

The staging block-submission fiber later reprocessed tx-order events for the
submitted block's event window to update `latest_ledger` and address history
(`staging:demo/midgard-node/src/fibers/block-submission.ts:168` through
`staging:demo/midgard-node/src/fibers/block-submission.ts:212`).

### Lifecycle In Staging

1. The sync fiber fetched tx-order UTxOs from L1 using an in-memory time cursor.
2. It inserted them into `transaction_order_utxos`.
3. The commitment worker selected events by `(event_start_time, event_end_time)`.
4. It decoded/applied tx-order payloads into the ledger MPT and transaction
   root.
5. The submission fiber re-read the same time window after L1 submission to
   update `latest_ledger` and address history.
6. There was no explicit terminal transition or pruning model in the table.

### Relationship To Current Branch

Current `tx-validation` does not have the staging tx-order event pipeline. It
uses durable admission via `tx_admissions`, validates txs locally, inserts
accepted txs into `mempool`, stores deltas in `mempool_tx_deltas`, and commits
from that state.

Relevant current replacements:

- `/submit` durable admission:
  [listen-router.ts](src/commands/listen-router.ts#L1056);
- validation claim/accept/reject loop:
  [tx-queue-processor.ts](src/fibers/tx-queue-processor.ts#L411);
- accepted tx insertion and delta writes:
  [txAdmissions.ts](src/database/txAdmissions.ts#L325) and
  [mempool.ts](src/database/mempool.ts#L61).

### Gaps In The Staging Design

Staging tx-order ingestion has several production issues:

- The fetch cursor was in memory (`LATEST_USER_EVENTS_FETCH_TIME`), not a
  durable stable-L1 cursor.
- `ON CONFLICT DO NOTHING` can hide conflicting payloads for the same event id.
- Retrieval orders only by `inclusion_time`, so same-timestamp events have no
  deterministic tie-breaker.
- There is no terminal state, pending block membership, archive status, or
  retention model.
- Tx-order payload validation in staging is much weaker than the current
  durable validation pipeline.

## `withdrawal_order_utxos`

### Purpose In Staging

`withdrawal_order_utxos` stored L1 withdrawal-order event UTxOs. A withdrawal
order identified an L2 outref to spend. During block construction, the
withdrawal event removed that L2 UTxO from the ledger MPT and contributed to a
withdrawals root.

The table name is defined at
`staging:demo/midgard-node/src/database/withdrawals.ts:8`.

### Schema And Stored Information

Like `transaction_order_utxos`, this table used the generic staging user-event
schema:

- `event_id BYTEA PRIMARY KEY`
- `event_info BYTEA NOT NULL`
- `asset_name TEXT NOT NULL`
- `l1_utxo_cbor BYTEA NOT NULL`
- `inclusion_time TIMESTAMPTZ NOT NULL`

Defined generically at
`staging:demo/midgard-node/src/database/utils/user-events.ts:25`.

For withdrawals, `event_info` encoded `WithdrawalInfo`. The helper
`entryToOutRef` decoded it and converted the referenced L2 outref into the
CBOR outref used by ledger tables
(`staging:demo/midgard-node/src/database/withdrawals.ts:21`).

### Writers

The staging user-event sync fiber fetched withdrawal UTxOs from L1 and inserted
them:

- fetch config for withdrawals:
  `staging:demo/midgard-node/src/fibers/sync-user-events.ts:48`;
- insert withdrawal rows:
  `staging:demo/midgard-node/src/fibers/sync-user-events.ts:117`.

### Readers

Readers included:

- `BlocksDB.retrieveEvents`, which selected withdrawals for a block event window
  (`staging:demo/midgard-node/src/database/blocks.ts:168`);
- `applyWithdrawalsToLedger`, which deleted withdrawn outrefs from the ledger
  MPT and built the withdrawals root
  (`staging:demo/midgard-node/src/workers/utils/block-commitment.ts:83`);
- the block-submission fiber, which resolved withdrawal entries against
  `latest_ledger` for address history and latest-ledger mutation
  (`staging:demo/midgard-node/src/fibers/block-submission.ts:63`).

### Lifecycle In Staging

1. The sync fiber fetched withdrawal order UTxOs from L1.
2. It inserted rows into `withdrawal_order_utxos`.
3. The commitment worker selected withdrawals in the next event window.
4. For each withdrawal, it decoded the L2 outref and deleted that outref from
   the ledger MPT.
5. After submit, the submission fiber resolved spent ledger entries from
   `latest_ledger`, updated address history, and removed spent outrefs from
   `latest_ledger`.
6. There was no explicit terminal transition or retention state in the
   withdrawal table itself.

### Relationship To Current Branch

The current branch does not include this L1 withdrawal-order table or the older
withdrawal-root block construction path. It has native transaction validation
rules for Cardano withdrawals as observers, but that is not the same as
staging's L1 withdrawal-order event log.

Relevant current code:

- native tx codec decodes Cardano withdrawals as part of native tx structure:
  [native.ts](src/midgard-tx-codec/native.ts#L1456);
- phase-one validation documents withdrawal constraints:
  [PHASE1_VALIDATION_RULES.md](src/validation/PHASE1_VALIDATION_RULES.md#L246).

### Gaps In The Staging Design

Staging withdrawal-order ingestion has production gaps similar to tx orders:

- in-memory event fetch cursor;
- generic `ON CONFLICT DO NOTHING` insert semantics;
- ordering by inclusion time only;
- no terminal state, pending membership, or pruning model;
- dependence on `latest_ledger` as the submitted-tip pre-state;
- no explicit proof that a withdrawal event was canonical/stable before local
  ledger mutation.

## Cross-Table Comparison To `tx-validation`

### What Current Branch Already Replaces

`blocks_txs` is already replaced by current `blocks`. Bringing back
`blocks_txs` by name would duplicate state. The real missing feature is not the
staging table; it is explicit block-local tx ordering and stronger replay
fingerprints.

The current branch also replaces much of `unsubmitted_blocks` with:

- `pending_block_finalizations` for submitted/local-finalization state;
- `local_mutation_jobs` for local side-effect accountability;
- `processed_mempool` for deferred tx payloads;
- persistent MPTs for roots;
- current `blocks` plus `immutable` for committed block tx replay.

### What Current Branch Does Not Replace

The current branch does not replace staging's L1 tx-order and withdrawal-order
event model. If the protocol still requires users to submit tx orders or
withdrawal orders as L1 UTxOs, current `tx-validation` needs a new design for
that functionality. The staging tables show the old shape, but they are not
production-grade enough to bring forward unchanged.

### Design Lessons Worth Carrying Forward

Useful concepts from staging:

- a durable pre-submit/pre-finalization block artifact can improve crash
  recovery if it has explicit ownership and replay semantics;
- L1 user events need durable event ids, inclusion times, payload bytes, and
  source UTxO evidence;
- block membership and event membership need explicit ordering;
- merge replay needs durable local evidence tied to state-queue headers.

Concepts that should not be copied directly:

- startup-created tables instead of migrations;
- generic event tables with `ON CONFLICT DO NOTHING`;
- in-memory event-fetch cursors;
- wall-clock time windows without stable L1 cursor evidence;
- ambiguous status columns that are not fully exercised;
- storing signed L1 CBOR without a complete recovery/adoption protocol.

