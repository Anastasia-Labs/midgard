# Exact-Once Deposit Projection

This document describes the implemented deposit-ingestion and projection model for
`demo/midgard-node`.

## Goals

- Never skip a valid deposit once it is stable on L1.
- Project each deposit into `mempool_ledger` exactly once.
- Make replay prevention and recovery durable in SQL rather than implicit in
  process globals.
- Keep `mempool_ledger` as the authoritative current L2 UTxO set while making
  its deposit-origin rows traceable back to canonical deposit events.

## Core Model

`deposits_utxos` is the durable ingress and projection log keyed by canonical
`event_id`.

Each deposit row carries:

- `deposit_l1_tx_hash`
- `status`
- `projected_header_hash`

Allowed states:

- `(awaiting, NULL)`: discovered from the provider-visible L1 set, not yet projected
- `(projected, NULL)`: projected into `mempool_ledger` exactly once, not yet
  assigned to the first committed header that carried it
- `(projected, H)`: projected exactly once and assigned to header `H`

`consumed` is also a persisted status: spending a deposit-origin ledger row
marks the deposit consumed so reconciliation cannot reinsert it. Matching-header
abandonment can clear an assignment, and authenticated state-queue correction
can reopen affected events. Assignment is conflict-checked, not permanently
immutable across these explicit recovery paths.

## Provider-visible discovery

The fetcher reconciles the full currently visible deposit UTxO set, rather than
advancing a stable-L1 SQL scan cursor. This avoids permanently missing an event
whose indexer visibility lagged an earlier scan. The commit-time ingestion
barrier adds an inclusion-time upper bound. `persistVisibleUserEventUTxOs`
converts and inserts the visible events through the idempotent deposit adapter.
The fetcher does not write `mempool_ledger`.

This is provider-visible discovery, not an independent finalized-chain proof.
Provider consistency and confirmation/recovery remain separate node boundaries.

## Exact-Once Projection

Projection is a separate SQL-driven step.

The projector:

- selects due `awaiting` rows in canonical `(inclusion_time, event_id)` order
- inserts their ledger entries into `mempool_ledger`
- sets `status='projected'`

The awaiting-row ledger reconciliation and status update share one SQL
transaction. The projector does not explicitly request serializable isolation.
It also reconciles missing ledger rows for still-`projected` deposits and rejects
conflicting existing payloads. `consumed` deposits are excluded.

Projected deposit rows are hidden from spendable UTxO queries until a confirmed
header is assigned. Projection alone does not authorize a same-block spend.

`mempool_ledger.source_event_id` is a foreign key to
`deposits_utxos(event_id)` and has a partial unique index for deposit-origin
rows. Storage therefore enforces that the same deposit cannot be projected more
than once.

## Block Inclusion

The commit worker selects unassigned events due by the effective block end
through `retrievePendingHeaderEntriesUpTo`. Selection is constrained by the
commit-time ingestion barrier and speculative predecessor exclusions; it is not
an unconditional selection of every unassigned projected row.

The selected ordered set supplies deposit-root construction, the final deposit
phase of the transition trace, and immutable pending-finalization membership.
Deposits execute after withdrawals, forced transactions, and normal transactions.
Confirmation processing assigns `projected_header_hash` and publishes newly
spendable ledger rows to the validation cache.

## Pending Finalization

Submitted blocks are journaled durably before submission.

The journal stores:

- `header_hash`
- `submitted_tx_hash`
- `block_end_time`
- included deposit event ids
- included L2 tx ids
- state machine status

Journal states:

- `pending_submission`
- `submitted_local_finalization_pending`
- `submitted_unconfirmed`
- `observed_waiting_stability`
- `finalized`
- `abandoned`

At most one active pending-finalization record may exist at a time.

After L1 submission succeeds, the journal first moves to
`submitted_local_finalization_pending`. Only once the local DB/trie side effects
finish does it advance to `submitted_unconfirmed`. This keeps crash recovery
durable: a restart can still distinguish “submitted but local finalization not
yet complete” from “submitted and locally finalized, only waiting for on-chain
confirmation”.

The node does not immediately finalize the journal after submit. It waits until
confirmation processing observes the submitted block and the configured
stable-L1 condition is satisfied, then finalizes the journal idempotently.

If the submission is abandoned, deposits remain `(projected, NULL)` and are
eligible for later inclusion without being reinserted into `mempool_ledger`.

## Recovery

SQL is authoritative. Tries and process globals are caches.

On startup:

- reconcile any active pending-finalization journal
- rebuild missing trie state from SQL-backed current state

`LATEST_LOCAL_BLOCK_END_TIME_MS` is no longer a correctness primitive for
deposit projection.

## Safety Invariants

- `deposits_utxos.status IN ('awaiting', 'projected', 'consumed')`
- `status='awaiting' => projected_header_hash IS NULL`
- an existing header assignment cannot be overwritten by a different header;
  clearing/reopening requires the matching recovery identity
- deposit payload drift for the same `event_id` is a hard error
- deposit-origin `mempool_ledger.source_event_id` is unique
- only one active pending-finalization journal exists

## Observability

Expose and alert on:

- awaiting deposit count
- projected-without-header count
- oldest awaiting deposit age
- oldest projected-without-header age
- replay-prevention violations
- journal abandonment
- SQL/trie divergence

## Implementation and checks

- Discovery: `src/fibers/fetch-and-insert-deposit-utxos.ts` and
  `src/fibers/user-event-ingestion.ts`.
- Projection: `src/fibers/project-deposits-to-mempool-ledger.ts`.
- Lifecycle and selection: `src/database/deposits.ts`,
  `src/database/utils/projected-events.ts`, and `src/database/mempoolLedger.ts`.
- Assignment and recovery: `src/fibers/block-confirmation.ts`,
  `src/database/pendingBlockFinalizations.ts`, and the state-correction path.

Use the deposit projection and pending-finalization scenarios in `tests/` for
behavioral changes; reading this model is not crash/restart acceptance evidence.
