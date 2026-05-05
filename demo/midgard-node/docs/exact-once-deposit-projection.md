# Exact-Once Deposit Projection

This document defines the production deposit-ingestion and projection model for
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

- `(awaiting, NULL)`: discovered from stable L1, not yet projected
- `(projected, NULL)`: projected into `mempool_ledger` exactly once, not yet
  assigned to the first committed header that carried it
- `(projected, H)`: projected exactly once and assigned to header `H`

No other transitions are allowed.

## Stable-L1 Discovery

Deposit discovery is driven by a durable SQL cursor, not by in-memory refs.

The cursor stores:

- the latest stable L1 view token used for scanning
- the stable scan upper bound time
- the last scanned deposit event id for auditability

The fetcher is discovery-only:

- resolve a stable L1 view
- fetch deposits in `(previous_scan_upper_bound, current_stable_upper_bound]`
- upsert them into `deposits_utxos` as `awaiting`
- advance the cursor in the same SQL transaction

The fetcher never writes `mempool_ledger`.

## Exact-Once Projection

Projection is a separate SQL-driven step.

The projector:

- selects `awaiting` rows in canonical `(inclusion_time, event_id)` order
- inserts their ledger entries into `mempool_ledger`
- sets `status='projected'`

This happens in one serializable SQL transaction.

`mempool_ledger.source_event_id` is a foreign key to
`deposits_utxos(event_id)` and has a partial unique index for deposit-origin
rows. Storage therefore enforces that the same deposit cannot be projected more
than once.

## Block Inclusion

The deposit set for the next block is not chosen by a time window.

It is exactly:

- all rows in `deposits_utxos` where `status='projected'` and
  `projected_header_hash IS NULL`

That exact ordered set is used for:

- overlaying deposit UTxOs into the ledger trie pre-state
- computing `depositsRoot`
- recording the pending-finalization journal membership
- assigning `projected_header_hash` after confirmation

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

- validate the stable-L1 cursor view if present
- reconcile any active pending-finalization journal
- rebuild missing trie state from SQL-backed current state

`LATEST_LOCAL_BLOCK_END_TIME_MS` is no longer a correctness primitive for
deposit projection.

## Safety Invariants

- `deposits_utxos.status IN ('awaiting', 'projected')`
- `status='awaiting' => projected_header_hash IS NULL`
- `projected_header_hash` is immutable once set
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
