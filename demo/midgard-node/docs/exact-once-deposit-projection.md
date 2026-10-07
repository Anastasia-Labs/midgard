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
- the L1 follower admission identity, `l1_event_key` and `l1_origin_outref`

Allowed states:

- `(awaiting, NULL)`: ingested from the L1 follower's event projection, not yet projected
- `(projected, NULL)`: projected into `mempool_ledger` exactly once, not yet
  assigned to the first committed header that carried it
- `(projected, H)`: projected exactly once and assigned to header `H`

`consumed` is also a persisted status: spending a deposit-origin ledger row
marks the deposit consumed so reconciliation cannot reinsert it. Matching-header
abandonment can clear an assignment, and authenticated state-queue correction
can reopen affected events. Assignment is conflict-checked, not permanently
immutable across these explicit recovery paths.

## Follower-driven ingestion

The L1 follower's change driver ingests the follower's event projection at one
follower view: it inserts new events into `deposits_utxos` (and
`withdrawal_utxos`), each with the follower admission identity, under the
history owner's producer permit. A row is canonical while the follower's
never-reuse key set `l1_event_keys` holds its identity; a follower rewind past
the admission deletes the key, which orphans the row, and the driver holds the
node unready (`l1_events_orphan_recovery`) until the history owner's recovery
repairs it and re-ingests. Ingestion never writes a row whose identity differs
from the live row with the same `event_id`.

The commit end time is bounded by min(journal coverage, follower ingestion):
events ingested through view time t allow an end time up to
t + event wait - 1, while that view is still on the follower's chain.

## Exact-Once Projection

Projection runs in the same ingestion transaction, with its cutoff at
min(follower view time, journal coverage). It:

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
commit event horizon; it is not an unconditional selection of every
unassigned projected row.

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

- Ingestion and projection: `reconcileFollowerEvents` in
  `src/database/follower-events.ts`, run by the follower-change driver
  (`src/l1-events/driver.ts`, sink in `src/services/l1-follower.ts`) and by the
  history owner's recovery (`src/services/l1-follower.recovery.ts`).
- Admission identity: `src/database/l1-admission-identity.ts`.
- Commit horizon: `commitEventHorizon` in
  `src/services/history-commit-window.ts`.
- Lifecycle and selection: `src/database/deposits.ts`,
  `src/database/utils/projected-events.ts`, and `src/database/mempoolLedger.ts`.
- Assignment and recovery: `src/fibers/block-confirmation.ts`,
  `src/database/pendingBlockFinalizations.ts`, and the state-correction path.

Use the deposit projection and pending-finalization scenarios in `tests/` for
behavioral changes; reading this model is not crash/restart acceptance evidence.
