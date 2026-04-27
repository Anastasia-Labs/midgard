# P0 Blocker 8: State-Aware Retention For Deposits And Replay Evidence

## Problem Statement

The current retention sweeper deletes deposit records by wall-clock age. That is
not safe for a production L2 because deposits are L1-to-L2 bridge evidence, not
ordinary cache rows. A deposit row may be old while still being live,
projectable, assigned to an unconfirmed block, needed for local-finalization
recovery, needed for merge replay, or needed to prove the correctness of a
prior bridge inclusion.

The fix is to make retention state-aware and audit-aware. Deposit retention must
only physically prune records that are terminal and already exported into a
durable audit archive with checksums. Live records, replay records, records
referenced by pending/finalized-but-unmerged block journals, and records whose
archive cannot be verified must remain in PostgreSQL. Age can be a secondary
condition; it must never be the primary proof of prune safety.

This plan covers P0 blocker 8 from the production-readiness report:
[`PRODUCTION_READINESS_REPORT.md:614`](../PRODUCTION_READINESS_REPORT.md#L614).
It is an implementation plan only. It does not implement the code changes.

## Current Behavior

The report identifies the blocker directly:

- The sweeper prunes deposits, address history, and rejections by age:
  [`PRODUCTION_READINESS_REPORT.md:618`](../PRODUCTION_READINESS_REPORT.md#L618).
- The report states that age alone does not prove a deposit is terminal,
  committed, merged, archived, or no longer needed for replay/audit:
  [`PRODUCTION_READINESS_REPORT.md:634`](../PRODUCTION_READINESS_REPORT.md#L634).
- The desired shape is explicit retention classes plus terminal-state checks:
  [`PRODUCTION_READINESS_REPORT.md:651`](../PRODUCTION_READINESS_REPORT.md#L651).

Relevant retention code:

- `RETENTION_DAYS` is read from config with default `0`:
  [`src/services/config.ts:175`](../src/services/config.ts#L175), then exposed
  on `NodeConfig`:
  [`src/services/config.ts:330`](../src/services/config.ts#L330).
- `.env.example` sets `RETENTION_DAYS=0`, so pruning is disabled unless
  operators opt in:
  [`.env.example:59`](../.env.example#L59).
- `shouldPruneRetention` enables pruning for any finite positive day count:
  [`src/database/retention-policy.ts:3`](../src/database/retention-policy.ts#L3).
- `computeRetentionCutoff` floors days and subtracts that number of days from
  `now`:
  [`src/database/retention-policy.ts:6`](../src/database/retention-policy.ts#L6).
- The node starts `retentionSweeperFiber` with
  `WAIT_BETWEEN_RETENTION_SWEEPS` alongside block, deposit, merge, and tx
  workers:
  [`src/commands/listen.ts:143`](../src/commands/listen.ts#L143).
- The sweeper calls `TxRejectionsDB.pruneOlderThan`,
  `AddressHistoryDB.pruneOlderThan`, and `DepositsDB.pruneOlderThan` in
  parallel:
  [`src/fibers/retention-sweeper.ts:26`](../src/fibers/retention-sweeper.ts#L26).

Relevant deposit table behavior:

- `deposits_utxos` has `event_id`, `event_info`, `inclusion_time`,
  `deposit_l1_tx_hash`, projected L2 ledger fields, `projected_header_hash`,
  and `status`:
  [`src/database/deposits.ts:60`](../src/database/deposits.ts#L60).
- Existing statuses are only `awaiting`, `projected`, and `consumed`:
  [`src/database/deposits.ts:28`](../src/database/deposits.ts#L28).
- The table accepts only those three statuses and requires `awaiting` rows to
  have no `projected_header_hash`:
  [`src/database/deposits.ts:69`](../src/database/deposits.ts#L69).
- Deposit ingestion inserts rows from visible L1 deposit UTxOs with status
  `awaiting` and full event/ledger payload:
  [`src/fibers/fetch-and-insert-deposit-utxos.ts:118`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L118).
- Insert is idempotent for identical payloads but refuses conflicting payloads
  for the same `event_id`:
  [`src/database/deposits.ts:125`](../src/database/deposits.ts#L125).
- Awaiting deposits are selected by inclusion time for projection:
  [`src/database/deposits.ts:254`](../src/database/deposits.ts#L254).
- `projectDepositsToMempoolLedger` reconciles already projected deposits and
  projects due awaiting deposits:
  [`src/fibers/project-deposits-to-mempool-ledger.ts:27`](../src/fibers/project-deposits-to-mempool-ledger.ts#L27),
  [`src/fibers/project-deposits-to-mempool-ledger.ts:78`](../src/fibers/project-deposits-to-mempool-ledger.ts#L78).
- Projection inserts/reconciles `mempool_ledger` rows and then marks deposits
  `projected`:
  [`src/fibers/project-deposits-to-mempool-ledger.ts:92`](../src/fibers/project-deposits-to-mempool-ledger.ts#L92).
- `mempool_ledger.source_event_id` references `deposits_utxos(event_id)` with
  `ON DELETE RESTRICT`:
  [`src/database/mempoolLedger.ts:67`](../src/database/mempoolLedger.ts#L67).
  This prevents deleting some live projected deposits, but it is an incidental
  database constraint, not a complete retention policy.
- A mempool transaction that spends deposit-origin UTxOs clears those UTxOs and
  marks corresponding deposits `consumed`:
  [`src/database/mempool.ts:83`](../src/database/mempool.ts#L83).
- Block construction resolves deposits pending header assignment and may
  re-include previously projected deposits after abandoned header assignment:
  [`src/workers/utils/mpt.ts:177`](../src/workers/utils/mpt.ts#L177).
- Block construction refuses to skip an overdue awaiting deposit:
  [`src/workers/utils/mpt.ts:203`](../src/workers/utils/mpt.ts#L203).
- Pending block finalization stores included deposit event ids in
  `pending_block_finalization_deposits`, with a foreign key back to
  `deposits_utxos` and `ON DELETE RESTRICT`:
  [`src/database/pendingBlockFinalizations.ts:130`](../src/database/pendingBlockFinalizations.ts#L130).
  Those member rows are not currently removed when the journal becomes
  finalized or abandoned, so retaining that direct FK forever would make
  physical deposit pruning unreachable unless the membership evidence is moved
  to a durable identity/audit table first.
- Successful commit submission assigns the header hash to included deposits:
  [`src/workers/utils/commit-submission.ts:183`](../src/workers/utils/commit-submission.ts#L183).
- Block confirmation also assigns/retains the header hash when a pending block
  is observed on L1:
  [`src/fibers/block-confirmation.ts:94`](../src/fibers/block-confirmation.ts#L94).
- Abandoned pending blocks clear the matching deposit header assignment:
  [`src/fibers/block-confirmation.ts:115`](../src/fibers/block-confirmation.ts#L115).
- Merge local finalization updates `confirmed_ledger` and clears the merged
  block from `BlocksDB`, but does not currently transition deposit rows to an
  audited terminal retention class:
  [`src/transactions/state-queue/merge-to-confirmed-state.ts:1413`](../src/transactions/state-queue/merge-to-confirmed-state.ts#L1413).
- Deposit-only commits can create pending-finalization journals with deposit
  members and no `BlocksDB` transaction members:
  [`src/workers/commit-block-header.ts:536`](../src/workers/commit-block-header.ts#L536).
  Retention must therefore record deposit block-membership and merge-boundary
  evidence explicitly; it cannot infer deposit terminality only from remaining
  `BlocksDB` rows.
- Current deposit pruning ignores every lifecycle field and deletes by
  `inclusion_time < cutoff`:
  [`src/database/deposits.ts:640`](../src/database/deposits.ts#L640).

Relevant non-deposit pruning behavior:

- `tx_rejections` has `tx_id`, rejection code/detail, and `created_at`, with no
  terminal/archive fields:
  [`src/database/txRejections.ts:33`](../src/database/txRejections.ts#L33).
- Rejection pruning deletes by `created_at < cutoff`:
  [`src/database/txRejections.ts:93`](../src/database/txRejections.ts#L93).
- `address_history` stores `(tx_id, address, created_at)` as a derived lookup
  index:
  [`src/database/addressHistory.ts:32`](../src/database/addressHistory.ts#L32).
- Address-history pruning deletes by `created_at < cutoff`:
  [`src/database/addressHistory.ts:158`](../src/database/addressHistory.ts#L158).
- Address history is derived from mempool/immutable transaction payloads:
  [`src/database/addressHistory.ts:121`](../src/database/addressHistory.ts#L121).
  Pruning it is safe only if source transaction payloads remain available or the
  pruned index rows are archived/rebuildable.

## Target Retention Invariants

1. A deposit row is never physically deleted because of age alone.
2. `awaiting`, `projected`, `consumed`, header-assigned, pending-finalization,
   submitted, confirmed-but-unmerged, abandoned-but-replayable, and
   archive-unverified deposits are not pruneable.
3. A deposit is pruneable only after a deterministic state transition marks it
   terminal for retention and links it to a verified durable archive export.
4. The archive must contain enough data to prove and replay the deposit
   lifecycle: L1 event identity, event payload, inclusion time, L1 deposit tx
   hash, derived L2 ledger output, address, header assignment, block/merge
   evidence, final lifecycle state, and hashes of any related tx/block records.
5. Pruning must be idempotent. Re-running a sweep must not change retained state
   except for deleting rows already marked pruneable.
6. Pruning must be auditable. Every prune batch has a durable audit row with
   cutoff, config snapshot, selected row count, archive id, archive checksum,
   deleted row count, operator/process identity, and failure state if any step
   fails.
7. Pruning must be transactional for PostgreSQL state. The archive verification
   step may happen before the delete, but the final "mark prune complete and
   delete rows" operation must be a single database transaction.
8. If archive verification, SQL deletion, audit logging, or invariant checking
   fails, the sweeper fails closed for that table and deletes nothing from
   `deposits_utxos`.
9. Foreign-key `ON DELETE RESTRICT` constraints remain defense in depth, not the
   primary safety mechanism. Live-reference FKs may point at hot tables, but
   durable audit/reference tables must not retain FKs that make terminal hot-row
   pruning impossible.
10. Retention does not create compatibility modes or legacy ID fallbacks in
    `demo/midgard-node`. Schema changes are explicit migrations.
11. Rejection and address-history pruning must not remove the last copy of
    canonical status or replay evidence. If a table is derived, the source or an
    audit archive must remain sufficient to rebuild or explain it.
12. Readiness/observability must expose blocked retention, unarchived terminal
    backlog, and failed prune attempts so operators can distinguish safe
    retention delay from a broken sweeper.
13. Pruning a hot deposit row must leave a durable, queryable identity/tombstone
    keyed by `event_id`. Existing and future status/audit lookups must be able
    to distinguish `archived_pruned` from `not_found`.

## Deposit Lifecycle States

### Existing States

The existing `deposits_utxos.status` is too coarse for retention:

- `awaiting`: L1 deposit was ingested and has not been projected into
  `mempool_ledger`.
- `projected`: deposit-origin UTxO has been projected into `mempool_ledger` or
  is replayable for block inclusion.
- `consumed`: deposit-origin UTxO was spent by an L2 transaction while still in
  the mempool/projection path.

These states describe operational projection, not final audit safety. In
particular, `consumed` is not terminal for retention. A consumed deposit may
still be part of a pending block, an unmerged block, a local-finalization
recovery path, or an audit investigation.

### Target Lifecycle Model

Implement a separate retention lifecycle rather than overloading the existing
projection status. This keeps the current operational state machine readable and
lets retention answer a narrower question: "May this evidence leave the hot
database?"

Recommended deposit retention states:

- `live_unprojected`: existing row is `awaiting`; never prune.
- `live_projected`: existing row is `projected` with no terminal block/merge
  evidence; never prune.
- `live_consumed`: existing row is `consumed` but not yet proven finalized and
  archived; never prune.
- `block_pending`: row is referenced by an active pending-finalization record;
  never prune.
- `block_abandoned_replayable`: header assignment was cleared or abandoned and
  the deposit must remain available for re-inclusion; never prune.
- `block_confirmed_unmerged`: row belongs to a confirmed state-queue block that
  has not passed merge/audit boundary; never prune. This includes journals
  observed on L1 but still requiring local finalization recovery.
- `merged_replay_retained`: row passed merge boundary but remains in hot storage
  for configured replay/audit horizon; not pruneable until archived.
- `archive_pending`: row is eligible for archival export but the archive has not
  been written and verified; never prune.
- `archived_terminal`: archive exists and checksum is verified; pruneable only
  after the configured terminal retention horizon.
- `pruned`: logical marker recorded in audit tables after the row has been
  physically deleted from `deposits_utxos`.

Allowed transitions must be monotonic except for explicit recovery transitions
from active block states back to replayable states:

```text
live_unprojected
  -> live_projected
  -> live_consumed

live_projected
  -> block_pending

live_consumed
  -> block_pending
  -> block_confirmed_unmerged
  -> merged_replay_retained
  -> archive_pending
  -> archived_terminal
  -> pruned

block_pending -> block_abandoned_replayable -> live_projected
```

The implementation can derive some target retention states from joins instead
of storing every name literally, but the database must still store enough
explicit state to make prune eligibility deterministic and inspectable.

The merge boundary must be represented by durable evidence, not by absence of
rows. Clearing `BlocksDB` during merge is not sufficient proof that deposit
members are terminal, especially for deposit-only blocks. Add or reuse an
explicit block-retention evidence record keyed by header hash that records:

- header hash and block end time;
- deposits root and ordered deposit event ids;
- ordered mempool tx ids, if any;
- local finalization completion time and source operation;
- merge-to-confirmed completion time, confirmed root evidence, and submitted
  merge tx hash where available;
- archive/export ids that captured the block membership evidence.

Only rows whose containing header has this merge-boundary evidence may move to
`merged_replay_retained` or `archive_pending`.

## Archival And Audit Model

Retention must introduce an append-only archive/audit model before enabling any
deposit pruning.

### Archive Contents

Each archived deposit record must include:

- `event_id` and `event_info`.
- `inclusion_time`.
- `deposit_l1_tx_hash`.
- `ledger_tx_id`, `ledger_output`, and `ledger_address`.
- Current projection status and retention state.
- `projected_header_hash`, if any.
- Pending/finalization journal evidence for the header that included it.
- Block membership evidence: header hash, deposit ordinal, deposits root, block
  end time, and confirmation/merge boundary where available.
- L1 observation evidence from deposit ingestion, including fetch barrier/cursor
  data once the startup fail-closed work makes it durable.
- Audit metadata: export id, export format version, export timestamp, exporting
  node identity/config hash, row digest, batch digest, and archive URI/path.
- Tombstone metadata after pruning: prune sweep id, prune timestamp, prune
  reason, and the retained identity row digest. This metadata lives in SQL even
  after the hot deposit row is gone.

Use a deterministic serialization format for archive records. JSON is acceptable
only if key ordering and binary encodings are canonicalized. A length-delimited
binary format is also acceptable. The archive checksum must be computed over the
canonical bytes, not over ad hoc rendered text.

### Archive Tables

Add explicit migrations for:

```sql
CREATE TABLE deposit_retention_identities (
  event_id BYTEA PRIMARY KEY,
  event_info_sha256 BYTEA NOT NULL,
  deposit_l1_tx_hash BYTEA NOT NULL,
  inclusion_time TIMESTAMPTZ NOT NULL,
  ledger_tx_id BYTEA NOT NULL,
  ledger_output_sha256 BYTEA NOT NULL,
  ledger_address TEXT NOT NULL,
  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  source_row_sha256 BYTEA NOT NULL
);

CREATE TABLE deposit_retention_archives (
  archive_id UUID PRIMARY KEY,
  archive_format_version INTEGER NOT NULL,
  storage_uri TEXT NOT NULL,
  row_count INTEGER NOT NULL CHECK (row_count >= 0),
  content_sha256 BYTEA NOT NULL,
  config_sha256 BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  verified_at TIMESTAMPTZ,
  verification_error TEXT
);

CREATE TABLE deposit_retention_archive_members (
  archive_id UUID NOT NULL REFERENCES deposit_retention_archives(archive_id)
    ON DELETE RESTRICT,
  event_id BYTEA NOT NULL REFERENCES deposit_retention_identities(event_id)
    ON DELETE RESTRICT,
  event_row_sha256 BYTEA NOT NULL,
  archived_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (archive_id, event_id),
  UNIQUE (event_id)
);

CREATE TABLE deposit_retention_block_memberships (
  header_hash BYTEA NOT NULL,
  event_id BYTEA NOT NULL REFERENCES deposit_retention_identities(event_id)
    ON DELETE RESTRICT,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  deposits_root BYTEA NOT NULL,
  block_end_time TIMESTAMPTZ NOT NULL,
  merge_evidence_sha256 BYTEA,
  archived_by UUID REFERENCES deposit_retention_archives(archive_id)
    ON DELETE RESTRICT,
  PRIMARY KEY (header_hash, event_id),
  UNIQUE (header_hash, ordinal)
);

CREATE TABLE retention_sweep_audit (
  sweep_id UUID PRIMARY KEY,
  table_name TEXT NOT NULL,
  retention_class TEXT NOT NULL,
  cutoff TIMESTAMPTZ NOT NULL,
  config_sha256 BYTEA NOT NULL,
  selected_count INTEGER NOT NULL CHECK (selected_count >= 0),
  deleted_count INTEGER NOT NULL DEFAULT 0 CHECK (deleted_count >= 0),
  archive_id UUID REFERENCES deposit_retention_archives(archive_id)
    ON DELETE RESTRICT,
  status TEXT NOT NULL CHECK (
    status IN ('started', 'archive_verified', 'delete_committed', 'failed')
  ),
  failure_reason TEXT,
  started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMPTZ
);

CREATE TABLE deposit_retention_pruned_events (
  event_id BYTEA PRIMARY KEY REFERENCES deposit_retention_identities(event_id)
    ON DELETE RESTRICT,
  archive_id UUID NOT NULL REFERENCES deposit_retention_archives(archive_id)
    ON DELETE RESTRICT,
  sweep_id UUID NOT NULL REFERENCES retention_sweep_audit(sweep_id)
    ON DELETE RESTRICT,
  event_row_sha256 BYTEA NOT NULL,
  pruned_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  prune_reason TEXT NOT NULL
);
```

Extend `deposits_utxos` with retention metadata:

```sql
ALTER TABLE deposits_utxos
  ADD COLUMN retention_state TEXT NOT NULL DEFAULT 'live_unprojected',
  ADD COLUMN retention_state_updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  ADD COLUMN terminal_at TIMESTAMPTZ,
  ADD COLUMN archive_id UUID REFERENCES deposit_retention_archives(archive_id)
    ON DELETE RESTRICT,
  ADD COLUMN archived_at TIMESTAMPTZ,
  ADD COLUMN archive_row_sha256 BYTEA;

ALTER TABLE deposits_utxos
  ADD CONSTRAINT deposits_retention_state_check CHECK (
    retention_state IN (
      'live_unprojected',
      'live_projected',
      'live_consumed',
      'block_pending',
      'block_abandoned_replayable',
      'block_confirmed_unmerged',
      'merged_replay_retained',
      'archive_pending',
      'archived_terminal'
    )
  );

ALTER TABLE deposits_utxos
  ADD CONSTRAINT deposits_archive_terminal_check CHECK (
    retention_state <> 'archived_terminal'
    OR (
      terminal_at IS NOT NULL
      AND archive_id IS NOT NULL
      AND archived_at IS NOT NULL
      AND archive_row_sha256 IS NOT NULL
    )
  );
```

Do not rely on the `DEFAULT` as a silent migration for existing rows. The
explicit migration must backfill and verify retention state from current
`status`, `projected_header_hash`, pending-finalization rows, `mempool_ledger`,
and block/merge evidence. If a row cannot be classified with proof, classify it
as a live non-pruneable state and record a migration audit warning.

The identity table is not optional. Populate `deposit_retention_identities`
before any FK rewrites or hot-row deletion. It is the stable target for audit
tables, archived block-membership rows, and pruned-status lookup after
`deposits_utxos` no longer contains the hot payload. Do not store
`pruned_by_sweep_id` only on `deposits_utxos`; that marker disappears when the
row is physically deleted.

Adjust pending-finalization deposit membership before enabling pruning. Either:

- migrate `pending_block_finalization_deposits.member_id` to reference
  `deposit_retention_identities(event_id)` instead of `deposits_utxos(event_id)`
  while keeping application-level and prune-query checks that active journals
  block deletion; or
- archive finalized/abandoned membership evidence into
  `deposit_retention_block_memberships` and remove only those non-active member
  rows before hot-row pruning.

Do not leave finalized or abandoned audit membership rows with a direct
`ON DELETE RESTRICT` FK to `deposits_utxos`, because that makes
`archived_terminal` pruning impossible. Active live references, such as
`mempool_ledger.source_event_id`, should continue to block deletion directly.

## Prune Query Changes

Replace `DepositsDB.pruneOlderThan(cutoff)` with a retention-class-specific
operation, for example `DepositsDB.pruneArchivedTerminal(cutoff, sweepId)`.

The delete must be shaped like this:

```sql
WITH candidates AS (
  SELECT d.event_id
  FROM deposits_utxos d
  JOIN deposit_retention_archive_members m
    ON m.event_id = d.event_id
  JOIN deposit_retention_archives a
    ON a.archive_id = m.archive_id
   AND a.archive_id = d.archive_id
  WHERE d.retention_state = 'archived_terminal'
    AND d.archived_at < $1
    AND d.terminal_at IS NOT NULL
    AND d.archive_row_sha256 = m.event_row_sha256
    AND a.verified_at IS NOT NULL
    AND a.verification_error IS NULL
    AND NOT EXISTS (
      SELECT 1
      FROM mempool_ledger ml
      WHERE ml.source_event_id = d.event_id
    )
    AND NOT EXISTS (
      SELECT 1
      FROM pending_block_finalization_deposits p
      JOIN pending_block_finalizations f
        ON f.header_hash = p.header_hash
      WHERE p.member_id = d.event_id
        AND f.status IN (
          'pending_submission',
          'submitted_local_finalization_pending',
          'submitted_unconfirmed',
          'observed_waiting_stability'
        )
    )
    AND EXISTS (
      SELECT 1
      FROM deposit_retention_identities i
      WHERE i.event_id = d.event_id
    )
    AND NOT EXISTS (
      SELECT 1
      FROM deposit_retention_pruned_events pe
      WHERE pe.event_id = d.event_id
    )
  ORDER BY d.archived_at ASC, d.event_id ASC
  LIMIT $2
)
DELETE FROM deposits_utxos d
USING candidates c
WHERE d.event_id = c.event_id
RETURNING d.event_id;
```

Additional implementation requirements:

- Use bounded batches. Unbounded `DELETE` can hold locks too long and harm
  liveness.
- Use a single SQL transaction to lock candidate rows, insert one
  `deposit_retention_pruned_events` tombstone per candidate, update the sweep
  audit row, and delete the selected hot rows.
- Verify `deleted_count === selected_count` unless a concurrent process made
  the candidate non-pruneable. Prefer row locks or `FOR UPDATE SKIP LOCKED` in
  the candidate selection to make this deterministic.
- If any candidate fails a join, digest, archive, or live-reference check, do
  not delete that candidate.
- Keep `ON DELETE RESTRICT` foreign keys in `mempool_ledger` and
  active live-reference tables as a final safety net. Do not keep finalized
  audit-only membership FKs pointed at `deposits_utxos`.
- Rename log fields from `deposits_utxos=<count>` to
  `deposits_archived_terminal_pruned=<count>` so logs describe what was
  actually eligible.

The sweeper must sequence deposit archive verification before deposit pruning.
It may continue pruning `tx_rejections` or `address_history` only after their
own safety rules are separated from deposit evidence. Deposit deletion must not
run in `Effect.all(..., { concurrency: "unbounded" })` with unrelated pruning if
that makes audit ordering unclear.

Audit rows for failed external steps must survive rollback of the delete
transaction. Use a two-phase audit pattern:

1. Insert `retention_sweep_audit(status='started')` before external archive
   verification.
2. If archive verification fails, update that row to `failed` without entering
   the delete transaction.
3. If verification succeeds, perform candidate locking, tombstone insertion,
   audit update to `delete_committed`, and hot-row deletion in one SQL
   transaction.
4. If the SQL transaction fails, record a separate failed attempt row or update
   the started row after rollback, and delete no hot rows.

## Tx Rejection And Address-History Retention

This P0 is deposit-driven, but the current sweeper prunes three tables together.
The implementation must avoid turning non-deposit pruning into hidden evidence
loss.

For `tx_rejections`:

- Treat rejection rows as canonical status evidence until durable admission or a
  replacement canonical tx-status table exists.
- Do not prune a rejection if it is the only durable explanation for a submitted
  tx id.
- Add archive metadata or move canonical rejection status into the durable
  admission table planned by P0 blocker 1 before enabling rejection pruning in
  production.
- If rejection pruning remains enabled, require a verified rejection archive
  with tx id, code, detail, created time, validation phase, and admission/status
  linkage.

For `address_history`:

- Treat rows as a derived index only if the source tx payload and ledger/block
  evidence remain available.
- Prune address history only when it can be rebuilt from retained mempool,
  immutable, confirmed, or archived tx payloads.
- If source payloads may be pruned, archive address-history rows with source
  tx digests first.

Configuration must separate these table classes. A single `RETENTION_DAYS`
switch is too broad for production evidence classes.

## Configuration Semantics

Replace broad `RETENTION_DAYS` semantics with explicit retention settings. Since
`demo/midgard-node` does not need backward-compatible shims, the implementation
may remove or hard-fail ambiguous legacy configuration once migrations land.

Recommended config:

- `RETENTION_ENABLED=false` by default. When false, the sweeper may report
  eligible counts but must not delete.
- `DEPOSIT_RETENTION_ENABLED=false` by default until archive verification,
  lifecycle migration, tests, and rollout checks pass.
- `DEPOSIT_ARCHIVE_REQUIRED=true` always in production. Do not provide a
  production mode that prunes without archive verification.
- `DEPOSIT_ARCHIVED_TERMINAL_RETENTION_DAYS=<positive integer>` controls only
  `archived_terminal.archived_at`, not `inclusion_time`.
- `RETENTION_SWEEP_BATCH_SIZE=<bounded positive integer>` controls maximum rows
  deleted per table per sweep.
- `RETENTION_DRY_RUN=true` is allowed for rollout and audits; it must select,
  verify, and audit candidates without deleting.
- `TX_REJECTION_RETENTION_DAYS` and `ADDRESS_HISTORY_RETENTION_DAYS` are
  separate from deposit retention and require their own archive/source
  guarantees.
- Invalid combinations fail startup, for example deposit pruning enabled while
  archive required is false, archive storage URI is unset, retention days is
  zero/negative, or migrations are not at the required schema version.

`RETENTION_DAYS` should not keep its current meaning of "delete every supported
table by age." For production-grade behavior, either remove it in an explicit
breaking config migration or reinterpret it only as a deprecated startup error
with clear remediation. Do not silently map it to deposit terminal retention.

## Implementation Plan

1. Add explicit schema migrations.
   Create identity, archive, block-membership, pruned-event, and sweep-audit
   tables; add deposit retention metadata; add indexes for
   `(retention_state, archived_at, event_id)`, active pending membership, and
   archive membership; and add check constraints.

2. Add a retention state classifier.
   Implement a deterministic SQL-backed classifier that derives current
   retention state from `deposits_utxos.status`, `projected_header_hash`,
   `mempool_ledger.source_event_id`, active/finalized pending-finalization
   rows, archived block membership, and merge boundary evidence. Unknown rows
   are live/non-pruneable.

3. Backfill retention state in the migration.
   Backfill `deposit_retention_identities` first, then retention state and
   block-membership evidence in bounded batches. Verify counts and record
   migration audit output. Do not silently rewrite conflicting rows; fail
   migration or mark rows non-pruneable with an explicit audit warning.

4. Wire lifecycle transitions into existing deposit paths.
   Update ingestion to create `live_unprojected`, projection to move to
   `live_projected`, mempool spending to move to `live_consumed`, pending-block
   creation to move included deposits to `block_pending`, abandonment to move
   them to `block_abandoned_replayable`, confirmation to move them to
   `block_confirmed_unmerged`, and merge/audit boundary to move them to
   `merged_replay_retained` or `archive_pending`. Record block-membership and
   merge-boundary evidence before clearing operational block rows.

5. Add archive writer and verifier.
   Export only candidates that have crossed the merge/audit boundary. Write the
   archive, compute per-row and batch checksums, read back the archive, verify
   hashes, and only then set `archive_id`, `archive_row_sha256`, `archived_at`,
   and `retention_state='archived_terminal'`.

6. Replace deposit prune query.
   Remove age-only `DepositsDB.pruneOlderThan` from the sweeper path. Add
   `pruneArchivedTerminal` with live-reference joins, archive verification, row
   locks, bounded batches, tombstone insertion, and sweep audit rows.

7. Split sweeper table classes.
   Make the sweeper call separate table-specific retention policies with
   separate config and audit entries. Do not run deposit pruning as a generic
   age-based peer of rejection/address pruning.

8. Add readiness and metrics.
   Expose retention failures, unarchived terminal backlog, archive verification
   failures, prune counts, blocked live-reference counts, and last successful
   sweep timestamp. Readiness should fail or degrade when retention is enabled
   but archive verification or schema invariants are broken.

9. Add operator commands.
   Provide dry-run commands for `retention classify`, `retention archive`, and
   `retention prune --dry-run`. Each command should print candidate counts,
   blocked reasons, archive ids, and checksums.

10. Update deposit-status/audit lookups.
    Make deposit status resolution check hot rows first, then
    `deposit_retention_pruned_events` plus archive membership. A pruned terminal
    deposit must return an explicit archived/pruned status with archive id,
    prune sweep id, and enough identity data to audit the event; it must not be
    reported as `404 not_found`.

11. Update docs and `.env.example`.
    Document the new explicit retention variables and remove the impression that
    `RETENTION_DAYS` is a safe broad production pruning switch.

## Tests And Fault Injection

Unit tests:

- `shouldPruneRetention` replacement rejects ambiguous/legacy/bad config.
- Cutoff applies to `archived_at`, not `inclusion_time`.
- Classifier maps `awaiting` with no header to `live_unprojected`.
- Classifier maps projected rows with `mempool_ledger.source_event_id` to
  `live_projected`.
- Classifier maps consumed rows without merge/archive proof to
  `live_consumed`.
- Active pending-finalization membership blocks pruning.
- Abandoned header assignment returns deposits to replayable non-pruneable
  state.
- Archive checksum mismatch blocks `archived_terminal`.
- `archived_terminal` without archive member row is rejected by constraints or
  prune query.
- A deposit cannot be marked pruneable unless `deposit_retention_identities`
  contains the event id and matching source-row digest.

Database integration tests:

- Age-old `awaiting`, `projected`, and `consumed` deposits are not deleted.
- Age-old deposits referenced by `mempool_ledger` are not deleted.
- Age-old deposits referenced by active `pending_block_finalization_deposits`
  journals are not deleted.
- A verified `archived_terminal` deposit older than terminal retention is
  deleted and creates/updates a `retention_sweep_audit` row.
- A candidate whose archive row digest differs is not deleted.
- A candidate whose archive has `verified_at IS NULL` is not deleted.
- A candidate with `verification_error IS NOT NULL` is not deleted.
- Batch size limits the number of rows deleted.
- Re-running the same prune after success is idempotent.
- Concurrent projection/finalization changing a candidate state prevents
  deletion deterministically.
- Pruning inserts `deposit_retention_pruned_events` in the same transaction as
  the hot-row delete.
- A finalized or abandoned pending-finalization member row that has been moved
  to durable block-membership evidence does not block pruning, while active
  pending-finalization membership still blocks pruning.
- A direct audit-only FK from finalized pending membership to `deposits_utxos`
  is rejected by migration/startup verification.

End-to-end tests:

- Deposit ingestion, projection, block inclusion, confirmation, merge, archive,
  and prune leaves enough archive data to answer deposit-status/audit queries.
- Deposit-only block inclusion follows the same archive and prune path even
  when there are no `BlocksDB` transaction rows for the header.
- Crash after archive write but before marking rows `archived_terminal` causes
  restart to verify and resume without deleting unmarked rows.
- Crash after marking `archived_terminal` but before delete leaves rows
  pruneable and auditable.
- Crash during prune transaction rolls back both delete and audit update.
- Startup fails closed when retention is enabled but archive tables or
  constraints are missing.

Fault injection:

- Archive storage write failure.
- Archive read-back failure.
- Archive checksum mismatch.
- SQL transaction failure after candidate selection.
- Foreign-key violation from an unexpected live reference.
- Clock skew around cutoff boundaries.
- Large archive batch with partial candidate invalidation.
- Operator starts with old `RETENTION_DAYS` only and no explicit new retention
  config.

Regression tests for non-deposit retention:

- Rejection pruning cannot remove the only canonical rejection/status evidence.
- Address-history pruning cannot remove rows unless the source tx payload or an
  address-history archive remains available.

## Rollout Steps

1. Land explicit migrations while retention deletion remains disabled.
2. Backfill deposit retention metadata in dry-run/report mode on representative
   data.
3. Enable classifier metrics and readiness checks without pruning.
4. Run archive export in dry-run mode and compare archive digests against SQL
   source rows.
5. Enable archive writing but keep pruning disabled.
6. Verify archive read-back and row checksums over multiple sweeps.
7. Enable `DEPOSIT_RETENTION_ENABLED=true` with a small batch size in a non-prod
   environment.
8. Run deposit flow emulator tests through ingestion, commit, confirmation,
   merge, archive, prune, and audit lookup.
9. Enable production pruning only after readiness, audit logs, and recovery
   drills pass.
10. Remove old broad `RETENTION_DAYS` behavior and update operational runbooks.

## Risks And Open Questions

- Merge boundary definition: the implementation must define the exact point at
  which a deposit no longer needs hot replay evidence. The likely boundary is
  after the containing block has been merged into confirmed state and local
  merge finalization is complete, but this must align with the recoverable
  mutation journal from P0 blocker 2 and merge failure handling from P0 blocker
  3.
- Deposit-status API after pruning: decide whether `GET /deposit-status` should
  query the archive/audit index for pruned terminal deposits or return a
  specific archived/pruned response. This plan requires the latter behavior to
  be explicit and non-ambiguous; implementation should decide the exact JSON
  shape while preserving current selector validation.
- Archive storage durability: choose a production storage backend and define
  fsync/object-store consistency requirements before enabling deletion.
- Existing rows: migration may find rows whose state cannot be proven because
  prior retention or manual cleanup removed related records. Those rows should
  remain non-pruneable until an explicit operator audit resolves them.
- Cross-P0 sequencing: state-aware retention depends on durable admission,
  atomic recoverable mutations, startup fail-closed checks, and schema
  migrations for its strongest guarantees. It can be built incrementally, but
  physical pruning should remain disabled until those dependencies are
  sufficiently in place.
- Rejection/address retention: table-specific archival may expand scope. If not
  completed in this P0, keep their pruning disabled in production rather than
  preserving the current broad age-based behavior.

## Concrete Checklist

- [ ] Add explicit migrations for deposit retention metadata, archive tables,
      identity/tombstone tables, audit tables, indexes, and constraints.
- [ ] Add deterministic deposit retention classifier and migration backfill.
- [ ] Add lifecycle transition updates to deposit ingestion, projection,
      mempool spending, pending block preparation, abandonment, confirmation,
      merge boundary, archive, and prune paths.
- [ ] Add durable block-membership/merge-boundary evidence and remove or
      retarget audit-only FKs that would otherwise keep pruned hot rows alive.
- [ ] Implement canonical archive serialization, per-row hashes, batch hashes,
      write, read-back, and verification.
- [ ] Replace `DepositsDB.pruneOlderThan` usage with
      `pruneArchivedTerminal(cutoff, batchSize, sweepId)`.
- [ ] Split sweeper config and code by retention class/table.
- [ ] Add retention sweep audit rows and structured logs for every archive and
      prune attempt.
- [ ] Add readiness/metrics for retention backlog, archive failures, prune
      failures, and last successful sweep.
- [ ] Add dry-run operator commands for classify/archive/prune.
- [ ] Update deposit-status/audit lookup to report archived/pruned deposits
      from tombstone and archive indexes instead of returning `not_found`.
- [ ] Disable or remove broad `RETENTION_DAYS` age-based production pruning.
- [ ] Add unit tests for config, classifier, archive checks, and prune
      eligibility.
- [ ] Add database integration tests for all live states and archived terminal
      pruning.
- [ ] Add fault-injection tests for archive and SQL failures.
- [ ] Add end-to-end deposit flow coverage through archive and prune.
- [ ] Update `.env.example`, README/runbook retention documentation, and
      production-readiness notes.
