# P0 Blocker 1: Durable, Idempotent Submission Admission Before Ack

## Problem Statement

`POST /submit` currently acknowledges responsibility for an L2 transaction after
placing it in process memory. A process crash after the response but before the
validation worker persists the transaction can make a client-observed
`status: "queued"` disappear into `not_found`. That violates the production L2
admission contract: success must mean the node has durable, replayable
responsibility for the submission.

The fix is to make durable admission the first state-changing step in `/submit`.
The HTTP handler must return success only after the normalized transaction bytes,
transaction id, and initial lifecycle state are committed to PostgreSQL. The
validation worker must use this durable table as its source of truth, including
after restart. No in-memory queue, compatibility mode, or legacy fallback may be
treated as authoritative.

This plan implements the first item in the production-readiness ordering:
[`PRODUCTION_READINESS_REPORT.md:1562`](../PRODUCTION_READINESS_REPORT.md#L1562).

## Current Behavior

The readiness report identifies the blocker directly:

- Ack means the `/submit` response with `status: "queued"`:
  [`PRODUCTION_READINESS_REPORT.md:22`](../PRODUCTION_READINESS_REPORT.md#L22).
- Durable means state that survives crashes and restarts:
  [`PRODUCTION_READINESS_REPORT.md:25`](../PRODUCTION_READINESS_REPORT.md#L25).
- Idempotent means repeated submission cannot create duplicate work:
  [`PRODUCTION_READINESS_REPORT.md:27`](../PRODUCTION_READINESS_REPORT.md#L27).
- The current node returns success from an in-memory queue path:
  [`PRODUCTION_READINESS_REPORT.md:48`](../PRODUCTION_READINESS_REPORT.md#L48).

Relevant code paths:

- `runNode` creates an in-memory dropping queue with
  `Queue.dropping<QueuedTxPayload>`:
  [`src/commands/listen.ts:65`](../src/commands/listen.ts#L65).
- The HTTP server receives that queue through `buildListenRouter(txQueue)`:
  [`src/commands/listen.ts:115`](../src/commands/listen.ts#L115).
- `/submit` accepts both query-string and JSON body input, normalizes the
  submitted bytes, builds a `QueuedTxPayload`, and offers it to the in-memory
  queue:
  [`src/commands/listen-router.ts:971`](../src/commands/listen-router.ts#L971),
  [`src/commands/listen-router.ts:1005`](../src/commands/listen-router.ts#L1005),
  [`src/commands/listen-router.ts:1032`](../src/commands/listen-router.ts#L1032),
  [`src/commands/listen-router.ts:1037`](../src/commands/listen-router.ts#L1037).
- If the offer succeeds, `/submit` increments a counter and returns
  `status: "queued"`:
  [`src/commands/listen-router.ts:1050`](../src/commands/listen-router.ts#L1050).
- The worker drains the Effect queue into a module-level `pendingPayloads` array:
  [`src/fibers/tx-queue-processor.ts:123`](../src/fibers/tx-queue-processor.ts#L123),
  [`src/fibers/tx-queue-processor.ts:387`](../src/fibers/tx-queue-processor.ts#L387).
- The worker assigns non-durable arrival sequence numbers from a module-level
  `nextArrivalSeq`:
  [`src/fibers/tx-queue-processor.ts:123`](../src/fibers/tx-queue-processor.ts#L123),
  [`src/fibers/tx-queue-processor.ts:249`](../src/fibers/tx-queue-processor.ts#L249).
- Accepted and rejected results are persisted only after validation:
  [`src/fibers/tx-queue-processor.ts:521`](../src/fibers/tx-queue-processor.ts#L521),
  [`src/fibers/tx-queue-processor.ts:539`](../src/fibers/tx-queue-processor.ts#L539).

Existing durable transaction tables are not an admission log:

- Generic tx tables use `tx_id` as the primary key and store transaction bytes:
  [`src/database/utils/tx.ts:43`](../src/database/utils/tx.ts#L43).
- Single-row tx insert currently updates bytes on conflict:
  [`src/database/utils/tx.ts:155`](../src/database/utils/tx.ts#L155).
- Bulk tx insert currently does nothing on conflict:
  [`src/database/utils/tx.ts:179`](../src/database/utils/tx.ts#L179).
- Rejections are stored in `tx_rejections`, but that table records only
  rejection metadata and has no transaction bytes:
  [`src/database/txRejections.ts:28`](../src/database/txRejections.ts#L28).
- `GET /tx-status` derives status from rejection, immutable, mempool, and
  processed-mempool tables, but not from a queued durable admission state:
  [`src/commands/listen-router.ts:332`](../src/commands/listen-router.ts#L332).
- Readiness currently reports in-memory queue depth, not durable admission
  backlog:
  [`src/commands/listen-router.ts:440`](../src/commands/listen-router.ts#L440),
  [`src/commands/readiness.ts:15`](../src/commands/readiness.ts#L15).

## Target Invariants

1. `/submit` never returns a success response until the transaction is durably
   admitted in PostgreSQL or already exists in durable canonical state.
2. A transaction acknowledged as `queued` must be discoverable after process
   crash and restart without client resubmission.
3. Admission is idempotent by `tx_id` and exact bytes: same `tx_id` plus same
   normalized bytes returns the existing durable state; same `tx_id` plus
   different bytes is an integrity conflict and must not overwrite data.
4. The validation worker's source of truth is the durable admission table.
   In-memory data may be used only as a non-authoritative wake-up optimization.
5. A worker crash cannot strand work permanently. `validating` claims must have
   leases that expire and are replayed.
6. Validation outcomes are exact-once at the durable state level. A transaction
   may be retried after infrastructure failure, but it must not create duplicate
   accepted or rejected records.
7. Admission backpressure is explicit and durable. If the node cannot accept
   responsibility, it returns a non-success response and does not pretend to
   queue the transaction.
8. Readiness answers whether the node is safe to admit new user traffic, using
   durable backlog and worker health, not only process memory.
9. Startup fails closed on schema ambiguity. No silent table reshaping or
   compatibility shim may be used for production behavior.

## Schema And Data Model Changes

Add a new explicit migration, after the migration framework from the readiness
report exists:
[`PRODUCTION_READINESS_REPORT.md:441`](../PRODUCTION_READINESS_REPORT.md#L441).
If the migration framework is not yet implemented, this P0 must include a
minimal explicit migration runner first; do not add another silent
`CREATE TABLE IF NOT EXISTS` startup mutation to `InitDB.program`.

Create `tx_admissions`:

```sql
CREATE TYPE tx_admission_status AS ENUM (
  'queued',
  'validating',
  'accepted',
  'rejected'
);

CREATE TABLE tx_admissions (
  tx_id BYTEA PRIMARY KEY CHECK (octet_length(tx_id) = 32),
  tx_cbor BYTEA NOT NULL CHECK (octet_length(tx_cbor) > 0),
  tx_cbor_sha256 BYTEA NOT NULL CHECK (octet_length(tx_cbor_sha256) = 32),
  arrival_seq BIGSERIAL UNIQUE NOT NULL,
  status tx_admission_status NOT NULL,
  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  validation_started_at TIMESTAMPTZ,
  terminal_at TIMESTAMPTZ,
  lease_owner TEXT,
  lease_expires_at TIMESTAMPTZ,
  attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  reject_code TEXT,
  reject_detail TEXT,
  submit_source TEXT NOT NULL,
  request_count BIGINT NOT NULL DEFAULT 1 CHECK (request_count >= 1),
  CONSTRAINT tx_admissions_submit_source_check
    CHECK (submit_source IN ('native', 'cardano-converted', 'backfill')),
  CONSTRAINT tx_admissions_time_order_check
    CHECK (last_seen_at >= first_seen_at AND updated_at >= first_seen_at),
  CONSTRAINT tx_admissions_lease_status_check CHECK (
    (
      status = 'validating'
      AND lease_owner IS NOT NULL
      AND lease_expires_at IS NOT NULL
      AND terminal_at IS NULL
    )
    OR (
      status <> 'validating'
      AND lease_owner IS NULL
      AND lease_expires_at IS NULL
    )
  ),
  CONSTRAINT tx_admissions_terminal_status_check CHECK (
    (status IN ('accepted', 'rejected') AND terminal_at IS NOT NULL)
    OR (status IN ('queued', 'validating') AND terminal_at IS NULL)
  ),
  CONSTRAINT tx_admissions_rejection_status_check CHECK (
    (status = 'rejected' AND reject_code IS NOT NULL)
    OR (status <> 'rejected' AND reject_code IS NULL AND reject_detail IS NULL)
  )
);

CREATE INDEX idx_tx_admissions_dequeue
  ON tx_admissions (next_attempt_at, arrival_seq)
  WHERE status IN ('queued', 'validating');

CREATE INDEX idx_tx_admissions_status_updated
  ON tx_admissions (status, updated_at);

CREATE INDEX idx_tx_admissions_lease
  ON tx_admissions (lease_expires_at)
  WHERE status = 'validating';
```

The migration must also make terminal rejection evidence exact-once. Add a
unique constraint or unique index on `tx_rejections(tx_id)` after failing closed
on any preexisting duplicate rejection rows whose code/detail cannot be
unambiguously reduced to one terminal outcome. `markRejected` may then use
`ON CONFLICT (tx_id) DO NOTHING` only after it has verified the existing
rejection row matches `tx_admissions.reject_code` and `reject_detail`.

Add `src/database/txAdmissions.ts` with a thin adapter matching the existing DB
module style, but do not use generic `Tx.insertEntry` because that helper updates
bytes on conflict. Required operations:

- `admit(normalized, source)`: serializable transaction that enforces duplicate
  semantics and returns the durable status row.
- `getByTxId(txId)`.
- `claimBatch({ limit, leaseOwner, leaseDurationMs })`: uses
  `FOR UPDATE SKIP LOCKED` and returns rows ordered by `arrival_seq`.
- `releaseForRetry(txIds, nextAttemptAt, reason)`: moves claimed rows back to
  `queued` after infrastructure failure.
- `extendLease({ txIds, leaseOwner, leaseDurationMs })`: optional but
  recommended for long validation batches; it must extend only rows still owned
  by the caller.
- `markAccepted(rows, processedTxs)`: in one SQL transaction, inserts mempool
  state and updates `tx_admissions.status = 'accepted'`.
- `markRejected(rows, rejectedTxs)`: in one SQL transaction, inserts rejection
  records and updates `tx_admissions.status = 'rejected'`.
- `requeueExpiredLeases(now)`: changes expired `validating` rows back to
  `queued` before normal claims.
- `countBacklog()`, `oldestBacklogAgeMs()`, and `countByStatus()` for readiness
  and metrics.

Do not store only hashes. The normalized transaction bytes must be durable so
the worker can replay after restart.

Do not allow generic retention pruning to delete `tx_admissions` rows. Admission
rows are canonical replay/status evidence until a future explicit archival plan
replaces them with verified audit evidence.

`tx_admissions.status` is the durable admission and validation state. External
API status should still be resolved canonically from all durable state. For
example, a row can remain `accepted` in `tx_admissions` while `GET /tx-status`
and duplicate `/submit` responses report `pending_commit` or `committed` because
the transaction is present in processed-mempool or immutable/block state.

## Admission API Behavior

Change `/submit` from "normalize then offer to queue" to "normalize then commit
admission row":

```text
POST /submit
  parse request
  validate size and hex shape
  normalize to canonical Midgard-native bytes and tx_id
  TxAdmissionsDB.admit(...)
  return durable state
```

Success responses:

- New admission: `202 Accepted`

```json
{
  "txId": "<64 hex>",
  "status": "queued",
  "firstSeenAt": "<iso timestamp>",
  "lastSeenAt": "<iso timestamp>"
}
```

- Duplicate with identical normalized bytes: `200 OK`, returning the current
  canonical durable status and timestamps. This includes `queued`, `validating`,
  `accepted`, `rejected`, `pending_commit`, and `committed`, resolved from
  `tx_admissions` plus the existing durable transaction tables.

Failure responses:

- Invalid request shape, invalid hex, invalid size, or normalization failure:
  keep the current 400 or 413 behavior.
- Durable backlog at or above configured admission capacity before accepting a
  new transaction: `503 Service Unavailable` with a retryable error. Existing
  duplicates must still be queryable and idempotent under backlog pressure.
- Database unavailable or migration state invalid: `503 Service Unavailable`;
  no in-memory fallback.
- Same `tx_id` with different normalized bytes: `409 Conflict` with a stable
  error code such as `E_TX_ID_BYTES_CONFLICT`; increment an integrity metric and
  emit a structured error log. Do not overwrite the stored bytes.

The current query-string submission path is a separate readiness finding, but
when this P0 is implemented, the durable admission behavior must apply to every
accepted submit path. A follow-up should remove query-string mutation entirely;
do not preserve it via a separate legacy queue path.

Before admitting a new transaction, `/submit` must also run the same admission
safety gates used by readiness: schema version valid, database writable, durable
backlog below capacity, validation worker heartbeat fresh enough, and no local
finalization recovery state that intentionally blocks user admission. These
gates apply only to new admissions. Duplicates with identical bytes must remain
idempotently queryable even while new admissions are blocked, unless the database
or schema is unavailable.

Add explicit configuration instead of reusing the old in-memory queue settings:

- `MAX_DURABLE_ADMISSION_BACKLOG`: hard cap for non-terminal
  `queued`/`validating` rows accepted by `/submit`.
- `READINESS_MAX_DURABLE_ADMISSION_BACKLOG`: readiness threshold, less than or
  equal to the hard cap.
- `READINESS_MAX_DURABLE_ADMISSION_AGE_MS`: oldest queued row age that makes
  readiness fail.
- `VALIDATION_LEASE_MS`: initial claim lease duration.
- `VALIDATION_RETRY_BACKOFF_BASE_MS` and
  `VALIDATION_RETRY_BACKOFF_MAX_MS`: bounded exponential retry delay for
  infrastructure failures.
- `VALIDATION_EXPIRED_LEASE_READINESS_THRESHOLD`: number of expired leases that
  makes readiness fail.

`MAX_SUBMIT_QUEUE_SIZE` and `READINESS_MAX_QUEUE_DEPTH` may remain during
transition for old tests, but production admission capacity must come from the
durable settings above.

## Worker And Replay Behavior

Replace the in-memory queue source of truth in
`tx-queue-processor.ts` with durable polling:

1. On each scheduled tick, update the worker heartbeat.
2. Requeue expired `validating` rows whose `lease_expires_at < now()`.
3. Claim up to `VALIDATION_BATCH_SIZE` rows with:

```sql
WITH candidates AS (
  SELECT tx_id
  FROM tx_admissions
  WHERE
    (status = 'queued' AND next_attempt_at <= now())
    OR (status = 'validating' AND lease_expires_at < now())
  ORDER BY arrival_seq
  FOR UPDATE SKIP LOCKED
  LIMIT $1
)
UPDATE tx_admissions a
SET
  status = 'validating',
  lease_owner = $2,
  lease_expires_at = now() + $3::interval,
  validation_started_at = COALESCE(validation_started_at, now()),
  attempt_count = attempt_count + 1,
  updated_at = now()
FROM candidates
WHERE a.tx_id = candidates.tx_id
RETURNING a.*;
```

4. Convert returned rows to `QueuedTx` using durable `arrival_seq` and
   `first_seen_at`, not module-level counters.
5. Run the existing phase-A and phase-B validation logic.
6. Persist accepted and rejected outcomes in SQL transactions that update both
   the existing destination table and `tx_admissions`.
7. If validation infrastructure fails before trustworthy validation results are
   produced, release the claimed rows back to `queued` with exponential backoff.
   Do not mark them rejected.
8. If the process crashes at any point before a terminal update, the row remains
   `queued` or eventually becomes claimable from expired `validating`.

The worker should no longer use `pendingPayloads`, `pendingSinceMillis`, or
`nextArrivalSeq` as durable scheduling state. Batch aging and backlog metrics
must come from `tx_admissions`.

Terminal writes must be lease-fenced. `markAccepted`, `markRejected`,
`releaseForRetry`, and `extendLease` must update rows only when
`status = 'validating'` and `lease_owner` matches the worker's claim token. A
stale worker whose lease expired must not be able to write mempool effects,
rejection effects, or terminal admission state after another worker has claimed
the row. If a fenced terminal update affects fewer rows than expected, the
worker must roll back the whole SQL transaction, discard its computed
validation result, increment a stale-lease metric, and let the current owner or
lease expiry drive replay.

Accepted persistence must not call the current `MempoolDB.insertMultiple` as a
standalone effect and then update `tx_admissions` separately. Refactor the
accepted path so the transaction-row insert, produced UTxO insert, delta upsert,
spent-input clearing, deposit consumed marking, address-history insert, and
`tx_admissions` transition all run inside one `sql.withTransaction` block. The
transition must be compare-and-set from the caller's `validating` lease to
`accepted`, clear the lease fields, set `terminal_at`, and verify exact row
counts. Existing destination rows for the same `tx_id` are idempotent only when
their bytes and derived effects match; conflicting destination state is an
integrity failure and must fail closed.

Rejected persistence follows the same pattern: insert the exact-once
`tx_rejections` row, set `tx_admissions.reject_code`, `reject_detail`, and
`status = 'rejected'`, clear the lease fields, set `terminal_at`, and commit all
of that atomically. Retrying a terminal write may observe an already-terminal
matching result and return success, but it must never insert a second rejection
row or reapply accepted ledger side effects.

## Duplicate And Idempotency Semantics

Use the report's rule:
[`PRODUCTION_READINESS_REPORT.md:939`](../PRODUCTION_READINESS_REPORT.md#L939).

```text
same tx_id + same normalized tx_cbor = idempotent
same tx_id + different normalized tx_cbor = integrity conflict
```

Admission conflict handling:

1. In a serializable transaction, try to insert the new row.
2. On primary-key conflict, lock the existing row.
3. Compare `tx_cbor_sha256` and, if equal, the stored `tx_cbor` bytes.
4. If both match, update only `last_seen_at` and `request_count`, then return
   the existing durable status.
5. If either differs, return `409` and record an integrity event. Do not change
   `tx_cbor`, `status`, or lifecycle timestamps.

Serialization failures in `admit` are retryable inside the adapter with a small
bounded retry count. After retries are exhausted, `/submit` must return `503`
rather than falling back to memory or returning an ambiguous success.

Cross-table duplicate suppression:

- If a transaction was accepted before `tx_admissions` exists, migration must
  backfill it or fail closed. Do not leave preexisting accepted rows as an
  untracked legacy path.
- If the same `tx_id` exists in more than one durable transaction table with
  different bytes, migration must fail closed with an integrity error. If bytes
  match, create one admission row and let canonical status resolution decide the
  user-facing state.
- For current code after the migration, every accepted or rejected transaction
  must have a `tx_admissions` row.
- `GET /tx-status` should include `queued` and `validating` states from
  `tx_admissions`. The canonical status priority should align with the report:
  committed, pending commit, accepted, rejected, validating, queued, not found
  ([`PRODUCTION_READINESS_REPORT.md:819`](../PRODUCTION_READINESS_REPORT.md#L819)).

## Migration Strategy

Use explicit migrations with recorded version and checksum. Do not hide this
behind normal startup table creation.

Required migration phases:

1. Add the `tx_admission_status` enum and `tx_admissions` table.
2. Backfill from durable transaction tables that contain bytes:
   `mempool`, `processed_mempool`, and immutable transaction storage. Backfilled
   rows must compute `tx_cbor_sha256`, set `status = 'accepted'`, and preserve a
   deterministic `arrival_seq` ordered by existing timestamps and `tx_id`.
   After explicit `arrival_seq` inserts, reset the backing sequence to
   `max(arrival_seq) + 1` in the same migration so new admissions cannot collide.
3. Backfill rejected rows only if there is enough durable information to prove
   exact bytes. Since `tx_rejections` currently stores no transaction CBOR, the
   migration must fail closed if orphan rejection rows exist without matching
   admission rows or tx bytes. Duplicate rejection rows for a transaction must
   also fail closed unless they are byte-for-byte equivalent in code/detail and
   can be collapsed before adding the unique constraint. This is acceptable for
   `demo/midgard-node` because backward compatibility shims are explicitly not a
   goal.
4. Add the `tx_rejections(tx_id)` uniqueness constraint only after the duplicate
   and orphan checks pass.
5. Add a startup assertion that every non-empty mempool, processed-mempool,
   immutable, or rejection row has a corresponding `tx_admissions` row after the
   migration.
6. Remove `tx_admissions` creation from regular `InitDB.program` once the
   migration is in place. Startup should assert the expected schema version
   before serving `/submit`.

Rollback:

- Schema rollback is not a safe operational recovery path once admissions have
  been acknowledged. Roll forward with a corrected migration.
- If rollout must be aborted before traffic is admitted, stop the node, restore
  the database snapshot taken immediately before migration, and restart the old
  binary only against that restored snapshot.

## Observability And Readiness

Add structured logs with a request/admission correlation path:

```text
request_id -> tx_id -> arrival_seq -> validation_batch_id
```

Metrics:

- `submission_admission_count_total{result="new|duplicate|conflict|backlog_full|db_error"}`
- `submission_admission_latency_ms`
- `durable_admission_backlog{status="queued|validating"}`
- `durable_admission_oldest_age_ms`
- `durable_admission_expired_lease_count`
- `durable_admission_retry_count_total`
- `durable_admission_integrity_conflict_count_total`
- `durable_admission_stale_lease_write_count_total`
- `durable_admission_terminal_conflict_count_total`
- `validation_claim_batch_size`
- `validation_claim_latency_ms`

Readiness changes:

- Replace in-memory `txQueue.size` as the admission signal with durable backlog
  counts from `tx_admissions`.
- Fail readiness when durable backlog exceeds
  `READINESS_MAX_DURABLE_ADMISSION_BACKLOG`.
- Fail readiness when the oldest queued admission exceeds a configured age,
  indicating the worker is not making progress.
- Fail readiness when expired `validating` leases accumulate above a small
  threshold.
- Preserve existing checks for database health, worker heartbeats, and local
  finalization pending.
- `/readyz` output should include durable backlog by status, oldest queued age,
  expired lease count, last successful claim timestamp, and whether new
  admission is currently allowed. Do not expose the old in-memory queue depth as
  the production admission signal after this migration.

This directly addresses the report's concern that current readiness can miss
payloads drained into in-memory `pendingPayloads`:
[`PRODUCTION_READINESS_REPORT.md:543`](../PRODUCTION_READINESS_REPORT.md#L543).

## Tests And Fault Injection

Unit tests:

- `TxAdmissionsDB.admit` inserts a new queued row and returns timestamps.
- Same `tx_id` and same bytes returns the existing row, increments
  `request_count`, and does not create another row.
- Same `tx_id` and different bytes returns an integrity conflict and leaves the
  original row unchanged.
- Admission rejects new work when durable backlog is at capacity, while still
  returning existing duplicates.
- `claimBatch` claims rows in `arrival_seq` order and uses `SKIP LOCKED` safely
  under concurrent workers.
- Expired `validating` rows become claimable again.
- `markAccepted` and `markRejected` update destination tables and
  `tx_admissions` atomically.
- Stale lease-owner terminal writes roll back without destination side effects.
- Existing accepted/rejected destination rows are idempotent only when bytes and
  metadata match; conflicts fail closed.
- Migration fails closed on cross-table same-`tx_id` byte conflicts, orphan
  rejections, and ambiguous duplicate rejection rows.

Router tests:

- Invalid payloads keep current 400/413 behavior and do not insert admissions.
- Successful `/submit` writes `tx_admissions` before responding.
- Duplicate `/submit` returns the existing durable status.
- Database failure returns 503 and does not enqueue in memory.
- Same id with different bytes returns 409.
- Admission safety gate failure blocks new submissions with 503 but still
  returns matching duplicate status when the database and schema are healthy.

Worker/replay tests:

- `submit -> durable ack -> process crash -> restart -> tx still queued`.
- `claim -> process crash before validation -> lease expires -> row is replayed`.
- `validation infrastructure failure -> row returns to queued with backoff`.
- `accepted validation -> mempool insert and admission status commit together`.
- `rejected validation -> tx_rejections insert and admission status commit together`.
- `lease expires while worker is validating -> stale worker terminal write is
  fenced out; current owner or later replay decides terminal state`.
- No transaction is processed twice into mempool or rejection tables under retry.

Fault-injection tests should include the report's submit crash scenario:
[`PRODUCTION_READINESS_REPORT.md:1429`](../PRODUCTION_READINESS_REPORT.md#L1429).
Inject failures after durable admission commit, after claim, during validation,
and during terminal persistence. Each test must verify restart behavior from
PostgreSQL state alone.

## Rollout Steps

1. Add explicit migration support if it is not already present.
2. Add and run the `tx_admissions` migration against a database snapshot in CI.
3. Add the `TxAdmissionsDB` adapter and unit tests.
4. Add explicit durable admission configuration and admission-readiness gates.
5. Change `/submit` to call durable admission and remove authoritative use of
   `Queue.dropping`.
6. Change `tx-queue-processor` to claim from `tx_admissions`; remove
   `pendingPayloads` and `nextArrivalSeq` scheduling state.
7. Add lease-fenced atomic accepted/rejected terminal update helpers.
8. Extend `GET /tx-status` with `queued` and `validating`.
9. Extend readiness and metrics with durable backlog signals.
10. Run integration and fault-injection tests against Postgres.
11. Deploy to a staging node with empty and non-empty databases. Verify
    migration, duplicate submission, crash replay, readiness, and metrics.
12. Deploy to production only after the node rejects traffic when migration or
    database health is invalid.

## Risks And Open Questions

- Existing `tx_rejections` rows lack transaction bytes. The safest migration is
  fail-closed unless those rows can be paired with durable bytes. This may require
  operators to start from a clean state or restore a snapshot with enough data.
- Exact atomicity between `tx_admissions` and all current mempool side effects
  depends on wrapping the existing multi-table mempool writes in one SQL
  transaction. This P0 should do that for validation terminal updates; broader
  cross-store atomicity remains covered by the next P0 blocker.
- `GET /tx-status` currently prioritizes rejection before committed state in
  `resolveTxStatus`. This plan adds queued/validating states, but the full
  canonical status cleanup should be coordinated with the report's separate
  canonical-status finding.
- Durable backlog counting in the hot `/submit` path is correct but may become
  expensive. Correctness wins for this P0; optimize later with a durable counter
  only if tests prove it is needed.
- Lease duration must exceed the worst expected validation batch duration while
  still allowing bounded recovery. Start with a conservative configuration,
  renew leases for long batches, and alert on expired leases rather than
  silently extending forever.

## Concrete Implementation Checklist

- [ ] Add migration runner or use the repository's explicit migration framework.
- [ ] Add migration for `tx_admission_status` and `tx_admissions`.
- [ ] Add fail-closed migration/backfill validation for preexisting tx state.
- [ ] Add unique rejection evidence semantics for `tx_rejections(tx_id)` or an
      equivalent exact-once rejection table covered by the same migration.
- [ ] Add `src/database/txAdmissions.ts`.
- [ ] Export `TxAdmissionsDB` from `src/database/index.ts`.
- [ ] Add `TxAdmissionsDB.admit` with same-bytes idempotency and different-bytes
      conflict detection.
- [ ] Add durable admission configuration and backlog capacity checks for new
      admissions.
- [ ] Add the admission-readiness guard for new `/submit` admissions while
      preserving duplicate lookup when safe.
- [ ] Change `/submit` to return only durable admission state.
- [ ] Remove `Queue.dropping` as the authoritative submission path from
      `listen.ts` and `listen-router.ts`; `buildListenRouter` and
      `txQueueProcessorFiber` should no longer require a queue parameter for
      production admission.
- [ ] Change `tx-queue-processor` to claim batches from `tx_admissions`.
- [ ] Replace module-level arrival sequencing with durable `arrival_seq`.
- [ ] Add lease expiry and retry/backoff handling.
- [ ] Add lease-owner fencing to terminal writes, retry release, and optional
      lease extension.
- [ ] Wrap accepted terminal writes and admission status update in one fenced SQL
      transaction that includes all mempool, ledger, delta, deposit, and address
      history side effects.
- [ ] Wrap rejected terminal writes and admission status update in one fenced SQL
      transaction with exact-once rejection evidence.
- [ ] Extend `GET /tx-status` to expose `queued` and `validating`.
- [ ] Extend readiness to use durable backlog, oldest queued age, and expired
      leases.
- [ ] Add metrics and structured logs for admission and replay.
- [ ] Add unit tests for admission, duplicates, conflicts, claims, leases, and
      terminal updates.
- [ ] Add router tests for durable ack and no in-memory fallback.
- [ ] Add fault-injection restart tests for ack-before-crash and claim replay.
- [ ] Run the full node test suite and a staging crash-replay rehearsal before
      production rollout.
