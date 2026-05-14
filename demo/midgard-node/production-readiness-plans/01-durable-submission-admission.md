# P0 Blocker 1: Durable, Idempotent Submission Admission Before Ack

Status: mostly landed in the current implementation. Remaining work is cleanup,
additional readiness gating, and test coverage.

## Current State Summary

`POST /submit` no longer acknowledges by offering a payload to an in-memory
Effect queue. The public submit path now accepts raw `application/cbor`, validates
and normalizes the submitted Midgard-native transaction envelope, and calls
`TxAdmissionsDB.admit` before returning success:
[`src/commands/listen-router.ts:1013`](../src/commands/listen-router.ts#L1013),
[`src/commands/listen-router.ts:1037`](../src/commands/listen-router.ts#L1037),
[`src/commands/listen-router.ts:1050`](../src/commands/listen-router.ts#L1050),
[`src/commands/listen-router.ts:1075`](../src/commands/listen-router.ts#L1075).

The durable source of truth is `tx_admissions`, introduced by migration
`0002_durable_tx_admissions.sql` and registered as schema version 2 in the
explicit migration manifest:
[`src/database/migrations/sql/0002_durable_tx_admissions.sql:13`](../src/database/migrations/sql/0002_durable_tx_admissions.sql#L13),
[`src/database/migrations/sql/0002_durable_tx_admissions.sql:20`](../src/database/migrations/sql/0002_durable_tx_admissions.sql#L20),
[`src/database/migrations/index.ts:28`](../src/database/migrations/index.ts#L28).

The old queue object is still constructed and passed through `runNode`,
`buildListenRouter`, and `txQueueProcessorFiber`, but it is a legacy parameter.
The current submit handler and processor name it `_txQueue` and do not offer to
or drain it:
[`src/commands/listen.ts:68`](../src/commands/listen.ts#L68),
[`src/commands/listen-router.ts:997`](../src/commands/listen-router.ts#L997),
[`src/fibers/tx-queue-processor.ts:347`](../src/fibers/tx-queue-processor.ts#L347).

## Landed Behavior

### Schema and Migration

The explicit migration creates:

- `tx_admission_status` with `queued`, `validating`, `accepted`, and `rejected`;
- `tx_admissions` with canonical transaction bytes, a byte hash,
  `arrival_seq`, lifecycle timestamps, lease fields, retry fields, rejection
  metadata, submit source, and request count;
- dequeue, status, and lease indexes;
- `uniq_tx_rejections_tx_id` for exact-once rejection evidence.

The migration fails closed if older transaction state already exists without an
audited backfill path:
[`src/database/migrations/sql/0002_durable_tx_admissions.sql:1`](../src/database/migrations/sql/0002_durable_tx_admissions.sql#L1).
That matches the repository directive: this pre-launch node should not preserve
legacy local state through compatibility shims.

Startup no longer creates or repairs application schema. `InitDB.program`
asserts that explicit migrations have already brought the database to the exact
schema supported by this binary:
[`src/database/init.ts:7`](../src/database/init.ts#L7).

### Admission Repository

`src/database/txAdmissions.ts` now provides the durable admission adapter:

- `admit` locks an existing row by `tx_id`, compares both hash and bytes for
  idempotency, rejects same-id/different-byte submissions, checks non-terminal
  backlog capacity, and inserts a new `queued` row:
  [`src/database/txAdmissions.ts:129`](../src/database/txAdmissions.ts#L129),
  [`src/database/txAdmissions.ts:147`](../src/database/txAdmissions.ts#L147),
  [`src/database/txAdmissions.ts:181`](../src/database/txAdmissions.ts#L181),
  [`src/database/txAdmissions.ts:199`](../src/database/txAdmissions.ts#L199).
- `getByTxId` reads one admission row for status resolution:
  [`src/database/txAdmissions.ts:244`](../src/database/txAdmissions.ts#L244).
- `requeueExpiredLeases`, `claimBatch`, and `releaseForRetry` implement durable
  replay and lease-fenced retry:
  [`src/database/txAdmissions.ts:221`](../src/database/txAdmissions.ts#L221),
  [`src/database/txAdmissions.ts:258`](../src/database/txAdmissions.ts#L258),
  [`src/database/txAdmissions.ts:296`](../src/database/txAdmissions.ts#L296).
- `markAccepted` and `markRejected` transition rows under the active
  `validating` lease and verify exact row counts:
  [`src/database/txAdmissions.ts:328`](../src/database/txAdmissions.ts#L328),
  [`src/database/txAdmissions.ts:374`](../src/database/txAdmissions.ts#L374).
- `countBacklog` and `oldestQueuedAgeMs` feed readiness and metrics:
  [`src/database/txAdmissions.ts:459`](../src/database/txAdmissions.ts#L459),
  [`src/database/txAdmissions.ts:469`](../src/database/txAdmissions.ts#L469).

`extendLease` and `countByStatus` are not present yet.

### Submit API

The current success responses are:

- new durable admission: `202` with `status`, `firstSeenAt`, `lastSeenAt`, and
  `duplicate: false`;
- identical duplicate: `200` with the existing durable status and
  `duplicate: true`.

Current failure behavior:

- missing or wrong content type returns `415`;
- empty or malformed payload returns `400`;
- oversized payload returns `413`;
- same `tx_id` with different normalized bytes returns `409` and
  `E_TX_ID_BYTES_CONFLICT`;
- durable backlog at capacity returns `503`;
- database admission failure returns a server error from `failWith500`.

The submit path only accepts Midgard-native transaction-envelope CBOR. Tests
cover raw payload size validation and rejection of ordinary Cardano transaction
bytes:
[`tests/listen-admission-auth.test.ts:103`](../tests/listen-admission-auth.test.ts#L103),
[`tests/listen-admission-auth.test.ts:129`](../tests/listen-admission-auth.test.ts#L129),
[`tests/listen-admission-auth.test.ts:191`](../tests/listen-admission-auth.test.ts#L191).

### Worker Replay

The tx processor now uses `tx_admissions` as the source of truth:

- requeues expired leases;
- counts durable backlog;
- pauses while local finalization recovery is pending;
- claims rows with a lease owner;
- converts durable admission rows into `QueuedTx` using `arrival_seq` and
  `first_seen_at`;
- runs existing phase-A and phase-B validation;
- marks accepted or rejected rows through `TxAdmissionsDB`;
- releases claimed rows for retry on infrastructure failure.

Relevant code:
[`src/fibers/tx-queue-processor.ts:365`](../src/fibers/tx-queue-processor.ts#L365),
[`src/fibers/tx-queue-processor.ts:390`](../src/fibers/tx-queue-processor.ts#L390),
[`src/fibers/tx-queue-processor.ts:408`](../src/fibers/tx-queue-processor.ts#L408),
[`src/fibers/tx-queue-processor.ts:431`](../src/fibers/tx-queue-processor.ts#L431),
[`src/fibers/tx-queue-processor.ts:488`](../src/fibers/tx-queue-processor.ts#L488),
[`src/fibers/tx-queue-processor.ts:504`](../src/fibers/tx-queue-processor.ts#L504),
[`src/fibers/tx-queue-processor.ts:530`](../src/fibers/tx-queue-processor.ts#L530).

### Status and Readiness Integration

`GET /tx-status` now reads `tx_admissions` and resolves `validating` and
`queued` after committed, pending-commit, accepted, and rejected states:
[`src/commands/listen-router.ts:355`](../src/commands/listen-router.ts#L355),
[`src/commands/tx-status.ts:100`](../src/commands/tx-status.ts#L100).

`GET /readyz` uses durable admission backlog instead of the Effect queue as the
readiness queue-depth input, includes durable oldest queued age, and still
returns the legacy in-memory queue depth as diagnostic output:
[`src/commands/listen-router.ts:454`](../src/commands/listen-router.ts#L454),
[`src/commands/listen-router.ts:493`](../src/commands/listen-router.ts#L493),
[`src/commands/listen-router.ts:526`](../src/commands/listen-router.ts#L526).

Configuration for durable admission and validation leases has landed:
[`src/services/config.ts:198`](../src/services/config.ts#L198),
[`src/services/config.ts:210`](../src/services/config.ts#L210),
[`src/services/config.ts:213`](../src/services/config.ts#L213),
[`src/services/config.ts:216`](../src/services/config.ts#L216).

## Remaining Work

- Remove the legacy queue parameter from `runNode`, `buildListenRouter`,
  `postSubmitHandler`, and `txQueueProcessorFiber`, and update stale comments
  that still describe enqueueing.
- Add the shared `requireAdmissionReady` guard for new `/submit` admissions.
  Today the admission transaction enforces durable backlog capacity, but
  `/submit` does not reject new work merely because `/readyz` would fail due to
  stale worker heartbeats, local finalization pending, unfinished local mutation
  jobs, or unresolved block submission age.
- Add `extendLease`, `countByStatus`, explicit expired-lease readiness exposure,
  and bounded exponential retry using `VALIDATION_RETRY_BACKOFF_MAX_MS`.
- Rename or split legacy metrics whose names still imply an in-memory queue when
  the sampled value is now durable backlog.
- Strengthen accepted-terminal idempotency checks around destination-table
  conflicts. `markAccepted` wraps `MempoolDB.insertMultiple` and the admission
  status update in a transaction, but the bulk tx insert still leaves existing
  rows unchanged on conflict; explicit byte/effect drift tests are still needed.
- Add direct unit tests for `TxAdmissionsDB.admit`, `claimBatch`,
  lease expiry, `releaseForRetry`, `markAccepted`, and `markRejected`.
- Add router tests proving `/submit` writes `tx_admissions` before responding,
  duplicate submissions are idempotent, byte conflicts return `409`, and backlog
  pressure returns `503` without touching the legacy queue.
- Add restart/fault-injection tests for ack-before-crash, claim-before-crash,
  stale lease terminal writes, and retry after validation infrastructure
  failure.
- Add `GET /tx-status` tests for `queued` and `validating` admission states.

## Updated Checklist

- [x] Explicit migration framework exists and startup verifies schema instead of
      silently creating application tables.
- [x] Migration `0002_durable_tx_admissions` creates `tx_admissions` and
      fail-closed rejection uniqueness.
- [x] `TxAdmissionsDB` is exported from `src/database/index.ts`.
- [x] `TxAdmissionsDB.admit` implements same-byte duplicate semantics,
      different-byte conflict detection, and backlog-capacity rejection.
- [x] `/submit` accepts raw `application/cbor`, normalizes native envelope CBOR,
      and returns only after durable admission.
- [x] The tx processor claims and replays work from `tx_admissions`.
- [x] Lease-fenced `markAccepted`, `markRejected`, and `releaseForRetry` exist.
- [x] `GET /tx-status` includes durable `queued` and `validating` states.
- [x] `/readyz` uses durable backlog and oldest queued age.
- [ ] Remove the legacy Effect queue from production wiring.
- [ ] Add the shared readiness/admission guard before new durable admissions.
- [ ] Add missing repository helpers and readiness details for expired leases
      and by-status admission counts.
- [ ] Complete dedicated unit, router, integration, and crash-replay tests.
