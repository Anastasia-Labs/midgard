# P0 Blocker 7: Production Readiness Means Safe To Admit Traffic

Status: partially landed. `/readyz` is now durable-admission-aware, but the full
production readiness contract, versioned response schema, dependency probes, and
shared `/submit` readiness guard are still open.

## Current Readiness Contract

`GET /healthz` remains process liveness. `GET /readyz` returns HTTP `200` when
the current evaluator has no blocking reasons and `503` otherwise:
[`src/commands/listen-router.ts:536`](../src/commands/listen-router.ts#L536).

The current response is still a compact, unversioned shape:

```json
{
  "ready": false,
  "reasons": [
    "queue_depth_exceeded:12001:12000",
    "durable_admission_oldest_age_exceeded:130000:120000"
  ],
  "durableAdmissionBacklog": "12001",
  "durableAdmissionOldestAgeMs": 130000,
  "unfinishedLocalMutationJobs": "0",
  "unresolvedBlockSubmissionAgeMs": 0,
  "legacyInMemoryQueueDepth": 0
}
```

Reason codes are strings, and some reasons embed dynamic values in the string.
The schema-v1 structured response described by the older plan has not landed.

## Signals Landed in `/readyz`

The handler now samples:

- legacy in-memory queue depth, only for diagnostics:
  [`src/commands/listen-router.ts:454`](../src/commands/listen-router.ts#L454);
- durable admission backlog from `tx_admissions`:
  [`src/commands/listen-router.ts:455`](../src/commands/listen-router.ts#L455);
- oldest queued durable admission age:
  [`src/commands/listen-router.ts:456`](../src/commands/listen-router.ts#L456);
- unfinished local mutation jobs:
  [`src/commands/listen-router.ts:457`](../src/commands/listen-router.ts#L457);
- worker heartbeat refs for block commitment, block confirmation, merge,
  deposit fetch, withdrawal fetch, and tx queue processing:
  [`src/commands/listen-router.ts:459`](../src/commands/listen-router.ts#L459);
- `LOCAL_FINALIZATION_PENDING`:
  [`src/commands/listen-router.ts:475`](../src/commands/listen-router.ts#L475);
- unresolved submitted-block age from process globals:
  [`src/commands/listen-router.ts:478`](../src/commands/listen-router.ts#L478);
- a PostgreSQL `SELECT 1` probe:
  [`src/commands/listen-router.ts:490`](../src/commands/listen-router.ts#L490).

The pure evaluator currently blocks on:

- `db_unhealthy`;
- stale worker heartbeat, reported as `stale_heartbeat:<worker>:<age>`;
- queue depth over threshold. The handler now passes durable admission backlog
  as this queue-depth value, so `queue_depth_exceeded` means durable admission
  backlog exceeded `READINESS_MAX_DURABLE_ADMISSION_BACKLOG`;
- `local_finalization_pending`;
- unresolved block submission age over `UNCONFIRMED_BLOCK_MAX_AGE_MS`.

Code:
[`src/commands/readiness.ts:16`](../src/commands/readiness.ts#L16),
[`src/commands/readiness.ts:43`](../src/commands/readiness.ts#L43),
[`src/commands/readiness.ts:57`](../src/commands/readiness.ts#L57),
[`src/commands/readiness.ts:64`](../src/commands/readiness.ts#L64),
[`src/commands/readiness.ts:69`](../src/commands/readiness.ts#L69),
[`src/commands/readiness.ts:73`](../src/commands/readiness.ts#L73).

The router adds two checks outside the pure evaluator:

- `durable_admission_oldest_age_exceeded:<observed>:<threshold>`;
- `unfinished_local_mutation_jobs:<count>`.

Code:
[`src/commands/listen-router.ts:513`](../src/commands/listen-router.ts#L513),
[`src/commands/listen-router.ts:521`](../src/commands/listen-router.ts#L521).

The response includes the observed durable backlog, durable oldest queued age,
unfinished mutation-job count, unresolved block submission age, and legacy queue
depth:
[`src/commands/listen-router.ts:526`](../src/commands/listen-router.ts#L526).
It does not include raw heartbeat ages unless a heartbeat is stale and appears
inside a reason string.

## Submit Admission Relationship

Durable admission has landed, so `/submit` now writes `tx_admissions` before
returning success:
[`src/commands/listen-router.ts:1075`](../src/commands/listen-router.ts#L1075).
The admission transaction enforces the hard durable backlog cap:
[`src/database/txAdmissions.ts:181`](../src/database/txAdmissions.ts#L181).

The shared `requireAdmissionReady` guard has not landed. A new `/submit` can
still be durably admitted when `/readyz` would be `503` for stale worker
heartbeat, local finalization pending, unfinished mutation jobs, or unresolved
block-submission age. That is a remaining production-readiness gap. The durable
admission insert is still the authoritative capacity and idempotency boundary,
but readiness is not yet used as an early public-write gate.

The legacy Effect queue is no longer authoritative. It is still constructed and
threaded through the router and processor:
[`src/commands/listen.ts:68`](../src/commands/listen.ts#L68),
[`src/commands/listen-router.ts:1142`](../src/commands/listen-router.ts#L1142),
[`src/commands/listen-router.ts:1194`](../src/commands/listen-router.ts#L1194),
[`src/fibers/tx-queue-processor.ts:347`](../src/fibers/tx-queue-processor.ts#L347).
The submit handler and processor ignore it as `_txQueue`.

## What Readiness Still Does Not Prove

Current `/readyz` is a useful admission-safety signal, but it is not yet the full
production contract. It does not yet:

- return a versioned schema with structured reason objects, components,
  observed values, thresholds, freshness metadata, and warning vs blocking
  severity;
- expose migration version or schema-manifest status in the response, although
  startup already verifies schema compatibility before serving;
- run bounded provider, indexer, or reference-script probes;
- inspect deposit cursor lag, deposit fetch freshness beyond heartbeat age, or
  deposit projection failure state;
- compare process unconfirmed-submission refs with durable
  `pending_block_finalizations` journal state;
- derive unresolved submission age from an immutable durable `submitted_at`
  field;
- expose startup integrity state, MPF integrity state, or latched degraded
  events through durable health-state storage;
- include validation infrastructure failure counters, last successful
  validation batch, expired lease counts, retry counts, or backlog by admission
  status;
- use snapshot caching, bounded probe timeouts, stale-snapshot failure, or
  shared in-flight probe suppression;
- block new `/submit` admissions through a shared readiness guard.

## Current Tests

`tests/readiness.test.ts` covers the pure evaluator for healthy input, stale
worker heartbeat, backlog threshold, local finalization pending, and database
probe failure:
[`tests/readiness.test.ts:10`](../tests/readiness.test.ts#L10),
[`tests/readiness.test.ts:34`](../tests/readiness.test.ts#L34),
[`tests/readiness.test.ts:57`](../tests/readiness.test.ts#L57),
[`tests/readiness.test.ts:80`](../tests/readiness.test.ts#L80),
[`tests/readiness.test.ts:103`](../tests/readiness.test.ts#L103).

I did not find dedicated `/readyz` handler tests for the current response shape,
durable backlog fields, unfinished mutation-job response fields, or HTTP
`200`/`503` behavior. I also did not find tests proving `/submit` is blocked by
readiness, because that guard has not landed.

## Remaining Production Contract

The target contract remains:

1. `GET /readyz` means the node is safe to admit new public user traffic now.
2. Readiness success must imply durable admission is available and validation
   infrastructure can make trustworthy progress.
3. Required snapshot data must be current, complete, internally consistent, and
   collected read-only.
4. Blocking reasons must be stable and machine-readable.
5. Readiness must fail closed on unknown startup integrity, schema uncertainty,
   local finalization ambiguity, unresolved L1 submission staleness, provider or
   indexer unavailability, deposit visibility uncertainty, and durable backlog
   exhaustion.
6. `/submit` must use the same readiness decision to reject new admissions, while
   still allowing byte-safe duplicate/status behavior when that can be proven
   without admitting new work.
7. Durable state transitions must remain authoritative; readiness is an early
   reject path, not the only capacity or idempotency control.

## Updated Implementation Plan

### 1. Keep Current Durable Signals

The landed durable admission fields should remain in the response until the
schema-v1 replacement lands:

- `durableAdmissionBacklog`;
- `durableAdmissionOldestAgeMs`;
- `unfinishedLocalMutationJobs`;
- `unresolvedBlockSubmissionAgeMs`;
- `legacyInMemoryQueueDepth` while the old queue is still wired.

Do not regress to memory-only queue depth. The durable backlog must remain the
queue-depth input for readiness.

### 2. Add Structured Readiness Types

Replace string reasons with versioned reason objects:

```ts
type ReadinessReason = {
  code: string;
  severity: "blocking" | "warning";
  component: string;
  detail: string;
  observed?: number | string | boolean;
  threshold?: number | string;
  unit?: "ms" | "count" | "bytes";
  since?: string;
  recoverable: boolean;
};
```

Preserve stable codes such as `db_unhealthy`,
`worker_heartbeat_stale`, `durable_admission_backlog_exceeded`,
`durable_admission_oldest_age_exceeded`,
`unfinished_local_mutation_jobs`, `local_finalization_pending`, and
`unresolved_block_submission_stale`.

### 3. Split Snapshot Collection From Evaluation

Keep the evaluator pure. Move effectful collection into a snapshot collector
that gathers:

- DB probe plus migration/schema status;
- worker heartbeat ages;
- durable admission backlog, oldest age, expired lease count, and status counts;
- unfinished local mutation jobs;
- active pending block-finalization journal plus process refs;
- local finalization state and age;
- validation infrastructure status;
- deposit fetch, cursor, and projection status;
- provider, indexer, and reference-script probes.

All probes must be read-only, bounded by timeouts, and represented as failed or
skipped checks in the snapshot rather than mutating state from readiness.

### 4. Add Admission Guard

Add `requireAdmissionReady` for new `/submit` admissions. It should use the same
snapshot/evaluation result as `/readyz`, reject new work with `503` when any
blocking reason exists, and never fall back to the legacy queue.

The guard must preserve the durable transaction's own protections:

- same `tx_id` plus same normalized bytes remains idempotent;
- same `tx_id` plus different normalized bytes remains `409`;
- durable backlog capacity is enforced in SQL even if a readiness sample was
  briefly green.

### 5. Add Durable Health State

Introduce a small durable health-state table for latched production failures
that must survive restart, such as startup integrity failure, MPF integrity
failure, pending L1 journal inconsistency, local finalization failure, deposit
projection payload mismatch, or admission byte conflict. Clearing these states
must be explicit, auditable, and reason-specific.

### 6. Add Handler and Fault Tests

Add tests for:

- current `/readyz` response fields and HTTP status;
- structured schema-v1 response once implemented;
- durable backlog threshold and oldest-age threshold;
- unfinished local mutation jobs;
- unresolved submission age;
- stale worker heartbeat reason normalization;
- provider/indexer timeout behavior;
- DB outage behavior;
- stale snapshot fail-closed behavior;
- `/submit` rejecting new admissions while readiness is blocking;
- duplicate same-byte submission behavior while degraded;
- restart with durable queued rows.

## Updated Checklist

- [x] `/readyz` evaluates DB health.
- [x] `/readyz` evaluates worker heartbeats.
- [x] `/readyz` evaluates local finalization pending.
- [x] `/readyz` evaluates unresolved block submission age from process refs.
- [x] `/readyz` uses durable admission backlog instead of authoritative memory
      queue depth.
- [x] `/readyz` includes durable oldest queued admission age.
- [x] `/readyz` includes unfinished local mutation-job count.
- [x] `/readyz` includes legacy in-memory queue depth as diagnostic output.
- [ ] Replace string reasons with versioned structured reasons.
- [ ] Include migration/schema status in readiness output.
- [ ] Add provider, indexer, and reference-script probes with timeouts.
- [ ] Add deposit cursor/projection readiness.
- [ ] Cross-check process L1 submission refs against durable pending
      finalization journal state.
- [ ] Add immutable durable pending-submission age fields.
- [ ] Add validation infrastructure readiness state.
- [ ] Add durable health-state storage for latched degraded reasons.
- [ ] Add snapshot freshness/cache behavior and stale-snapshot fail-closed rules.
- [ ] Add `requireAdmissionReady` to block new `/submit` admissions when
      readiness is blocking.
- [ ] Remove the legacy Effect queue from router and processor wiring once all
      production paths are durable.
- [ ] Add handler, integration, and fault-injection tests for the production
      readiness contract.
