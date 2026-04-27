# P0 Blocker 7: Production Readiness Means Safe To Admit Traffic

## Problem Statement

`GET /readyz` must answer one production question:

```text
Is this node safe to admit new user traffic right now?
```

Today readiness is a narrow liveness and pressure check. It verifies PostgreSQL
connectivity, in-memory queue depth, worker heartbeat age, and one local
finalization flag. That is not enough for a production L2 admission boundary.
A node can report ready while it has work hidden in process memory, an
unresolved L1 block submission, stale deposit ingestion, provider/indexer
failure, pending local recovery, or startup integrity uncertainty.

The target behavior is fail-closed readiness. If the node cannot prove that it
can durably accept, validate, progress, and recover new submissions without
weakening state integrity, `/readyz` returns `503` with machine-readable evidence.
This plan is intentionally limited to implementation planning. It does not
implement code.

This plan implements the blocker described in
[`PRODUCTION_READINESS_REPORT.md:524`](../PRODUCTION_READINESS_REPORT.md#L524)
and belongs after durable admission, startup integrity, and migration work in
the recommended ordering:
[`PRODUCTION_READINESS_REPORT.md:1562`](../PRODUCTION_READINESS_REPORT.md#L1562).

## Current Behavior

The current readiness evaluator accepts only these inputs:
`nowMillis`, heartbeat limits, queue depth, worker heartbeats,
`localFinalizationPending`, and `dbHealthy`:
[`src/commands/readiness.ts:15`](../src/commands/readiness.ts#L15).

`evaluateReadiness` fails when:

- the database probe failed:
  [`src/commands/readiness.ts:40`](../src/commands/readiness.ts#L40);
- any worker heartbeat is older than `maxHeartbeatAgeMs`:
  [`src/commands/readiness.ts:44`](../src/commands/readiness.ts#L44);
- `queueDepth` exceeds `maxQueueDepth`:
  [`src/commands/readiness.ts:60`](../src/commands/readiness.ts#L60);
- `LOCAL_FINALIZATION_PENDING` is true:
  [`src/commands/readiness.ts:65`](../src/commands/readiness.ts#L65).

The `/readyz` handler samples the in-memory Effect queue size, worker heartbeat
refs, `LOCAL_FINALIZATION_PENDING`, and `SELECT 1`:
[`src/commands/listen-router.ts:440`](../src/commands/listen-router.ts#L440).
It returns only `{ ready, reasons }` with status `200` or `503`:
[`src/commands/listen-router.ts:484`](../src/commands/listen-router.ts#L484).

The queue depth input is incomplete. `POST /submit` offers normalized payloads
to an in-memory queue:
[`src/commands/listen-router.ts:1032`](../src/commands/listen-router.ts#L1032).
The tx queue processor drains that queue into module-level `pendingPayloads`:
[`src/fibers/tx-queue-processor.ts:123`](../src/fibers/tx-queue-processor.ts#L123),
[`src/fibers/tx-queue-processor.ts:387`](../src/fibers/tx-queue-processor.ts#L387).
After drain, `/readyz` can observe a low queue size even while many payloads are
still pending validation in memory. The worker does emit `validation_queue_depth`
and `validation_oldest_queued_tx_age_ms`, but those metrics are not wired into
readiness:
[`src/fibers/tx-queue-processor.ts:66`](../src/fibers/tx-queue-processor.ts#L66),
[`src/fibers/tx-queue-processor.ts:87`](../src/fibers/tx-queue-processor.ts#L87).
The underlying `pendingPayloads` array is module-local:
[`src/fibers/tx-queue-processor.ts:123`](../src/fibers/tx-queue-processor.ts#L123).
Production readiness must expose worker status through an explicit snapshot/ref
or durable admission state; it must not reach into private worker globals as an
implicit API.

The worker pauses when `LOCAL_FINALIZATION_PENDING` is true:
[`src/fibers/tx-queue-processor.ts:367`](../src/fibers/tx-queue-processor.ts#L367),
[`src/fibers/tx-queue-processor.ts:378`](../src/fibers/tx-queue-processor.ts#L378).
Readiness sees that one flag, but it does not include the exact recovery state,
active pending journal status, or age.

Process-global state already tracks unresolved L1 block submission:
`UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH` and
`UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS`:
[`src/services/globals.ts:52`](../src/services/globals.ts#L52). Commitment
sets those refs after successful or deferred submission:
[`src/fibers/block-commitment.ts:143`](../src/fibers/block-commitment.ts#L143),
[`src/fibers/block-commitment.ts:177`](../src/fibers/block-commitment.ts#L177),
[`src/fibers/block-commitment.ts:199`](../src/fibers/block-commitment.ts#L199).
Confirmation clears them on confirmed or abandoned outcomes:
[`src/fibers/block-confirmation.ts:289`](../src/fibers/block-confirmation.ts#L289),
[`src/fibers/block-confirmation.ts:314`](../src/fibers/block-confirmation.ts#L314).
`/logGlobals` and queue metrics compute the age, but `/readyz` does not:
[`src/commands/listen-router.ts:836`](../src/commands/listen-router.ts#L836),
[`src/fibers/queue-metrics.ts:58`](../src/fibers/queue-metrics.ts#L58).

Durable pending block state exists in `pending_block_finalizations` with active
statuses:
[`src/database/pendingBlockFinalizations.ts:30`](../src/database/pendingBlockFinalizations.ts#L30).
The table is constrained to one active row:
[`src/database/pendingBlockFinalizations.ts:161`](../src/database/pendingBlockFinalizations.ts#L161),
and `retrieveActive` returns the active journal:
[`src/database/pendingBlockFinalizations.ts:183`](../src/database/pendingBlockFinalizations.ts#L183).
Startup hydrates in-memory unconfirmed and local-finalization refs from that
row, using `updated_at` as the unconfirmed age source:
[`src/commands/listen-startup.ts:211`](../src/commands/listen-startup.ts#L211),
[`src/commands/listen-startup.ts:230`](../src/commands/listen-startup.ts#L230).
The production-readiness report separately identifies mutable `updated_at` as
insufficient for stuck-submission age:
[`PRODUCTION_READINESS_REPORT.md:1103`](../PRODUCTION_READINESS_REPORT.md#L1103).

Deposit ingestion has a process heartbeat and last successful fetch timestamp:
[`src/services/globals.ts:58`](../src/services/globals.ts#L58).
The fetch worker updates both after successful reconciliation:
[`src/fibers/fetch-and-insert-deposit-utxos.ts:217`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L217).
The fetch and projection fibers log and suppress steady-state failures:
[`src/fibers/fetch-and-insert-deposit-utxos.ts:257`](../src/fibers/fetch-and-insert-deposit-utxos.ts#L257),
[`src/fibers/project-deposits-to-mempool-ledger.ts:121`](../src/fibers/project-deposits-to-mempool-ledger.ts#L121).
Startup currently also suppresses deposit catch-up and projection failures:
[`src/commands/listen.ts:73`](../src/commands/listen.ts#L73),
[`src/commands/listen.ts:81`](../src/commands/listen.ts#L81).

There is a durable deposit cursor table with stable L1 tip and scan upper bound
fields:
[`src/database/depositIngestionCursor.ts:10`](../src/database/depositIngestionCursor.ts#L10).
Readiness does not inspect it.

Provider construction supports `Kupmios` and `Blockfrost`:
[`src/services/config.ts:12`](../src/services/config.ts#L12).
Lucid wires the configured provider at startup:
[`src/services/lucid.ts:685`](../src/services/lucid.ts#L685).
Blockfrost has fallback-key and Koios fallback wrappers:
[`src/services/lucid.ts:703`](../src/services/lucid.ts#L703),
[`src/services/lucid.ts:720`](../src/services/lucid.ts#L720).
Readiness does not perform any bounded provider/indexer probe.

Reference scripts are verified during startup:
[`src/commands/listen-startup.ts:58`](../src/commands/listen-startup.ts#L58).
`POST /deposit/build` fetches deposit reference scripts on demand:
[`src/commands/listen-router.ts:918`](../src/commands/listen-router.ts#L918).
Readiness does not expose reference-script availability after startup.

MPT initialization currently seeds genesis when the ledger trie root is empty:
[`src/workers/utils/mpt.ts:100`](../src/workers/utils/mpt.ts#L100).
Block construction later refuses to build past a deposit visibility barrier:
[`src/workers/utils/mpt.ts:350`](../src/workers/utils/mpt.ts#L350).
Readiness has no explicit startup integrity or MPT integrity input.

## Target Readiness Contract

`GET /healthz` remains process liveness: the HTTP server can answer.
`GET /readyz` becomes admission safety: load balancers may send user traffic,
including `POST /submit`, only when this endpoint returns `200`.

The production contract:

1. Ready means the node can safely admit new public user traffic now.
2. Readiness success must imply durable admission is available. No in-memory
   queue can be the source of truth for accepted work.
3. Readiness must fail closed for startup integrity failure or unknown startup
   integrity.
4. Readiness must fail while local finalization recovery is pending, failed, or
   ambiguous.
5. Readiness must fail while an active L1 submission is unresolved beyond the
   configured safe window, or when its durable journal state is inconsistent
   with process refs.
6. Readiness must fail when provider/indexer access needed for admission and
   near-term progress is unhealthy or stale.
7. Readiness must fail when deposit ingestion has not proven visibility through
   the required barrier for current admission/commit semantics.
8. Readiness must fail when durable admission or validation backlog exceeds
   configured capacity or age thresholds.
9. Readiness must fail when validation infrastructure cannot make trustworthy
   progress, including repeated provider/local evaluation infrastructure
   failures.
10. Readiness must be machine-readable, stable enough for automation, and rich
    enough for operators to identify the blocking dependency.
11. Readiness must fail closed when its dependency snapshot is stale, incomplete,
    internally inconsistent, or collected from probes older than their configured
    maximum age.
12. Readiness is an early admission signal, not the only capacity control. The
    durable admission insert must still enforce capacity and idempotency inside
    PostgreSQL so concurrent submissions cannot race past a green readiness
    sample.

Readiness is not a promise that every future transaction will validate. It is a
promise that the node is in a safe state to accept responsibility for new work
and has the dependencies required to progress or explicitly reject it.

## Degraded State Model

Introduce a single production readiness state model. It can be implemented as a
new module such as `src/commands/readiness-state.ts`, backed by durable tables
and process refs.

Top-level states:

- `ready`: all admission-safety gates pass.
- `not_ready`: one or more gates fail, but the node may recover automatically.
- `degraded`: the node is serving read-only/status endpoints but must not admit
  new public writes.
- `recovering`: recovery is actively running and admission is blocked.
- `failed`: recovery or integrity failed and explicit operator action is
  required.
- `unknown`: a required signal is missing or stale; fail closed.

State derivation must be deterministic. Suggested precedence:

1. `failed` for latched integrity/recovery failures requiring operator action.
2. `recovering` for an active recovery worker that intentionally blocks writes.
3. `degraded` for blocking dependency/backlog states where read-only/status
   serving is still safe.
4. `not_ready` for short-lived auto-recoverable blocking states.
5. `unknown` when any required signal is absent or stale and no more specific
   blocking state applies.
6. `ready` only when no blocking reason exists.

Degraded reason categories:

- `startup_integrity_unknown`
- `startup_integrity_failed`
- `schema_migration_not_current`
- `db_unhealthy`
- `provider_unhealthy`
- `indexer_unhealthy`
- `reference_scripts_unavailable`
- `deposit_ingestion_stale`
- `deposit_projection_failed`
- `durable_admission_backlog_exceeded`
- `durable_admission_oldest_age_exceeded`
- `validation_backlog_exceeded`
- `validation_oldest_age_exceeded`
- `validation_infrastructure_degraded`
- `worker_heartbeat_stale`
- `local_finalization_pending`
- `local_finalization_failed`
- `pending_l1_submission_active`
- `pending_l1_submission_stale`
- `pending_l1_submission_inconsistent`
- `pending_merge_active`
- `mpt_integrity_unknown`
- `mpt_integrity_failed`
- `reset_in_progress`

Each reason must include:

- stable `code`;
- `severity`: `blocking` or `warning`;
- `component`;
- observed value and threshold where applicable;
- `since` when known;
- whether it is expected to auto-recover;
- operator-facing `detail` that does not include secrets or raw tx payloads.

Only `blocking` reasons determine HTTP status. Warnings are returned for
visibility but do not allow shortcuts around blocking conditions.

## Dependency Health Checks

Add a `collectReadinessSnapshot` effect that samples all required dependencies
with bounded timeouts. It must not mutate protocol state.

Snapshot collection rules:

- Probes must be read-only. `/readyz` must never create, migrate, repair,
  abandon, finalize, resubmit, or clear any protocol state.
- Every sampled value must include `observedAt` and, when derived from cached
  probe data, `probeStartedAt`, `probeCompletedAt`, and `ageMs`.
- Cached provider/indexer/reference-script probes are allowed to control load,
  but stale cache data is a blocking `unknown`/dependency reason, not a best
  effort success.
- Concurrent `/readyz` calls should share one in-flight probe per dependency
  instead of stampeding providers. Sharing must not extend the maximum response
  timeout.
- The admission guard may use a cached snapshot only when it is younger than a
  stricter `READINESS_MAX_ADMISSION_SNAPSHOT_AGE_MS`; otherwise it must collect
  or fail closed.
- If a probe is skipped because a required prerequisite failed, the skipped
  check must be represented explicitly in the response as `status: "skipped"`
  with the prerequisite reason, not omitted.

### Database

Keep the existing `SELECT 1` probe:
[`src/commands/listen-router.ts:465`](../src/commands/listen-router.ts#L465),
but extend it to include:

- migration table at expected version;
- required readiness/admission/finalization tables and indexes present;
- ability to read durable admission backlog counts;
- ability to read active pending finalization rows.

Database probe failure is always blocking because durable admission depends on
PostgreSQL.

The database readiness probe must be read-only and must run with statement
timeouts. It must not call existing `createTable`/`createTables` helpers or the
migration runner. Schema mismatch, missing migration metadata, an active
migration lock, or an unversioned database is `schema_migration_not_current` or
`startup_integrity_unknown`, not an opportunity to repair schema from readiness.

### Provider And Indexer

Add explicit provider/indexer probes rather than inferring health from worker
heartbeats.

For both `Kupmios` and `Blockfrost`:

- bounded `currentSlot` or equivalent protocol tip read;
- bounded `getUtxos`/state-queue address read for the configured state queue;
- bounded reference-script address read for runtime scripts;
- bounded transaction lookup only when there is an active pending submitted tx.

For `Kupmios`, treat Ogmios and Kupo as separate dependencies because the
provider is constructed from both endpoints:
[`src/services/lucid.ts:689`](../src/services/lucid.ts#L689).
For `Blockfrost`, expose primary provider health and fallback usage separately;
fallback success can keep readiness `ready`, but the response must include a
warning that the primary provider is degraded.

Provider/indexer checks must have strict timeouts shorter than the Kubernetes or
load-balancer readiness timeout. A hung provider call must produce
`provider_unhealthy`, not a hung `/readyz`.

Provider probe responses must redact URLs containing credentials and API keys.
For Blockfrost fallback paths, the probe must record whether the primary key,
fallback key, or Koios fallback answered. Fallback success may be a warning only
when the fallback provides every capability required for admission, validation,
block commitment, merge, and recovery; partial fallback capability is blocking.

### Reference Scripts

Reuse the startup verification logic as a read-only readiness probe:
[`src/commands/listen-startup.ts:76`](../src/commands/listen-startup.ts#L76).
The probe must verify that runtime scripts required by admission and block
progress are present and match configured contracts. Missing reference scripts
block readiness because `deposit/build`, commit, merge, and recovery can fail
later under load.

### Startup Integrity

Readiness must consume the startup integrity status planned in
`04-startup-fail-closed-integrity.md`, not repeat every expensive check on each
request. Startup should publish a durable/process status:

```ts
type StartupIntegrityState =
  | { status: "ok"; checkedAt: Date; schemaVersion: string }
  | { status: "failed"; checkedAt: Date; code: string; detail: string }
  | { status: "unknown"; detail: string };
```

`unknown` and `failed` are blocking. If startup later discovers integrity drift,
the status must latch to `failed` until explicit recovery clears it.

### Deposit Ingestion And Projection

Use both process and durable signals:

- `HEARTBEAT_DEPOSIT_FETCH`:
  [`src/services/globals.ts:78`](../src/services/globals.ts#L78);
- `LATEST_DEPOSIT_FETCH_TIME`:
  [`src/services/globals.ts:58`](../src/services/globals.ts#L58);
- `deposit_ingestion_cursor.scan_upper_bound_time_ms`:
  [`src/database/depositIngestionCursor.ts:18`](../src/database/depositIngestionCursor.ts#L18);
- deposit projection success/failure status from the projector.

Add a durable or process-latched projection status because the projection fiber
currently suppresses failures:
[`src/fibers/project-deposits-to-mempool-ledger.ts:126`](../src/fibers/project-deposits-to-mempool-ledger.ts#L126).
A recent heartbeat without a successful cursor advance is not enough.

Projection status must distinguish:

- no due deposits found;
- successful projection with `projectedCount`, `reconciledCount`, and
  `completedAt`;
- transient database/provider failure that may auto-recover;
- integrity failure such as a projected payload mismatch, which must latch until
  explicit recovery.

Readiness should compare the cursor's scan upper bound against the local block
boundary/admission visibility requirement. If the node cannot prove deposits are
visible through the safe barrier, readiness fails with
`deposit_ingestion_stale`.

### Local Finalization And L1 Submission

Read both refs and durable pending journal:

- `LOCAL_FINALIZATION_PENDING`:
  [`src/services/globals.ts:70`](../src/services/globals.ts#L70);
- `AVAILABLE_LOCAL_FINALIZATION_BLOCK`:
  [`src/services/globals.ts:37`](../src/services/globals.ts#L37);
- `UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH` and `SINCE_MS`:
  [`src/services/globals.ts:52`](../src/services/globals.ts#L52);
- `PendingBlockFinalizationsDB.retrieveActive()`:
  [`src/database/pendingBlockFinalizations.ts:183`](../src/database/pendingBlockFinalizations.ts#L183).

Refs are operational cache only. Durable pending-finalization state is the
source of truth. If refs and durable state disagree, readiness fails with
`pending_l1_submission_inconsistent`.

### Validation Infrastructure

The tx processor already distinguishes Plutus evaluation infrastructure
failures from script invalidity:
[`src/fibers/tx-queue-processor.ts:153`](../src/fibers/tx-queue-processor.ts#L153).
Expose this as readiness state:

- last validation infrastructure failure time;
- consecutive infrastructure failure count;
- last successful validation batch time;
- oldest pending validation age;
- durable admission lease-expiry/retry counts after durable admission exists.

Repeated infrastructure failures are blocking because the node cannot safely
turn admitted work into accepted/rejected durable outcomes.

The worker must publish this status through an explicit readiness-facing ref or
durable table updated from the same code path that restores batches for retry.
Metrics alone are insufficient because the evaluator needs deterministic,
typed, timestamped state.

## Backlog And Staleness Metrics

Add readiness-specific metrics and use the same sampled values in the response.

Required backlog gauges:

- `readiness_durable_admission_backlog`
- `readiness_durable_admission_oldest_age_ms`
- `readiness_validation_backlog`
- `readiness_validation_oldest_age_ms`
- `readiness_in_memory_submit_queue_depth` during migration only
- `readiness_pending_payloads_depth` until `pendingPayloads` is removed
- `readiness_processed_unsubmitted_txs_count`
- `readiness_processed_unsubmitted_txs_size_bytes`
- `readiness_blocks_in_queue`

Required staleness gauges:

- `readiness_worker_heartbeat_age_ms{worker=...}`
- `readiness_deposit_fetch_age_ms`
- `readiness_deposit_cursor_lag_ms`
- `readiness_provider_probe_age_ms`
- `readiness_indexer_probe_age_ms`
- `readiness_pending_l1_submission_age_ms`
- `readiness_local_finalization_age_ms`

Required counters:

- `readiness_transition_count{from,to}`
- `readiness_blocking_reason_count{code}`
- `readiness_provider_probe_failure_count{provider,dependency}`
- `readiness_validation_infrastructure_failure_count`

Configuration to add through explicit migration/config work:

```ts
READINESS_MAX_DURABLE_ADMISSION_BACKLOG
READINESS_MAX_DURABLE_ADMISSION_AGE_MS
READINESS_MAX_VALIDATION_BACKLOG
READINESS_MAX_VALIDATION_AGE_MS
READINESS_MAX_DEPOSIT_FETCH_AGE_MS
READINESS_MAX_DEPOSIT_CURSOR_LAG_MS
READINESS_MAX_PROVIDER_PROBE_AGE_MS
READINESS_PROVIDER_PROBE_TIMEOUT_MS
READINESS_MAX_ADMISSION_SNAPSHOT_AGE_MS
READINESS_MAX_PENDING_L1_SUBMISSION_AGE_MS
READINESS_MAX_LOCAL_FINALIZATION_AGE_MS
```

Do not reuse `MAX_SUBMIT_QUEUE_SIZE` as the production admission capacity once
durable admission exists. `MAX_SUBMIT_QUEUE_SIZE` is tied to the current
in-memory queue:
[`src/services/config.ts:163`](../src/services/config.ts#L163).
Readiness must use explicit durable capacity thresholds.

## Unresolved L1 Submission Checks

This plan depends on the stuck L1 submission model from the readiness report:
[`PRODUCTION_READINESS_REPORT.md:1103`](../PRODUCTION_READINESS_REPORT.md#L1103).

Current pending block rows have `created_at` and `updated_at`, but no immutable
`submitted_at`:
[`src/database/pendingBlockFinalizations.ts:14`](../src/database/pendingBlockFinalizations.ts#L14).
`markSubmitted` stores the submitted tx hash and updates `updated_at`:
[`src/database/pendingBlockFinalizations.ts:294`](../src/database/pendingBlockFinalizations.ts#L294).
Startup hydrates unconfirmed age from `updated_at`:
[`src/commands/listen-startup.ts:230`](../src/commands/listen-startup.ts#L230).

Implementation plan:

1. Add immutable `submitted_at`, `first_unresolved_at`, `last_checked_at`, and
   `last_check_error` fields to pending finalization records.
2. Set `submitted_at` exactly once when transitioning from
   `pending_submission` to a submitted status.
3. Compute unresolved submission age from `submitted_at`, not `updated_at`.
4. Treat active statuses as readiness-blocking or warning by status:
   - `pending_submission`: blocking if older than a short preparation threshold;
   - `submitted_local_finalization_pending`: blocking;
   - `submitted_unconfirmed`: warning until age threshold, then blocking;
   - `observed_waiting_stability`: blocking unless a recovery worker is actively
     finalizing and age is within a strict bound.
5. If process refs report no unconfirmed submission but the durable active row
   has a submitted tx hash, readiness fails with
   `pending_l1_submission_inconsistent`.
6. If process refs report an unconfirmed hash but no durable active row,
   readiness fails with `pending_l1_submission_inconsistent`.
7. If provider/indexer cannot verify the pending tx or state-queue tip, readiness
   fails as `provider_unhealthy` plus `pending_l1_submission_active`.

Readiness must not abandon, finalize, or mutate pending L1 state. It reports the
condition. Recovery remains in the block-confirmation/local-finalization paths.

## Durable Admission Integration

This readiness blocker should integrate with P0 blocker 1 instead of creating a
parallel queue model.

The existing report calls out that `/submit` currently acknowledges after
offering to memory:
[`PRODUCTION_READINESS_REPORT.md:48`](../PRODUCTION_READINESS_REPORT.md#L48).
The production fix is a durable admission table:
[`PRODUCTION_READINESS_REPORT.md:101`](../PRODUCTION_READINESS_REPORT.md#L101).

Readiness integration requirements:

1. `POST /submit` must call a shared `requireAdmissionReady` guard before
   admitting new tx bytes. This guard must use the same snapshot and blocking
   reasons as `/readyz`.
2. Duplicate/idempotent lookups by tx id should still be allowed under degraded
   readiness if they do not admit new work. Returning current durable status is
   safe; accepting a new tx is not.
3. Durable admission backlog is measured from `tx_admissions`, not the Effect
   queue.
4. Validation backlog includes rows in `queued`, `validating`, expired lease,
   and retryable infrastructure-failure states.
5. While migrating away from the in-memory queue, readiness must include both
   `txQueue.size` and `pendingPayloads.length`. This is temporary migration
   instrumentation only and must be removed once durable admission is the sole
   source of truth.
6. No compatibility mode may keep old in-memory-only admission as production
   behavior.
7. Backlog capacity must be enforced again inside the durable admission
   transaction, using the same thresholds or stricter limits. Readiness is
   allowed to reject early, but it must not be the only protection against
   concurrent requests exceeding capacity.
8. If the readiness snapshot used by `requireAdmissionReady` is stale,
   unavailable, or missing any required gate, new admission returns `503` and
   does not insert a row.

Suggested guard behavior:

```text
if readiness has blocking reason:
  if request is duplicate status lookup only:
    return current durable status
  else:
    reject admission with 503 and readiness reason summary
```

This prevents load balancer lag from admitting traffic after the node has
already detected it is unsafe.

The duplicate/status exception must be byte-safe. If a duplicate submit supplies
the same `tx_id` but different normalized bytes, it is an integrity conflict and
must not be treated as a safe status lookup under degraded readiness.

## Response Schema

Replace the current minimal response:

```json
{
  "ready": false,
  "reasons": ["local_finalization_pending"]
}
```

with a versioned schema:

```json
{
  "schemaVersion": 1,
  "ready": false,
  "state": "degraded",
  "checkedAt": "2026-04-23T12:00:00.000Z",
  "snapshotAgeMs": 250,
  "node": {
    "network": "Preprod",
    "schemaVersion": "007",
    "startupIntegrity": "ok"
  },
  "reasons": [
    {
      "code": "pending_l1_submission_stale",
      "severity": "blocking",
      "component": "block_confirmation",
      "detail": "Submitted block has not resolved within readiness threshold",
      "observed": 900000,
      "threshold": 180000,
      "unit": "ms",
      "since": "2026-04-23T11:45:00.000Z",
      "recoverable": true
    }
  ],
  "checks": {
    "database": { "status": "ok", "latencyMs": 5 },
    "provider": { "status": "ok", "name": "Blockfrost", "latencyMs": 120 },
    "indexer": { "status": "ok", "name": "Blockfrost", "latencyMs": 120 },
    "referenceScripts": { "status": "ok", "verifiedAt": "2026-04-23T11:59:55.000Z" },
    "startupIntegrity": { "status": "ok", "checkedAt": "2026-04-23T11:59:00.000Z" },
    "depositIngestion": {
      "status": "ok",
      "lastFetchAgeMs": 4000,
      "cursorLagMs": 1000
    },
    "admission": {
      "status": "blocked",
      "durableBacklog": 12000,
      "oldestAgeMs": 45000
    },
    "validation": {
      "status": "ok",
      "backlog": 300,
      "oldestAgeMs": 80
    },
    "l1Submission": {
      "status": "stale",
      "txHash": "<64 hex>",
      "ageMs": 900000,
      "journalStatus": "submitted_unconfirmed"
    },
    "localFinalization": { "status": "none" },
    "workers": {
      "blockCommitment": { "status": "ok", "ageMs": 1000 },
      "blockConfirmation": { "status": "ok", "ageMs": 2000 },
      "merge": { "status": "ok", "ageMs": 3000 },
      "depositFetch": { "status": "ok", "ageMs": 4000 },
      "txQueueProcessor": { "status": "ok", "ageMs": 500 }
    }
  }
}
```

Rules:

- `ready: true` only when no blocking reasons exist.
- HTTP status is `200` when `ready: true`, otherwise `503`.
- Keep reason `code` values stable. Do not embed dynamic values in the code.
- Put dynamic values in `observed`, `threshold`, `unit`, `since`, and component
  details.
- Include snapshot/probe freshness metadata so automation can detect stale
  readiness data without parsing free-form details.
- Do not expose secrets, seed phrases, raw tx CBOR, raw provider API keys, or
  excessive payload material.
- Keep `GET /healthz` unchanged except for any existing liveness improvements.

## Implementation Plan

### 1. Define Readiness Types

Extend `src/commands/readiness.ts` from a simple evaluator into a typed
production evaluator:

```ts
type ReadinessReasonCode =
  | "db_unhealthy"
  | "provider_unhealthy"
  | "indexer_unhealthy"
  | "deposit_ingestion_stale"
  | "durable_admission_backlog_exceeded"
  | "validation_backlog_exceeded"
  | "pending_l1_submission_stale"
  | "local_finalization_pending"
  | "startup_integrity_failed"
  | "...";

type ReadinessReason = {
  code: ReadinessReasonCode;
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

Keep the pure evaluator separate from effectful collection so unit tests can
exercise all state combinations deterministically.

### 2. Add Snapshot Collection

Add `collectReadinessSnapshot` near the router or in a dedicated command module.
It should gather:

- database/migration state;
- startup integrity state;
- worker heartbeat ages;
- durable admission backlog and oldest age;
- validation backlog and last success/failure state;
- active pending block finalization row;
- unconfirmed refs and local finalization refs;
- deposit cursor and fetch/projection status;
- provider/indexer/reference-script probe status;
- queue/process-memory metrics during migration.

The collector must use bounded timeouts and return failed checks as snapshot
data rather than throwing, except for catastrophic coding errors. `/readyz`
should remain responsive while dependencies are degraded.

Add a small `ReadinessSnapshotCache` abstraction if provider/indexer probes are
too expensive for every request. The cache must:

- deduplicate concurrent probe work;
- cap total collection time below the HTTP readiness timeout;
- preserve the last successful and last failed result separately;
- expose stale last-success data as stale, not healthy;
- be bypassed or refreshed for admission when the cached result exceeds
  `READINESS_MAX_ADMISSION_SNAPSHOT_AGE_MS`.

### 3. Replace Handler Wiring

Change `getReadinessHandler` from direct sampling and `evaluateReadiness` call
to:

```text
snapshot = collectReadinessSnapshot(...)
readiness = evaluateProductionReadiness(snapshot, config)
return readiness response with 200/503
```

The router registration stays at `/readyz`:
[`src/commands/listen-router.ts:1081`](../src/commands/listen-router.ts#L1081).

### 4. Add Admission Guard

After durable admission exists, route `POST /submit` through the same readiness
decision before inserting a new admission row. This guard belongs before durable
admission for new txs, after parsing enough request data to identify safe
duplicate status checks where applicable.

The guard must never skip validation, never set
`.complete({ localUPLCEval: false })`, and never use an in-memory fallback.

Treat the guard as a precondition, not a transaction boundary. The durable
admission insert still needs its own serializable/idempotent capacity check from
P0 blocker 1. If readiness flips between the guard and insert, the insert-side
capacity and duplicate checks decide; no admitted row may rely on an in-memory
queue for replay.

### 5. Add Latched Degraded State

Some failures should latch until explicit recovery:

- startup integrity failure;
- MPT integrity failure;
- local finalization failure;
- pending L1 submission inconsistent with durable journal;
- tx id bytes conflict in durable admission;
- deposit projection payload mismatch.

Add a small durable `node_degraded_events` or `node_health_state` table through
versioned migration. Process refs alone are not enough because a restart must
not erase a production safety failure.

Recommended durable shape:

```sql
CREATE TABLE node_health_state (
  code TEXT PRIMARY KEY,
  severity TEXT NOT NULL CHECK (severity IN ('blocking', 'warning')),
  component TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('active', 'cleared')),
  first_observed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_observed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  cleared_at TIMESTAMPTZ,
  clear_reason TEXT,
  evidence JSONB NOT NULL DEFAULT '{}'::jsonb
);
```

`evidence` must contain bounded, non-secret facts only: hashes, counts,
timestamps, status names, schema versions, and error classes. Clearing an active
blocking health state must be explicit and auditable unless the reason is
documented as safe to auto-clear after a verified successful operation. Auto
clear rules must be reason-specific; a successful deposit projection must not
clear MPT, startup, pending L1, or admission-integrity failures.

### 6. Add Metrics

Publish metrics from the same snapshot source. Avoid computing one value for
metrics and a different value for `/readyz`.

### 7. Update Runbooks

Add operator guidance for each blocking reason:

- when it appears;
- whether it can auto-recover;
- what evidence to inspect;
- which recovery command or manual procedure is allowed;
- when to escalate instead of clearing state.

## Tests

### Pure Evaluator Unit Tests

Add table-driven tests for `evaluateProductionReadiness`:

- all checks healthy returns `ready: true`, `state: ready`, HTTP `200`;
- database unhealthy blocks;
- stale worker heartbeat blocks with stable `worker_heartbeat_stale` code and
  worker component;
- warning-only provider fallback keeps `ready: true` but includes warning;
- startup integrity unknown/failed blocks;
- local finalization pending blocks;
- pending L1 active under threshold returns warning or configured non-blocking
  state;
- pending L1 over threshold blocks;
- active durable journal and empty process refs blocks as inconsistent;
- process unconfirmed refs and no durable journal blocks as inconsistent;
- deposit fetch heartbeat fresh but cursor stale blocks;
- durable admission backlog over threshold blocks;
- durable admission oldest age over threshold blocks;
- validation infrastructure consecutive failures over threshold block;
- missing, stale, or skipped required snapshot signals block as `unknown`;
- latched `node_health_state` entries force `failed` or `degraded` until cleared;
- reset in progress blocks public admission.

### Handler Tests

Add `/readyz` tests around response shape and status:

- healthy snapshot returns schema version, checks, empty blocking reasons, and
  `200`;
- blocking snapshot returns `503` and structured reason objects;
- dependency probe timeout returns `503` quickly with `provider_unhealthy`;
- stale cached probe data returns `503` with freshness metadata rather than
  reporting the last healthy result;
- skipped dependent probes are present in the response with their prerequisite
  failure;
- no secrets or raw tx CBOR appear in response.

### Durable Admission Integration Tests

After blocker 1 exists:

- when readiness is blocking, new `/submit` returns `503` and does not insert a
  new admission row;
- identical duplicate `/submit` can return existing durable status without
  creating new work;
- backlog at capacity rejects new txs but still reports existing tx status;
- concurrent submissions cannot exceed durable capacity even if they all observe
  the same ready snapshot;
- stale admission-guard snapshot rejects new work without inserting a row;
- duplicate submit with same `tx_id` but different normalized bytes remains an
  integrity conflict while degraded and does not become a status lookup;
- restart with durable queued rows reports backlog from PostgreSQL, not memory.

### L1 Submission And Recovery Tests

- active `submitted_unconfirmed` row under age threshold is reported with tx hash
  and age;
- active `submitted_unconfirmed` over threshold returns
  `pending_l1_submission_stale`;
- startup hydration using durable pending row results in readiness blocking until
  recovery clears it;
- `NoTxForConfirmationOutput` and `FailedConfirmationOutput` paths leave
  readiness not-ready while the active journal remains unresolved:
  [`src/fibers/block-confirmation.ts:332`](../src/fibers/block-confirmation.ts#L332);
- abandoned stale pending block clears readiness only after durable journal and
  refs agree:
  [`src/fibers/block-confirmation.ts:302`](../src/fibers/block-confirmation.ts#L302).

### Deposit Tests

- stale `LATEST_DEPOSIT_FETCH_TIME` blocks;
- fresh fetch time with stale `deposit_ingestion_cursor` blocks;
- projection failure latches degraded state;
- successful projection clears only the projection reason, not unrelated startup
  integrity failures.

### Fault Injection Tests

- provider probe hangs longer than timeout; `/readyz` returns within timeout
  budget;
- database unavailable returns `503` without crashing the handler;
- validation infrastructure failures restore batch for retry and mark readiness
  degraded until a successful batch clears the condition;
- process restart preserves latched degraded events from durable health state.
- clearing a latched health state requires the documented recovery evidence and
  does not clear unrelated active reasons.

## Rollout Steps

1. Land versioned migrations needed for readiness health state, durable
   admission metrics, pending L1 immutable timestamps, and any startup integrity
   status storage.
2. Add pure readiness types/evaluator while preserving current `/readyz`
   response behind tests only. Do not change admission behavior yet.
3. Add snapshot collection for existing signals: DB, heartbeats, local
   finalization, unconfirmed refs, active pending journal, deposit fetch time,
   and in-memory queue/pending payload migration metrics.
4. Add provider/indexer/reference-script probes with strict timeouts.
5. Add durable admission integration after blocker 1 lands; switch backlog
   readiness from memory to `tx_admissions`.
6. Add latched degraded-state table and recovery clearing rules.
7. Add snapshot freshness/cache handling and make stale snapshots blocking.
8. Replace `/readyz` response with schema version 1 and update operational
   dashboards/alerts.
9. Wire `POST /submit` to `requireAdmissionReady` for new admissions while
   keeping durable admission's in-transaction capacity checks authoritative.
10. Run unit, handler, integration, and fault-injection tests.
11. Remove temporary in-memory readiness inputs once the Effect queue and
    `pendingPayloads` are no longer authoritative or present.

Rollout must not include compatibility shims for old readiness semantics in
`demo/midgard-node`. If strict readiness breaks old behavior, keep the strict
behavior.

## Risks And Open Questions

- Durable admission must land first or concurrently. Without it, readiness can
  only approximate admission safety because accepted work still enters memory.
- Provider/indexer probes can add load or latency. They need caching, bounded
  timeouts, and rate-aware polling, but cached probe data must expire quickly
  enough to be meaningful.
- Some readiness checks require new durable timestamps and health state tables.
  These must be introduced through explicit migrations, not silent startup table
  creation.
- Pending merge readiness should use the dedicated pending merge journal planned
  in blocker 3. Until that exists, readiness can only report generic merge
  heartbeat and local finalization state.
- Deposit cursor semantics are present but may not yet be fully wired into the
  current fetch path. The implementation must decide whether to advance that
  cursor during full visible-set reconciliation or replace it with a clearer
  durable deposit visibility record.
- MPT integrity checks may be expensive. Startup should perform full integrity
  verification; readiness should consume a latched status and lightweight drift
  indicators.
- Kubernetes readiness probes often expect small responses. Keep the default
  response concise enough for probes, and expose optional detail only if needed
  through an admin endpoint or query parameter.
- Any auto-clear rule for degraded state must be conservative and auditable.
  Integrity failures should generally require explicit recovery evidence.
- Readiness checks have time-of-check/time-of-use races by nature. Mitigation:
  use readiness as an early reject path only and enforce idempotency, leases, and
  capacity in the durable state transitions themselves.

## Concrete Checklist

- [ ] Define versioned readiness response types and stable reason codes.
- [ ] Keep readiness evaluation pure and table-testable.
- [ ] Add effectful readiness snapshot collection with bounded dependency
      probes.
- [ ] Add snapshot freshness, cache TTL, and stale-snapshot fail-closed rules.
- [ ] Include DB health, migration version, startup integrity, and durable
      health-state checks.
- [ ] Include provider, indexer, and reference-script health checks.
- [ ] Include deposit fetch age, deposit cursor lag, and projection failure
      state.
- [ ] Include durable pending block-finalization status and immutable submitted
      age.
- [ ] Detect disagreement between process unconfirmed refs and durable pending
      finalization rows.
- [ ] Include local finalization state and age.
- [ ] Include durable admission backlog and oldest age after blocker 1 lands.
- [ ] Include validation backlog, oldest age, last success, and infrastructure
      failure counters.
- [ ] Include temporary `txQueue.size` and `pendingPayloads.length` migration
      metrics until durable admission replaces memory.
- [ ] Add latched degraded-state storage for integrity and recovery failures.
- [ ] Publish readiness metrics from the same sampled snapshot.
- [ ] Replace `/readyz` with schema version 1 response and `200`/`503` status.
- [ ] Add `requireAdmissionReady` guard to new `/submit` admissions.
- [ ] Enforce durable admission capacity in the admission transaction, not only
      in readiness.
- [ ] Preserve safe duplicate/status lookups during degraded readiness without
      admitting new work.
- [ ] Add unit tests for every blocking and warning reason.
- [ ] Add handler tests for response schema, status codes, timeouts, and secret
      redaction.
- [ ] Add integration tests for durable admission backlog, pending L1
      submission, deposit staleness, and local finalization pending.
- [ ] Add fault-injection tests for provider timeout, DB outage, restart with
      latched degraded state, and validation infrastructure failure.
- [ ] Update dashboards, alerts, and runbooks for every blocking reason.
- [ ] Remove temporary in-memory readiness inputs when no longer needed.
