# ExecPlan: Throughput Phase 1 — Admission & Query Quick Wins (A1–A4, C4)

**Status:** Implemented 2026-07-10; controlled live benchmark acceptance pending
**Effort:** weeks 1–3, low risk
**Owner:** TBD
**Depends on:**
- Phase 0 baseline instrumentation (parent plan §Phase 0) — the per-stage dashboard must exist before the exit criterion can be asserted.
- Parent plan: `THROUGHPUT-2500-TPS-PLAN.md` §Phase 1 — this ExecPlan implements items 1–5 and 7 of that section (bottlenecks A1, A2, A3, A4, C4). Item 6 (validation caps / double CBOR decode, B2/B3) is deliberately **out of scope** here; it belongs to the Phase 2 (parallel validation) ExecPlan.

**Exit criterion:** Stage A ceiling ≥5k admits/s measured on the Phase 0 harness, zero oldest-tx starvation (oldest mempool tx age bounded across a sustained-overload soak).

All file paths below are relative to `demo/midgard-node/src/` unless prefixed otherwise. All line numbers were verified against the working tree on 2026-07-09 (branch `tx-validation`).

---

## 1. Current state (verified)

### 1.1 A1 — per-tx admission transaction with live backlog COUNT

`TxAdmissionsDB.admit` (`database/txAdmissions.ts:126-219`) runs one `sql.withTransaction` per `/submit` containing up to three statements:

1. `SELECT * FROM tx_admissions WHERE tx_id = $1 FOR UPDATE` (`:146-149`). If a row exists and its bytes match, a second statement `UPDATE ... SET last_seen_at = NOW(), updated_at = NOW(), request_count = request_count + 1 ... RETURNING *` (`:166-172`) returns `kind: "duplicate"`; if bytes differ it fails `TxAdmissionConflictError` (`:158-165`).
2. `SELECT COUNT(*)::bigint ... WHERE status IN ('queued','validating')` (`:179-184`), compared against `maxBacklog` (`:185-195`) → `TxAdmissionBacklogFullError` when full.
3. `INSERT INTO tx_admissions (...) VALUES (...) RETURNING *` (`:197-210`) → `kind: "new"`.

So the *hot path* (a brand-new tx) is 1 lock-taking SELECT + 1 COUNT + 1 INSERT + BEGIN/COMMIT — five round trips, one of which (`COUNT(*)`) scans the queued/validating set and gets slower exactly when the node is loaded. The COUNT can use the partial index `idx_tx_admissions_dequeue ... WHERE status IN ('queued','validating')` (`database/migrations/sql/0002_durable_tx_admissions.sql:64-66`), but at 10k backlog that is still a ~10k-entry index scan per submit.

Schema facts that matter for the redesign (`0002_durable_tx_admissions.sql:20-62`): `tx_id BYTEA PRIMARY KEY` (`:21`) — so `ON CONFLICT (tx_id)` needs no new index; `arrival_seq BIGSERIAL UNIQUE` (`:24`) — assigned by default on insert, so a plain INSERT preserves FIFO claiming (`claimBatch` orders by `arrival_seq ASC`, `txAdmissions.ts:274`); CHECK constraints require `status='queued'` rows to have NULL lease columns — the single INSERT below satisfies them identically to today's INSERT.

HTTP semantics today (`commands/listen-router.ts:1841-1994`):
- new: **202** with `{txId, status, firstSeenAt, lastSeenAt, duplicate: false}` (`:1937-1946`);
- duplicate: **200**, same body shape with `duplicate: true` (`:1943-1945`);
- byte-conflict: **409** `{error: "E_TX_ID_BYTES_CONFLICT", ...}` (`:1952-1963`);
- backlog full: **503** `{error, backlog, maxBacklog}` (`:1965-1977`);
- DB error: **500** (`:1979-1989`).
On `kind === "new"` the handler wakes the tx-queue processor (`:1930-1932`). These semantics must be preserved bit-for-bit.

Other `countBacklog` consumers (all off the hot path, unchanged by this plan except where noted): readiness handler `listen-router.ts:977-978`, pipeline-status `:1183`, `:1536`, merge fiber `fibers/merge.ts:417`, tx-queue processor tick `fibers/tx-queue-processor.ts:352` (also `oldestQueuedAgeMs` at `:372`).

### 1.2 A2 — hard-coded 20-connection pool

`services/database.ts:18-30`: the SQL layer is `PgClient.layer` from `@effect/sql-pg` (porsager `postgres` underneath) with `maxConnections: 20` hard-coded at `:27`, `idleTimeout: 5 min`, `connectTimeout: 2 s`. It is exposed solely as the generic `SqlClient.SqlClient` tag via `Database.layer` (`:56-67`); every consumer (`yield* SqlClient.SqlClient`) shares whichever single pool its runtime was provided.

Verified pool topology: the `listen` command builds **one** runtime providing `Database.layer` for the HTTP router *and* all background fibers (`commands/listen.ts:313-368` — `appThread` plus `blockCommitmentFiber`, `txQueueProcessorFiber(mkSchedule(500))`, merge, retention, monitors, all in one `Effect.all`). The commit worker runs in a separate worker thread and provides **its own** `Database.layer` (`workers/commit-block-header.ts:98-107`), i.e. a second 20-connection pool. So batch work inside the main process (validation drain: claim, `markAccepted`'s mempool fan-out, `markRejected`) competes directly with `/submit` latency for the same 20 connections.

One routing subtlety verified for the split design: `requestTxQueueProcessorWakeup` is invoked *from the HTTP handler fiber* (`listen-router.ts:1931`) and `Effect.forkDaemon`s a drain (`fibers/tx-queue-processor.ts:550-562`). Forked fibers inherit context, so after a naive pool split the validation drain triggered by a submit would run on the **admission** pool. §2.2 addresses this explicitly.

### 1.3 A3 — N+1 rejection updates

`markRejected` (`database/txAdmissions.ts:386-469`): inside one transaction, a single batched upsert into `tx_rejections` (`:408-426` — already bulk, keep), then a `for` loop issuing **one `UPDATE tx_admissions ... WHERE tx_id = $1 AND status='validating' AND lease_owner = $2` per rejected tx** (`:437-454`), followed by an exactly-once count check (`:455-464`). Per-row values that differ: `reject_code`, `reject_detail` (`:446-447`). Everything else (status/lease/terminal/updated columns, lease-owner guard) is uniform — so `WHERE tx_id = ANY($1)` alone is *not* sufficient; the batch must carry per-row code/detail, which is exactly the `FROM unnest(...)` shape. Under an adversarial or buggy load producing full-batch rejections this is up to `VALIDATION_BATCH_SIZE` (default 1000, `services/config.ts:255-257`) sequential round trips per drain tick.

### 1.4 A4 — accept-path write fan-out

`MempoolDB.insertMultiple` (`database/mempool.ts:87-142`) — called from inside `markAccepted`'s transaction (`txAdmissions.ts:344-378`), so all of the following commits atomically with the admissions terminal update:

1. `mempool` bulk insert (`:103`, via `Tx.insertEntries`, `database/utils/tx.ts:177-200`);
2. `mempool_ledger` bulk insert of produced UTxOs (`:113`);
3. `mempool_tx_deltas` upsert = **DELETE + INSERT** (`:118` → `database/mempoolTxDeltas.ts:99-119`);
4. spent-input DELETE from `mempool_ledger` returning consumed deposit event ids (`:125` → `database/mempoolLedger.ts:351-368`) + `deposits` `markConsumedByEventIds` update (`:126`);
5. `address_history` SELECT + INSERT (`:132` → `database/addressHistory.ts:72-96`).

That is 7 statements across 5 tables per accepted batch (per-batch, not per-tx — the parent plan's "per accepted tx fans out to ~6–7 statements" reads per-tx; the bulk path amortizes across the batch, but steps 3 and 5 still add 3 statements and row-count-proportional write volume to the latency-critical accept commit).

**Consumer verification (decisive for write-behind):**

- `mempool_tx_deltas` has exactly one reader: `MempoolTxDeltasDB.retrieveByTxIds` at `workers/utils/mpf.ts:1403-1405`, at commit-build time. Crucially, `resolveTxDeltaForCommit` (`mpf.ts:137-179`) treats a **missing delta as a soft miss**: it falls back to re-decoding the tx CBOR via `findSpentAndProducedUTxOs` (`:159-161`) and only rejects the tx if the CBOR itself is malformed. Deltas are a pure cache. Write-behind (and even crash loss) degrades to CPU, never correctness. Rows are deleted when txs leave the mempool (`mempool.ts:177-183` `clearTxs`).
- `address_history` has two readers: the `GET /address-history` endpoint (`listen-router.ts:1480` → `AddressHistoryDB.retrieve`, `addressHistory.ts:125-158`) and the retention sweeper prune (`fibers/retention-sweeper.ts:44`). Neither is in the accept transaction, neither does read-after-write within a request. Eventual visibility (tens of ms) on a user-facing history endpoint is acceptable — the endpoint already reflects only-accepted (not merely submitted) txs, so its freshness contract is already asynchronous.

**Correction (parent plan A4 mischaracterizes the address_history select):** the "spent-side" half of `AddressHistoryDB.insert` is dead code today, twice over. (a) It queries `mempool_ledger WHERE tx_id IN (spent)` (`addressHistory.ts:84-86`), but `spent` contains **CBOR-encoded outrefs** (`utils.ts:80-90`, `midgardOutRefToCbor`), not 32-byte tx ids — the predicate compares outref bytes against the `tx_id` column and matches nothing. (b) Even if the key matched, `insertMultiple` runs `MempoolLedgerDB.clearUTxOs(allSpent)` — which deletes by `outref` (`mempoolLedger.ts:359-363`) — *before* the address-history select (`mempool.ts:124-132`), inside the same transaction, so spent rows would be invisible anyway. Net effect today: only **produced-side** addresses are recorded. Phase 1 will reproduce exactly this observable behavior from in-memory data (no DB read at all) and drop the dead SELECT; restoring spent-side history is flagged as follow-up work, not silently "fixed" here (it would change endpoint output and belongs in its own change).

`DepositsDB.markConsumedByEventIds` (`mempool.ts:126`) **must stay synchronous**: consumed-deposit tracking feeds commit-barrier logic and must commit atomically with the ledger delete that discovered the event ids.

### 1.5 C4 — inverted, capped mempool retrieval and endTime anchoring

`MempoolDB.retrieve` (`database/mempool.ts:156-172`):

```sql
SELECT tx_id, tx, time_stamp_tz FROM mempool
ORDER BY time_stamp_tz DESC LIMIT 100000   -- :163-165
```

Sole caller: `workers/commit-block-header.ts:535`, every commit tick (`WAIT_BETWEEN_BLOCK_COMMITMENT` default 1000 ms, `services/config.ts:234-236`). Consequences, verified:

- **Starvation starts at ~10k backlog, not 100k.** `planCommitBatchBudgets` truncates from the *front* of the candidate list (`workers/utils/commit-block-planner.ts:413-451`, `maxL2TxCount: 10_000` at `:71`). With DESC ordering the front is the *newest* txs, so whenever backlog exceeds one block budget, the oldest txs are deferred — indefinitely under sustained load. The 100k LIMIT (`mempool.ts:165`) merely makes starvation absolute past that point. **Correction** to the parent plan's "once backlog exceeds 100k rows": the effective threshold is the per-block tx budget (10k).
- **endTime anchors on the first row.** `establishEndTimeFromTxRequests` returns `candidateTxs[0][time_stamp_tz]` (`commit-block-planner.ts:308-313`). Under DESC, element 0 is the max timestamp, which is the correct block `endTime` upper bound. Consumers: `commit-block-header.ts:638-643` (scheduler end-time fit) and `:877` → `endTime` for `submitTxBackedCommit` (`:938-941`). A second, independent first-element anchor exists at `workers/utils/mpf.ts:1445-1448` (`effectiveEndTime = decodedMempoolTxs[0]?...`), which also gates the deposit/withdrawal/tx-order visibility barriers (`mpf.ts:1451-1497`). Flipping retrieval to ASC without fixing **both** anchors would set `endTime` to the *oldest* candidate's timestamp — i.e. below the timestamps of every other included tx — producing an invalid block. This is the one correctness-coupled edit in C4.
- **Full CBOR re-read.** Up to 100k full `tx` BYTEA blobs (~30 MB at 300 B/tx) are read every ~1 s even though at most 10k are packed.
- Index: `idx_mempool_time_stamp_tz` on `(time_stamp_tz)` only (`database/utils/tx.ts:51-53`; listed in `database/migrations/index.ts:168`). `tx_id` is the PK (`tx.ts:49`). There is **no composite `(time_stamp_tz, tx_id)` index**, and one is genuinely needed: rows inserted in one accept batch share an identical `time_stamp_tz` (single multi-row INSERT, column default `NOW()` = transaction timestamp, `tx.ts:48`), so a 1000-tx batch is 1000 ties — keyset pagination without the tx_id tie-break column in the index degrades to a filter scan across the tie group.

**Correction (bonus finding):** `Tx.retrieveAllEntries` (`database/utils/tx.ts:205-220`) interpolates the ORDER BY column as a **bound parameter**, not an identifier: `ORDER BY ${Columns.TIMESTAMPTZ} DESC` (`:213`) — note the missing `sql(...)` wrapper present everywhere else. Postgres receives `ORDER BY 'time_stamp_tz'` (a constant), which is a no-op: `ProcessedMempoolDB.retrieve` (`database/processedMempool.ts:19`) and its consumers (`workers/commit-block-header.ts:539`, `workers/utils/commit-submission.ts:208`) get **unordered** rows today. The parent plan did not catch this. Fixed as part of C4 (item 3.9).

---

## 2. Architecture decisions

### 2.1 A1 — single-statement admission + cached backlog gauge

**Decision:** replace the three-statement locking transaction with a transactionless `INSERT ... ON CONFLICT (tx_id) DO NOTHING RETURNING *` hot path; move the backlog check out of SQL entirely, into an in-process gauge maintained by a background fiber that refreshes the true count every **500 ms** and is incremented locally on every successful new admit (hybrid gauge).

Hot path (new tx — the overwhelming case under benchmark load) becomes exactly one round trip:

```sql
INSERT INTO tx_admissions
  (tx_id, tx_canonical_cbor, tx_canonical_cbor_sha256, status, submit_source)
VALUES ($1, $2, $3, 'queued', $4)
ON CONFLICT (tx_id) DO NOTHING
RETURNING *;
```

- 1 row returned → `kind: "new"` (HTTP 202). `arrival_seq` is assigned by its BIGSERIAL default exactly as today, preserving FIFO claim order (`claimBatch` orders by `arrival_seq ASC`, `txAdmissions.ts:274`).
- 0 rows returned → the tx_id already exists (ON CONFLICT arbitration waits out any concurrent in-flight insert, so by the time the empty result returns, the winner's row is committed and visible to the next statement under read committed). Run the duplicate statement:

```sql
UPDATE tx_admissions
SET last_seen_at = NOW(), updated_at = NOW(), request_count = request_count + 1
WHERE tx_id = $1
  AND tx_canonical_cbor_sha256 = $2
  AND tx_canonical_cbor = $3
RETURNING *;
```

- 1 row → `kind: "duplicate"` (HTTP 200, `duplicate: true`) — byte-identical response semantics to today's `:166-177` path, including the `request_count`/`last_seen_at` bump.
- 0 rows → the row exists with different bytes → `TxAdmissionConflictError` → HTTP 409 `E_TX_ID_BYTES_CONFLICT`, same as today's `:158-165`. (Theoretical race: the row was deleted between the two statements. `tx_admissions` rows are never deleted by any current code path — verified: no DELETE on the table anywhere in `src/` — so treat 0 rows as conflict; if a future retention policy prunes terminal admissions, add one bounded retry of the INSERT.)

Backlog gating moves to the handler, *before* the INSERT, against the cached gauge — with one ordering subtlety to preserve today's semantics: today the duplicate check precedes the backlog check (`:146-177` before `:179-195`), so duplicates succeed even when the backlog is full. Therefore: if `gauge < maxBacklog` → hot path as above. If `gauge ≥ maxBacklog` → run only the duplicate UPDATE; 1 row → 200 duplicate (parity), 0 rows → 503 backlog-full (parity — a genuinely new tx never reaches INSERT while the gauge is full). Cost under overload: one PK-guarded UPDATE per rejected submit instead of COUNT(*) + FOR UPDATE — strictly cheaper.

**Gauge design.** State lives in the existing `Globals` service pattern as `Ref<{ baseCount: bigint; localNewAdmits: bigint; inFlightReservations: bigint; refreshedAtMs: number }>`. Reported value = `baseCount + localNewAdmits + inFlightReservations`. A fiber refreshes `baseCount` by running today's `countBacklog` query (`txAdmissions.ts:471-479`, uses the partial index `idx_tx_admissions_dequeue`) every `ADMISSION_BACKLOG_REFRESH_MS` (default 500) and resets `localNewAdmits` to admits-since-snapshot without erasing reservations that are still in flight.

The submit handler atomically checks the reported value and reserves one slot in the same `Ref.modify` before it attempts the INSERT. An uninterruptible exit finalizer converts a `kind: "new"` result into `localNewAdmits` and releases proven duplicate/conflict/backlog exits. A database error, interruption, or defect is commit-ambiguous, so its reservation is conservatively retained as a local admit until the next live-count refresh; this can reject early for at most one refresh interval but cannot under-count a row that PostgreSQL committed before the client observed failure. This reservation is necessary: a plain read → INSERT → increment sequence lets concurrent distinct requests all observe the same below-cap value and overshoot by their concurrency. Duplicate-first semantics remain unchanged: a request that cannot reserve still executes the PK-guarded duplicate UPDATE, while a below-cap duplicate temporarily reserves and then releases a slot.

**Overshoot bound.** Backlog *grows* only through `/submit` on this process (the only other `submit_source`, `"backfill"`, is an offline/admin import path; `admit` itself excludes it, `txAdmissions.ts:134`; lease requeues flip validating→queued, both of which already count — neutral, `:221-242`). Because every growth event increments the gauge synchronously, the gauge **never under-counts**, so the cap is never overshot by local traffic: worst-case overshoot ≈ 0, vs. 5,000 admits/s × 0.5 s = 2,500 rows (25% of the default 10,000 cap, `MAX_DURABLE_ADMISSION_BACKLOG`, `services/config.ts:287-289`) for a naive periodic-only gauge — which is why the hybrid is required, and why 500 ms (2 COUNTs/s of DB load) is affordable rather than needing a 250 ms interval. Staleness errs only in the conservative direction: drains (markAccepted/markRejected, up to ~1,000/500 ms tick) are not observed until the next refresh, so the node may 503 up to one drain-batch (~10% of cap) early for ≤500 ms. Acceptable: 503 is a retryable signal and the cap is a soft protection, not a ledger invariant. The gauge value is also what the 503 body reports as `backlog`.

**Rejected alternatives.**
- `pg_class.reltuples` estimate: counts the whole table including terminal rows (accepted/rejected dominate over time), can't filter by status, and is only refreshed by autovacuum/analyze — error unbounded in exactly the bursty regime the cap exists for.
- Trigger-maintained counter row: turns every admit into a serialized hot-row UPDATE — reintroduces contention worse than the COUNT.
- Advisory-lock or `FOR UPDATE`-based schemes: keep the extra round trips; the lock was only ever needed to make check-then-insert atomic, and `ON CONFLICT` provides that atomicity for free.
- `INSERT ... ON CONFLICT DO UPDATE` (single statement covering new+duplicate): tempting, but the conditional `WHERE` needed to refuse byte-mismatched rows makes 0-rows ambiguous between "conflict" and nothing else, still needs a follow-up SELECT for the 409 body, and takes a row lock plus dead-tuple churn on every duplicate. `DO NOTHING` keeps the hot path lock-free and pays the second statement only on duplicates.

### 2.2 A2 — env-configurable, split pools

**Decision:** build **two** `PgClient` pools in the main process — `admission` (latency-sensitive) and `batch` (default) — both env-sized; keep the commit-worker thread's own pool (also env-sized). The generic `SqlClient.SqlClient` tag remains the *batch/default* pool everywhere, so zero call sites in fibers/workers change; the HTTP server layer alone is provided the admission pool as its `SqlClient.SqlClient`, so zero handler call sites change either.

Concretely in `services/database.ts`: `createPgLayerEffect` is parameterized by `(maxConnections)`, reading `NodeConfig`; `Database.layer` composes:
- `SqlClientLive` (batch pool, `POSTGRES_BATCH_POOL_SIZE`, default 20) → provides `SqlClient.SqlClient` + `PgClient.PgClient` as today;
- `AdmissionSqlLive` → provides a new tag `AdmissionSql` (a `Context.Tag<AdmissionSql, SqlClient.SqlClient>`) backed by a second `PgClient.layer` with `POSTGRES_ADMISSION_POOL_SIZE` (default 10).

Wiring in `commands/listen.ts:313-318`: wrap only the router serve layer — `HttpServer.serve(buildListenRouter(...)).pipe(Layer.provide(admissionAsDefaultSqlLayer))` where `admissionAsDefaultSqlLayer` maps `AdmissionSql → SqlClient.SqlClient`. All fibers in the `Effect.all` at `:336-368` keep the ambient batch pool untouched.

**Verified pitfall handled:** `requestTxQueueProcessorWakeup` forks the validation drain *from the handler fiber* (`listen-router.ts:1931`, `fibers/tx-queue-processor.ts:550-562`), and forked fibers inherit context — so without intervention the submit-triggered drain would consume admission connections. Fix inside `requestTxQueueProcessorWakeup`: re-provide the batch client to the forked drain (`Effect.provideService(SqlClient.SqlClient, yield* BatchSql)` where `BatchSql` is a tag carrying the batch client that is available in *both* scopes). This is the only cross-pool touch point found; the readiness/pipeline-status handlers (`listen-router.ts:977-978, 1183, 1536`) intentionally stay on the admission pool (their queries become gauge-cheap after A1 anyway).

The commit worker keeps constructing its own layer (`workers/commit-block-header.ts:98-107`) — it already is a separate pool; it just becomes sized by `POSTGRES_WORKER_POOL_SIZE` (default 10) instead of the hard-coded 20.

Connection budget at defaults: 20 (batch) + 10 (admission) + 10 (worker thread) = 40 of Postgres's default `max_connections = 100`, leaving headroom for ops tooling; all three knobs exist precisely so the Phase 0 benchmark can sweep them.

**Rejected alternatives.**
- One bigger shared pool: raising 20 → 50 helps but leaves head-of-line blocking — a commit-tick 100k-row retrieve or a markAccepted mega-transaction can still monopolize connections ahead of `/submit`. Splitting is the point; sizing is secondary.
- Tag-per-consumer (`AdmissionSql` yielded explicitly in `txAdmissions.ts`): touches every admission-path DB function signature and breaks the `Database` type alias used across ~40 modules. Layer-scoped default swapping achieves the same isolation with a wiring-only diff.
- PgBouncer sidecar: right answer at multi-node scale, but out of scope for a weeks-1–3 low-risk phase and unnecessary at 40 connections.

### 2.3 A3 — batch rejection via `UPDATE ... FROM unnest`

**Decision:** replace the per-row loop (`txAdmissions.ts:437-454`) with a single statement carrying parallel arrays, because `reject_code`/`reject_detail` differ per row (`:446-447`) — plain `WHERE tx_id = ANY($1)` cannot express that:

```sql
UPDATE tx_admissions AS a
SET status          = 'rejected',
    lease_owner     = NULL,
    lease_expires_at = NULL,
    terminal_at     = NOW(),
    reject_code     = r.reject_code,
    reject_detail   = r.reject_detail,
    updated_at      = NOW()
FROM unnest($1::bytea[], $2::text[], $3::text[]) AS r(tx_id, reject_code, reject_detail)
WHERE a.tx_id = r.tx_id
  AND a.status = 'validating'
  AND a.lease_owner = $4
RETURNING a.tx_id;
```

The exactly-once invariant check (`:455-464`) is preserved verbatim against `RETURNING` row count; the already-batched `tx_rejections` upsert (`:408-426`) and the enclosing transaction are unchanged. `tx_id` join is PK-indexed; batch size is bounded by `VALIDATION_BATCH_SIZE` (1000) so the arrays are small. Implementation note: `@effect/sql-pg` (porsager) binds JS arrays as PG arrays with an explicit cast as written above; the unit test in §6 asserts the binding (BYTEA array of 32-byte buffers) round-trips — if it does not, the fallback is a `(VALUES ...)` row list built via `sql.csv`, same plan shape.

**Rejected alternatives:** `tx_id = ANY($1)` + a second pass for codes (two statements, still row-count-dependent); per-row `Effect.all` with concurrency (burns pool connections, keeps N statements); moving rejection off the transaction (breaks the exactly-once-under-lease invariant the count check enforces).

### 2.4 A4 — write-behind for `mempool_tx_deltas` and `address_history`

**Decision:** remove statements 3 and 5 (§1.4) from the accept transaction. Both consumers verified write-behind-tolerant (§1.4): deltas are a soft cache with a decode fallback (`mpf.ts:137-179`); address_history is an eventually-consistent user query with no read-after-write contract. Neither requires the per-batch-synchronous fallback contemplated by the parent plan — full asynchronous write-behind is safe. `mempool`, `mempool_ledger`, spent-delete + deposit consumption, and the admissions terminal UPDATE stay in the atomic accept transaction (they are the correctness core: ledger state, deposit tracking, exactly-once terminal transition).

**Writer:** one in-process fiber (`services/write-behind.ts`) draining a bounded `Queue` of items
`{ kind: "tx_deltas", deltas: TxDelta[] } | { kind: "address_history", entries: AddressHistoryDB.Entry[] }`.
Flush policy: whenever buffered items ≥ `WRITE_BEHIND_MAX_BATCH` (default 1,000 tx-deltas / entries) **or** `WRITE_BEHIND_FLUSH_INTERVAL_MS` (default 100 ms) elapses with a non-empty buffer, issue the same bulk statements used today (`MempoolTxDeltasDB.upsertMany`, `AddressHistoryDB.insertEntries` — both already batch-shaped, `mempoolTxDeltas.ts:99-119`, `addressHistory.ts:54-70`) on the **batch** pool. Queue capacity `WRITE_BEHIND_QUEUE_CAPACITY` (default 50,000 items); on offer-would-block, fall back to a synchronous inline write (never drop while the process lives — loss is crash-only). Enqueue happens in `markAccepted` *after* the accept transaction commits, so the queue never holds rows for a rolled-back accept.

**Data capture (no re-reads):** the enqueue payload is computed from the in-memory `ProcessedTx[]` already in hand at `mempool.ts:95-118` — `toTxDelta` (`:48-55`) for deltas; for address_history, entries are built directly as `{tx_id: tx.txId, address: produced.address}` from `ProcessedTx.produced` (which carries addresses, `utils.ts:91-103`). Per the §1.4 Correction this reproduces today's *actual* output exactly (produced-side only) while deleting the dead spent-side SELECT — one fewer statement and no dependency on `mempool_ledger` state at flush time (which matters: the spent rows are deleted inside the accept tx, so any deferred DB lookup would race).

**Coherence with `clearTxs`:** `mempool.clearTxs` deletes deltas for committed txs (`mempool.ts:177-183`). A flush landing *after* the clear would resurrect delta rows for already-committed txs. These orphans are functionally harmless (`retrieveByTxIds` is only ever called with current-mempool tx ids, and tx ids never re-enter the mempool) but would leak storage; mitigation: the retention sweeper (`fibers/retention-sweeper.ts`) gains a periodic `DELETE FROM mempool_tx_deltas d WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = d.tx_id)` sweep. address_history has no delete-side race (it is append-only until retention prune).

**Crash-loss analysis:** on crash, up to `flush interval + queue depth` of rows are lost. (a) Lost deltas: next commit build silently re-decodes those txs' CBOR (`mpf.ts:159-178`) — cost is CPU on the commit worker, bounded by block size; no state divergence; no rebuild needed. (b) Lost address_history rows: the `/address-history` endpoint permanently omits those txs. Tolerable because the table is already an auxiliary, retention-pruned (`RETENTION_DAYS`, `retention-sweeper.ts:44`), partial (produced-side-only) index; and it is offline-rebuildable — every needed fact (`tx_id`, output addresses) is derivable from `mempool`/`immutable` tx CBOR, so a backfill script can reconstruct it if an operator ever cares. Document this in the runbook; do not build the rebuilder in Phase 1.

**Rejected alternatives:** per-batch synchronous writes outside the transaction (halves the win — still serializes two extra statements into accept latency; only justified if a consumer had needed request-scoped consistency, and none does); Postgres `LISTEN/NOTIFY` or logical-replication-driven projection (operationally heavy for two cache tables); dropping `mempool_tx_deltas` entirely in favor of always-decode (rejected *for Phase 1* — it would shift measurable CPU onto the commit worker that Phase 3 is about to rework; revisit there).

### 2.5 C4 — oldest-first keyset retrieval + endTime re-anchoring

**Decision:** replace `MempoolDB.retrieve` with an oldest-first, keyset-paginated reader; cursor = `(time_stamp_tz, tx_id)` row-value; add the composite index; flip both endTime anchors from first-element to max-timestamp-element.

New API (`database/mempool.ts`):

```ts
export type MempoolCursor = { readonly timeStampTz: Date; readonly txId: Buffer };
export const retrievePage: (opts: {
  readonly after?: MempoolCursor;      // exclusive
  readonly limit: number;              // required, no hidden 100k
}) => Effect.Effect<
  { readonly entries: readonly Tx.EntryWithTimeStamp[]; readonly nextCursor: MempoolCursor | null },
  DatabaseError, Database>;
```

```sql
SELECT tx_id, tx, time_stamp_tz
FROM mempool
WHERE ($1::timestamptz IS NULL) OR (time_stamp_tz, tx_id) > ($1, $2)
ORDER BY time_stamp_tz ASC, tx_id ASC
LIMIT $3;
```

The tx_id tie-break is load-bearing, not cosmetic: all rows of one accepted batch share one `NOW()` timestamp (§1.5), so ties of 1,000+ rows are the normal case. Row-value comparison + `ORDER BY time_stamp_tz, tx_id` gets a clean forward index scan given the new composite index (migration `0013`, §3.8); the existing single-column `idx_mempool_time_stamp_tz` cannot serve the tie-break and is superseded (dropped for `mempool` in the same migration to avoid double write amplification; `processed_mempool` gets the same treatment).

Caller change (`workers/commit-block-header.ts:535`): one page with `limit = MEMPOOL_RETRIEVE_PAGE_SIZE` (default 20,000 = 2× `maxL2TxCount`, headroom for planner pruning by scheduler window, `commit-block-planner.ts:541-545`) replaces the 100k read — an ~80% cut in bytes read per tick at deep backlog, and the oldest txs are always in the page, eliminating starvation at its root. The commit flow deletes committed txs from `mempool` afterwards, so each tick naturally restarts from `after: undefined`; the cursor exists for callers that need to walk deeper than one page within a tick (Phase 3's planner — see §8). No persistent cursor state, hence no cursor-invalidations across ticks.

**endTime re-anchoring (correctness-coupled, same commit):**
- `establishEndTimeFromTxRequests` (`commit-block-planner.ts:308-313`) → return the timestamp of the **last** element (max under ASC): `candidateTxs[candidateTxs.length - 1][TIMESTAMPTZ]`. Both call sites (`commit-block-header.ts:638`, `:877`) get the corrected upper bound; the `:938-940` comment ("first-candidate timestamp rule") is updated.
- `mpf.ts:1445-1448` → `effectiveEndTime` becomes the max-timestamp element of `decodedMempoolTxs` (last element — decode preserves input order, and skipped/rejected txs can only remove elements, `mpf.ts:1407-1434`). The visibility-barrier guards at `mpf.ts:1451-1497` then compare the true block upper bound against ingestion barriers, which is what they always intended.
- `planCommitBatchBudgets` (`commit-block-planner.ts:413-451`) needs no change: front-truncation of an ASC list keeps the *oldest* txs — the desired FIFO packing — and `planSchedulerAwareCommitSelection`'s timestamp-cap filter (`:541-549`) is order-preserving.
- Fix the `Tx.retrieveAllEntries` ORDER BY parameter bug (`database/utils/tx.ts:213`) to a real identifier with **ASC** ordering, making `ProcessedMempoolDB.retrieve` (`processedMempool.ts:19`; consumers `commit-block-header.ts:539`, `commit-submission.ts:208`) deterministically oldest-first too — today it is unordered (§1.5 Correction), and processed txs feed the same endTime anchor at `commit-block-header.ts:877`.

**Rejected alternatives.**
- `OFFSET` pagination: O(offset) rows scanned and discarded per page; under a moving mempool (rows deleted every commit) offsets also skip/duplicate rows. Keyset is O(page) and delete-stable.
- `ORDER BY arrival` via a mempool serial column: `mempool` has no serial; adding one duplicates what `(time_stamp_tz, tx_id)` already provides for free and needs a data backfill.
- Fixing starvation by raising the LIMIT: does nothing about planner front-truncation under DESC (§1.5) and makes the byte re-read worse.
- Keeping DESC and reversing in JS: still reads 100k rows to find the 10k oldest; the sort is not the cost, the I/O is.

---

## 3. Implementation items (ordered; each independently revertible)

1. **`database/txAdmissions.ts` — single-statement admission.**
   Replace `admit` (`:126-219`) with two exported effects, no `withTransaction`:
   - `tryInsert({txId, txCanonicalCbor, submitSource}): Effect<Entry | null, DatabaseError, Database>` — the `ON CONFLICT DO NOTHING RETURNING *` INSERT (§2.1); `null` on 0 rows.
   - `touchDuplicate({txId, txCanonicalCborSha256, txCanonicalCbor}): Effect<Entry | null, DatabaseError, Database>` — the guarded UPDATE (§2.1).
   - Keep a composed `admit({..., currentBacklog, maxBacklog})` with today's `AdmitResult`/error union so non-handler callers and tests keep one entry point; it implements the gate-order logic of §2.1 using the passed-in gauge value instead of `COUNT(*)`. `countBacklog` (`:471-479`) and `oldestQueuedAgeMs` (`:481-493`) remain for the gauge fiber and readiness.
2. **Backlog gauge fiber — new `fibers/admission-backlog-gauge.ts`.**
   `admissionBacklogGaugeFiber(schedule)` refreshing `Globals` ref (new fields `ADMISSION_BACKLOG_BASE`, `ADMISSION_BACKLOG_LOCAL_DELTA`, `ADMISSION_BACKLOG_REFRESHED_AT`) via `TxAdmissionsDB.countBacklog`; exported `readAdmissionBacklogGauge: Effect<bigint, never, Globals>` and `noteLocalAdmit: Effect<void, never, Globals>`. Registered in `commands/listen.ts` `Effect.all` (`:336-368`) with `mkSchedule(nodeConfig.ADMISSION_BACKLOG_REFRESH_MS)`. Seed the ref with one live count at fiber start so the first 500 ms is not gated on zero.
3. **`commands/listen-router.ts` `postSubmitHandler` (`:1841-1994`).**
   Replace the `TxAdmissionsDB.admit` call (`:1921-1926`) with: read gauge → call composed `admit` with `currentBacklog` → on `kind:"new"` run `noteLocalAdmit`. Response construction (`:1937-1946`), error catches (`:1952-1989`), timers, and the wakeup (`:1930-1932`) are untouched.
4. **`services/database.ts` + `services/config.ts` — pool config & split (§2.2).**
   - `config.ts`: add `POSTGRES_ADMISSION_POOL_SIZE` (10), `POSTGRES_BATCH_POOL_SIZE` (20), `POSTGRES_WORKER_POOL_SIZE` (10), `ADMISSION_BACKLOG_REFRESH_MS` (500), `MEMPOOL_RETRIEVE_PAGE_SIZE` (20,000), `WRITE_BEHIND_FLUSH_INTERVAL_MS` (100), `WRITE_BEHIND_MAX_BATCH` (1,000), `WRITE_BEHIND_QUEUE_CAPACITY` (50,000) to `NodeConfigDep` (`:23-90`) and `makeConfig` (`:118-579`), each `Config.integer(...).pipe(Config.withDefault(...), Config.mapAttempt(positive-safe-integer guard))` per house style (e.g. `:133-145`).
   - `database.ts`: parameterize `createPgLayerEffect(:18-51)` by pool size + a `role` label; export `Database.layer` (batch as `SqlClient.SqlClient`), `AdmissionSql` tag + layer, `BatchSql` alias tag. Worker thread (`workers/commit-block-header.ts:104`) picks up `POSTGRES_WORKER_POOL_SIZE` through the same parameterized layer.
   - `commands/listen.ts:313-318`: provide admission pool as the router's default `SqlClient.SqlClient`.
   - `fibers/tx-queue-processor.ts:550-562`: `requestTxQueueProcessorWakeup` re-provides `BatchSql` as `SqlClient.SqlClient` to the forked drain (§2.2 pitfall).
5. **`database/txAdmissions.ts` `markRejected` (`:386-469`) — batch UPDATE.**
   Replace the loop (`:437-454`) with the `unnest` statement of §2.3; keep the count check (`:455-464`) against `RETURNING` length; keep the `tx_rejections` upsert and transaction unchanged.
6. **Write-behind writer — new `services/write-behind.ts` (§2.4).**
   `WriteBehind` Context.Tag exposing `enqueueTxDeltas(deltas)`, `enqueueAddressHistory(entries)`, `flushNow` (for tests/shutdown), depth gauges. Fiber registered in `listen.ts`; graceful shutdown drains the queue (`Effect.addFinalizer` → `flushNow`).
7. **`database/mempool.ts` `insertMultiple` (`:87-142`) — slim the accept transaction.**
   Delete steps 3 (`:117-121`) and 5 (`:130-135`) plus their timers from the transaction; after `markAccepted`'s transaction commits (`txAdmissions.ts:378`), enqueue `processedTxs.map(toTxDelta)` and produced-side address entries. Delete `AddressHistoryDB.insert` (dead spent-side SELECT, `addressHistory.ts:72-96`) once the single-tx `insert` path (`mempool.ts:57-85`) is migrated the same way; `insertEntries` remains as the writer's flush target. Extend `fibers/retention-sweeper.ts` with the orphan-delta sweep (§2.4).
8. **Migration `database/migrations/sql/0013_mempool_keyset_indexes.sql`** (+ register in `migrations/index.ts` `MIGRATIONS` array per the `version/name/checksum` convention `:27-112`, bump `EXPECTED_SCHEMA_VERSION`, update `APPLICATION_INDEX_NAMES` `:152-187`):
   ```sql
   CREATE INDEX idx_mempool_time_stamp_tz_tx_id
     ON mempool (time_stamp_tz, tx_id);
   CREATE INDEX idx_processed_mempool_time_stamp_tz_tx_id
     ON processed_mempool (time_stamp_tz, tx_id);
   DROP INDEX IF EXISTS idx_mempool_time_stamp_tz;
   DROP INDEX IF EXISTS idx_processed_mempool_time_stamp_tz;
   ```
   Transactional (house convention, `migrations/index.ts:21`); table sizes at cap (≤100k rows) make in-transaction index builds sub-second, so `CONCURRENTLY` is unnecessary. `immutable` keeps its single-column index (retention pruning only, `tx.ts:226-242`).
9. **`database/mempool.ts` / `database/utils/tx.ts` / planners — C4 (§2.5).**
   - Add `retrievePage` to `mempool.ts`; delete the old `retrieve` (`:156-172`) once the caller is migrated.
   - `workers/commit-block-header.ts:535`: `MempoolDB.retrievePage({limit: nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE})`.
   - `commit-block-planner.ts:308-313`: last-element anchor; `mpf.ts:1445-1448`: max-timestamp anchor; update comment `commit-block-header.ts:938-940`.
   - `tx.ts:213`: `ORDER BY ${sql(Columns.TIMESTAMPTZ)} ASC` (bug fix + deterministic oldest-first for `ProcessedMempoolDB.retrieve`).

Ordering rationale: items 1–3 (A1) land first — they gate the exit criterion; 4 (A2) second because the pool split changes the environment under which 5–9 are benchmarked; 5 (A3) is independent; 6–7 (A4) before 8–9 (C4) so the accept-path speedup and the retrieval change are measured separately on the Phase 0 dashboard.

## 4. Config surface

| Env var | Default | Parsed at | Consumed by |
|---|---|---|---|
| `POSTGRES_ADMISSION_POOL_SIZE` | 10 | `services/config.ts` `makeConfig` (new; style of `:133-145`) | `services/database.ts` admission `PgClient.layer` |
| `POSTGRES_BATCH_POOL_SIZE` | 20 | 〃 | `services/database.ts` batch/default layer (replaces hard-coded `:27`) |
| `POSTGRES_WORKER_POOL_SIZE` | 10 | 〃 | commit-worker layer (`workers/commit-block-header.ts:104`) |
| `ADMISSION_BACKLOG_REFRESH_MS` | 500 | 〃 | `fibers/admission-backlog-gauge.ts` schedule |
| `MEMPOOL_RETRIEVE_PAGE_SIZE` | 20000 | 〃 | `workers/commit-block-header.ts:535` retrieve call |
| `WRITE_BEHIND_FLUSH_INTERVAL_MS` | 100 | 〃 | `services/write-behind.ts` |
| `WRITE_BEHIND_MAX_BATCH` | 1000 | 〃 | 〃 |
| `WRITE_BEHIND_QUEUE_CAPACITY` | 50000 | 〃 | 〃 (offer-full → inline sync fallback) |

Unchanged but load-bearing: `MAX_DURABLE_ADMISSION_BACKLOG` (10,000, `config.ts:287-289`) — now enforced via the gauge; `WAIT_BETWEEN_BLOCK_COMMITMENT` (1,000 ms, `:234-236`); `VALIDATION_BATCH_SIZE` (1,000, `:255-257`). All new vars follow the existing convention: `Config.integer` + `Config.withDefault` + `Config.mapAttempt` positive-safe-integer guard, surfaced through `NodeConfigDep`.

## 5. Observability

Existing timers already isolate the target costs — keep them as the before/after yardstick: `submit_durable_admission_duration` (`listen-router.ts:1920-1929` wrap), `tx_admission_mark_accepted_{mempool,terminal,total}_duration` (`txAdmissions.ts:18-31`), `mempool_persist_*_duration` (`mempool.ts:23-46` — the `deltas`/`address_history` timers move to the write-behind flush), `validation_queue_depth` / `validation_oldest_queued_tx_age` gauges (`tx-queue-processor.ts:353, 373-375`).

New metrics:
- `admission_backlog_gauge_value` (gauge) and `admission_backlog_gauge_staleness_ms` — dashboard proof that cap enforcement stays inside the §2.1 bound.
- `admission_backlog_refresh_duration` (timer) — detects COUNT degradation.
- `admission_duplicate_path_total`, `admission_backlog_reject_total` (counters) — 200/503 mix.
- `tx_admission_mark_rejected_duration` (timer; A3 before/after).
- `write_behind_queue_depth` (gauge), `write_behind_flush_duration` (timer), `write_behind_flush_rows_total`, `write_behind_inline_fallback_total` (counter — nonzero means capacity misconfigured).
- `mempool_retrieve_page_duration` (timer) + `mempool_retrieve_page_rows` — replaces the implicit cost inside the commit tick.
- `mempool_oldest_tx_age_ms` (gauge, computed in `monitorMempoolFiber`, `listen.ts:362`) — **this is the exit-criterion starvation metric**: under sustained overload it must saw-tooth (bounded by a few commit intervals), never grow monotonically.

## 6. Test & verification plan

**Unit** (existing emulator/pg-test harness):
- A1 state machine: new→202/`kind:new`; resubmit same bytes→200/`duplicate:true` with `request_count` incremented and `first_seen_at` stable; same tx_id different cbor→409 `E_TX_ID_BYTES_CONFLICT`; gauge-full + duplicate→200 (parity with today's check order); gauge-full + new→503 with gauge value in body. Concurrency: N parallel identical submits yield exactly one `kind:new` and N−1 duplicates (ON CONFLICT arbitration).
- Gauge arithmetic: base+delta+in-flight reporting, atomic concurrent reservation bound, commit/release accounting, reset-on-refresh, and monotone non-undercount under interleaved reservations/admits/refreshes.
- A3: batch of k rejections with distinct codes/details lands per-row-correct values; count-mismatch (one row not under lease) fails the transaction exactly as the loop did (`:455-464` parity); array binding round-trip for `bytea[]`.
- C4: `retrievePage` returns strict `(time_stamp_tz, tx_id)` ASC order across pages **with ties** (insert 3 batches sharing timestamps; walk with `limit` smaller than a tie group; assert no skip/dup); `establishEndTimeFromTxRequests` returns max timestamp for ASC input; `mpf` `effectiveEndTime` equals max included timestamp when rejects remove tail/head elements.
- A4: write-behind flush-by-size and flush-by-interval; enqueue-after-commit (a failed accept transaction enqueues nothing); queue-full inline fallback; orphan-delta sweep removes resurrected rows.

**Integration** (node against real Postgres):
- Duplicate submission end-to-end: two HTTP submits, assert 202 then 200 bodies byte-compatible with pre-change fixtures.
- **Backlog-cap under stale gauge:** freeze the refresh fiber (test hook), issue more parallel distinct submits than remaining slots, assert admitted-over-cap = 0 (hybrid reservation property) and that 503s report the gauge value; unfreeze, assert recovery within one refresh interval.
- **Starvation regression:** fill mempool with 3× `MEMPOOL_RETRIEVE_PAGE_SIZE` txs with distinct timestamps, run commit ticks (emulator commit path per `commit-block-header.ts` export note at `:973-975`), assert the first committed block contains the globally oldest tx and that block `endTime` ≥ every included tx timestamp (would have failed under naive ASC without the anchor fix).
- **Crash-loss:** kill the process between accept-commit and flush; restart; assert next commit succeeds (delta fallback decode path exercised — assert via `tx_delta_resolution` timing log fields `decoded_tx_count` vs cache hits, `mpf.ts:1435-1443`) and address-history rows for the lost window are absent (documented behavior).
- Pool split: saturate batch pool with an artificial long transaction; assert `/submit` p99 remains ≤1,000 ms and within 20% of the unsaturated same-process baseline (admission pool isolation), and that submit-triggered validation drains use the batch pool (assert via `pg_stat_activity` application_name labels — set per-pool `applicationName` in the layer, part of item 4). The 1 s ceiling is deliberately conservative: it is the existing Stage-B per-batch p99 bound and leaves a full order of magnitude over the 100 ms write-behind interval; the paired 20% comparison catches isolation regressions even on a fast host.

**Benchmark assertions (Phase 0 harness):** Stage A ceiling ≥5,000 admits/s sustained ≥5 min with p99 `/submit` latency ≤1,000 ms (`STRESS_SUBMIT_LATENCY_P99_MAX_MS=1000`); `mempool_oldest_tx_age_ms` bounded (≤3× the observed p95 interval between successful commit submissions) under 2× overload for 10 min; `mempool_retrieve_page_duration` p95 at 3× and 10× page depth no more than 10% above its one-page p95 on the same isolated database and warmed query plan; markRejected wall time for a 1,000-row batch reduced ≥10× vs the exact pre-A3 per-row loop baseline on cloned databases. These are fail-closed gates: null/missing p99 or p95 samples fail rather than silently passing.

## 7. Risks & rollback

Every item is an independent commit, revertible in isolation; no data migrations are destructive (the only schema change is index add/drop — reverting `0013` recreates the single-column indexes; follow the migration runner's forward-only convention by shipping a `0014` revert if needed post-deploy).

- **A1 (items 1–3).** Risk: subtle HTTP-semantics drift (duplicate/409/503 ordering) or gauge undercount admitting past the cap. Mitigations: parity fixtures in §6; hybrid gauge never undercounts local growth (§2.1). Residual: `backfill`-source imports bypass the local increment for up to one refresh interval — acceptable (offline/admin path). Rollback: restore old `admit`; handler signature unchanged apart from the gauge read.
- **A2 (item 4).** Risk: connection exhaustion from mis-sized pools, or a missed context-inheritance path pinning batch work to the admission pool (one found and fixed: wakeup fork; others hunted via per-pool `applicationName` + `pg_stat_activity` in the integration test). Rollback: set both pool sizes to 20 and point the router back at the default layer — behaviorally identical to today.
- **A3 (item 5).** Risk: array-binding edge (bytea[] of Buffers) in `@effect/sql-pg`. Caught by unit test before merge; fallback shape specified (§2.3). Rollback: restore loop.
- **A4 (items 6–7).** Highest-residual-risk item. Risks: (a) crash loss of address_history rows — accepted and documented (§2.4); (b) writer fiber death silently stopping flushes — mitigated by `write_behind_queue_depth` alert + supervisor restart (`repeatScheduledWithCauseLogging` pattern used by existing fibers); (c) orphaned delta rows — sweeper. Rollback: re-inline the two statements into `insertMultiple`; the tables' schemas are untouched, so mixed old/new rows are indistinguishable.
- **C4 (items 8–9).** Risk: the endTime anchor flip is correctness-coupled — an ASC retrieve with a missed anchor produces blocks whose `endTime` predates included txs (on-chain invalid / barrier-check failures at `mpf.ts:1451-1497`). Mitigation: anchors and ordering flip in **one commit**, guarded by the §6 endTime integration assertion; both anchor sites enumerated (§1.5) — a repo-wide grep for `[0]` on retrieve results is part of review checklist. Risk: unknown latent dependency on `ProcessedMempoolDB.retrieve`'s (accidental) insertion order — the `tx.ts:213` fix makes it deterministic ASC, which the deferred-payload path (`commit-block-header.ts:885-900`) replays in commit order; verified no consumer sorts DESC-dependently (`commit-submission.ts:208` builds a set). Rollback: restore DESC + first-element anchors together (single revert).

## 8. Interface contracts (what later phases rely on)

- **Keyset retrieve API (Phase 3 planner):** `MempoolDB.retrievePage({after?, limit}) → {entries, nextCursor}` with the invariants: strict ASC `(time_stamp_tz, tx_id)` ordering, exclusive cursor, stability under concurrent deletes of already-returned rows, ties resolved by `tx_id`. Phase 3's recalibrated planner (parent §Phase 3.5) walks multiple pages per tick against 100k budgets; it must not assume one page = whole backlog. `EntryWithTimeStamp` shape (`database/utils/tx.ts:24-31`) is frozen for this API.
- **endTime rule:** block `endTime` = max `time_stamp_tz` of included candidates (last element under ASC). Phase 3/4 speculative-build code must preserve this rule when it re-slices candidate sets (`establishEndTimeFromTxRequests` stays the single implementation; do not re-derive inline).
- **Pool tags (Phase 2 validation workers):** `SqlClient.SqlClient` = batch pool by default; `AdmissionSql`/`BatchSql` tags exported from `services/database.ts`. Phase 2's `worker_threads` validation pool must size its own connections from `POSTGRES_WORKER_POOL_SIZE` (or a new var) rather than assuming 20, and any fiber forked from HTTP-handler context must re-provide `BatchSql` (§2.2 pattern).
- **Backlog gauge (Phase 2):** `reserveAdmissionBacklogSlot` plus commit/release finalization in `Globals` makes the cap concurrency-safe; `readAdmissionBacklogGauge`/`noteLocalAdmit` remain for observation and non-HTTP accounting. Phase 2's multi-drain loops may add `noteLocalDrain` decrements to tighten the conservative staleness window; the never-undercount and in-flight-reservation properties must be preserved.
- **Write-behind contract (Phase 3):** `mempool_tx_deltas` is best-effort cache; Phase 3's MPF rework must keep the decode fallback (`resolveTxDeltaForCommit`, `mpf.ts:137-179`) or explicitly take a hard dependency and move delta writes back into the accept transaction — flagged in Phase 3's plan if so.
- **Admission single-statement invariant (Phase 0/2 dashboards):** `arrival_seq` remains the FIFO claim key and is assigned by the INSERT default; nothing may add statements back onto the `/submit` hot path without re-running the Stage A ceiling benchmark.

## 9. Implementation evidence (2026-07-10)

Implementation of A1–A4 and C4 is complete in the working tree. Verification used an isolated `midgard_test` Postgres database; no production or long-lived demo database was reset.

- `pnpm exec tsc --noEmit --pretty false`: passed.
- `pnpm run build`: passed for the full `demo/midgard-node` workspace dependency build.
- Touched-file ESLint and Prettier checks: passed.
- `git diff --check`: passed.
- `pnpm exec vitest run tests/database.test.ts`: 48/48 passed against real Postgres. This applied migration `0013_mempool_keyset_indexes.sql` from a clean schema and covered pool labels, single-statement admission semantics and concurrency, stale-gauge cap enforcement, batched rejection updates and rollback, keyset pagination with timestamp ties, write-behind flush-by-size/interval, post-commit enqueue, overflow fallback, and orphan cleanup.
- `NODE_ENV=emulator pnpm exec vitest run tests/admission-backlog-gauge.test.ts tests/commit-block-planner.test.ts tests/commit-mempool-decode.test.ts tests/listen-admission-auth.test.ts tests/tx-queue-processor.test.ts tests/submit-l2-transfer.test.ts tests/retention-policy.test.ts`: 50/50 passed.

A fresh concurrency review found that the original handler sequence read the gauge before incrementing it, allowing parallel distinct requests to reserve the same apparent capacity. The implementation now uses atomic in-flight reservations with commit/release exit finalization. Post-fix evidence:

- `pnpm exec vitest run tests/admission-backlog-gauge.test.ts`: 4/4 passed, including 12 concurrent reservations against a cap of 5 and refresh preservation of an in-flight slot.
- `pnpm exec vitest run tests/database.test.ts -t 'does not overshoot the cap under parallel distinct admits'`: 1/1 passed against real PostgreSQL; 16 simultaneous distinct attempts admitted exactly 5 and returned backlog-full for 11.
- `pnpm exec tsc --noEmit --pretty false`: passed after the reservation fix and the concurrent integration regression were added.

The numeric exit criterion is not claimed by these local correctness checks. The currently running demo node was started before this implementation, so measuring it would test stale code; rebuilding or resetting that long-lived stack was deliberately not done as part of this scoped implementation. Final acceptance still requires a controlled deployment of this revision with the Phase 0 harness: Stage A at least 5,000 admits/s for at least 5 minutes and a 10-minute 2x-overload soak with bounded `mempool_oldest_tx_age_ms`.

### 9.1 Verification-closure surfaces staged after the strict audit

The strict §6 audit found that the earlier 48/48 and 50/50 suites proved component behavior but did not execute the five named integration scenarios. The worktree now stages those missing surfaces without claiming their results: the real submit `HttpRouter` is exercised for exact 202/200/409/503 bodies and identical-submit arbitration; the gauge fiber has a cancellation-safe pre-refresh gate for frozen-refresh cap/recovery; the emulator commit suite seeds a backlog four pages deep and asserts global-oldest selection, max included `endTime`, and positive fallback decoding; a supervised child is SIGKILLed after the authoritative accept commit but before write-behind enqueue; and the real pool harness holds every batch connection while measuring admission-pool HTTP p99 and checking `pg_stat_activity` labels. `tx_delta_resolution` now reports `cache_hit_tx_count` and `fallback_decoded_tx_count`, backed by monotone counters with the same split.

The Phase 0 client now fails a stage when submit p99 is missing or exceeds `STRESS_SUBMIT_LATENCY_P99_MAX_MS` (1,000 ms by default), and the process exits nonzero when any primary measured stage fails that evaluation. `bench:phase1:query-write` implements the warmed 1x/3x/10x page-depth p95 gate plus randomized repeated legacy-loop-vs-batch measurements; every path runs from the same validating-row state and is rolled back transactionally before its peer runs.

The exact Node 22.22.2 query/write operator gate has now run against real
PostgreSQL and passed 2/2 in 36.02 seconds. The retained report is
`demo/midgard-node/logs/phase1-formal-query-write-20260714T084052Z/phase1-query-write.json`:
the 3x/1x and 10x/1x page-depth p95 ratios are 1.034685 and 0.880644,
respectively, while 1,000-row rejection updates improved from a 610.345 ms
legacy median to a 23.251 ms batched median (26.25x). This closes command 3
below only; the query/write result is not live acceptance evidence.

Commands 1 and 2 were rerun on 2026-07-14 with exact Node `v22.22.2`,
pnpm `9.15.4`, `TMPDIR=/tmp`, and real child-process/PostgreSQL access. Command
1 passed 7/7 selected tests (108 skipped) in 7.03 seconds, including the real
HTTP parity, frozen-refresh recovery, batch-pool isolation, positive fallback
decode, and post-accept `SIGKILL` recovery paths. Command 2 passed its selected
four-page backlog test (12 skipped) in 71.44 seconds and committed the globally
oldest transactions with the expected maximum `endTime`. These results close
the current-tree local commands 1 and 2. Controlled live command 4 and its
terminal wallet recovery remain required.

1. `NODE_ENV=emulator pnpm exec vitest run tests/admission-backlog-gauge.test.ts tests/commit-mempool-decode.test.ts tests/database.test.ts -t 'submit-router HTTP parity|stale refresh|batch pool is held|SIGKILL after accept commit|refresh schedules|test refresh gate|positively decodes' --reporter=verbose --disable-console-intercept`
2. `NODE_ENV=emulator pnpm exec vitest run tests/deposit-flow-emulator.test.ts -t 'backlog deeper than three retrieval pages' --reporter=verbose --disable-console-intercept`
3. `BENCH_PHASE1_OPERATOR=1 POSTGRES_DB=midgard_phase1_bench_gate NODE_ENV=emulator pnpm run bench:phase1:query-write` — **passed** with the retained evidence above.
4. Controlled node/corpus: run `pnpm run bench:l2:scenario:phase1-starvation-2x-soak` once against the bound live corpus. The scenario consumes one continuous 10-minute 5,000 TPS stream: its observer-only five-minute checkpoint proves the Stage-A rate/latency gate, and its full window proves the commit-enabled 2x starvation gate. Do not run the separate five-minute scenario first against the same corpus because that would spend inputs required by the soak.

### 9.2 Controlled live corpus identity binding

The controlled fresh deployment MUST NOT submit the earlier
`logs/phase-1-full-corpus-20260709T002743Z` corpus. That artifact remains valid
source/verification evidence, but its 4,096 wallet addresses differ from the
wallet records generated for the fresh deployment. A successful fanout to a
different wallet set does not make the old corpus live-spendable.

After the bounded `stress-wallets:fanout` run completes, live acceptance must
generate a new corpus from those exact wallet records and their verified live
funding outrefs:

1. Require exactly 4,096 `wallet-*.json` records. Hash the ordered
   `walletId|l2Address` pairs as the **wallet-set hash**. Never hash or copy seed
   phrases into benchmark evidence.
2. Require every record to contain at least one
   `latestFunding.fundingUtxos[]` entry observed from the fresh node. Hash the
   ordered `walletId|outref|sha256(outputCbor)` rows as the **funding-set hash**;
   require 4,096 unique first funding outrefs.
3. Generate the corpus with `--wallets-dir` pointing at that same directory,
   `--funding-source existing`, and a new run-scoped `--out-dir`. The generator
   therefore binds each chain's first input to the recorded fresh-deployment
   funding outref.
4. Run `stress-corpus-verify` against the generated corpus, index, manifest,
   and the same wallet directory. The verifier must stream-verify every row and
   rebuild-compare the configured deterministic chain sample; missing or
   mismatched funding snapshots fail closed.
5. Write a binding artifact containing the wallet-set hash, funding-set hash,
   corpus/index/manifest SHA-256 values, node image ID, node container ID,
   deployment manifest ID, corpus slice ID, and exact `STRESS_CORPUS_*`
   environment used by the load generator. The benchmark report's corpus
   paths and hashes must match this binding artifact.
6. Before measured traffic, query a deterministic wallet sample through
   `/utxos` and prove that each corpus chain's first input outref is present.
   Any absent first input invalidates the corpus for live acceptance.

For the 10-minute 5,000 TPS starvation gate, the current 4,096-wallet fanout
amount (`11,228,229` lovelace per wallet) is sufficient for a corpus generated
with the live fee parameters (`MIN_FEE_A=10`, `MIN_FEE_B=10`),
`--amount-lovelace 1`, `--duration-ms 600000`, `--safety-factor 1.02`, and an
assumed acceptance latency of `819` ms (the maximum that keeps 5,000 TPS within
4,096 one-in-flight chains). This yields depth `748`, `3,063,808` rows, and
`45,990,825,984` lovelace of final-wallet funding. The generated plan and live
client self-check remain
authoritative: if either rejects the requested rate or funding, do not weaken
the gate; provision more wallets/funding instead.

The combined scenario is fail-closed and does not split the load into two
stages. At five minutes it asynchronously snapshots Prometheus counters and
the lengths of the in-memory latency/scheduling samples while the same open
loop continues. The report requires at least 300 seconds, at least 98% of the
5,000 TPS durable-admit target, at least 99% accepted, submit p99 at most
1,000 ms, the configured scheduling limits, zero duplicate/non-202 successes,
zero submit errors, zero queue-full responses, zero validation rejections, and
all required metrics. It also records hashes and totals for every corpus
cursor at stage start, checkpoint, and stage end, and fails if any per-chain
cursor regresses. The full 600-second stage independently retains the §6
starvation proof. This is one immutable submit stream; no input is reset or
reused between the two observations.

Because this is a formal scenario, all named gate variables are immutable:
pre-set environment values may supply endpoints, corpus bindings, and output
paths, but any conflict with the pinned duration, rates, threshold values, or
gate-enable flags aborts before load. The five-minute callback and the
Prometheus response must both land no later than one second after the exact
300-second deadline; a later callback is not allowed to substitute a longer
average for the first-five-minute window.
