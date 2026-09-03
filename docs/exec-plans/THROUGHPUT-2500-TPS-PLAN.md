# Midgard Throughput Plan: Reaching 2,500 Sustained TPS

**Status:** Historical parent analysis from 2026-07-08; do not execute it as a
current runbook. The phase ExecPlans contain subsequent source-verified
corrections.

**Last reviewed:** 2026-07-22

**Goal:** Demonstrate ≥2,500 sustained L2 tx/s on a declared workload and
production-shaped deployment without weakening protocol safety or recovery.
**Method:** Fresh analysis of `demo/midgard-node` and `demo/midgard-sdk` source (no reliance on prior docs). All findings cite file:line and were spot-verified.

---

## Non-negotiable protocol gates

Throughput evidence is inadmissible if a run disables local UPLC evaluation,
proof-relevant commitments, DA validation/retention, durable journals,
deployment identity checks, or finality/rollback handling. Larger blocks and
new payload versions require proof-witness worst-case-fit analysis, challenger
deadline analysis, independent DA retrieval, deterministic root/vector
conformance across TypeScript/Aiken, and crash/rollback recovery evidence before
they are enabled. An acceptance TPS number is not a settlement-throughput or
security claim; report each pipeline stage separately.

## 1. Throughput model

A tx must pass four pipeline stages. Sustained TPS is the minimum of the four stage ceilings:

| Stage           | Path                                       | Estimated ceiling today               |
| --------------- | ------------------------------------------ | ------------------------------------- |
| A. Admission    | HTTP `/submit` → `tx_admissions`           | ~1–3k/s (Postgres round-trips per tx) |
| B. Validation   | tx-queue-processor → mempool               | ~1–3k/s (single JS thread)            |
| C. Block commit | mempool → MPF roots → L1 header tx         | **~250–500 TPS (hard cap)**           |
| D. DA + merge   | libp2p payload publish, attestation, merge | ~sufficient bytes-wise; latency-gated |

**The binding constraint is Stage C.** Blocks are capped at 10,000 txs (`workers/utils/commit-block-planner.ts:71`) and block N+1 cannot start until block N's L1 tx is confirmed on Cardano (~20–40 s effective cadence: `workers/commit-block-header.ts:868-889`, `pending-journal.ts:115-177`, `WAIT_BETWEEN_BLOCK_CONFIRMATION=10s` in `services/config.ts:237-239`). That yields an absolute ceiling of **10,000 ÷ 20–40 s ≈ 250–500 TPS**, regardless of how fast every other stage runs. Reaching 2,500 TPS requires blocks of **50k–100k txs per L1 confirmation window**, which in turn requires the MPF and validation engines to be rebuilt for that scale.

---

## 2. Bottleneck inventory (verified, ranked)

### Stage C — block commit (hard ceiling)

**C1. Per-operation, disk-backed MPF updates with per-op root recomputation.**
`MidgardMpf.applyBatch` (`workers/utils/mpf.ts:2455-2478`) is a sequential loop; each `insert`/`delete` awaits an `@aiken-lang/merkle-patricia-forestry` promise against on-disk LevelDB **and then recomputes the root** (`mpf.ts:2444, 2452` — `Effect.andThen(() => this.root())`). `buildTransitionTraceResult` (`mpf.ts:970-1075`) applies these per source event, sequentially, single-threaded. At 50k txs/block this is hundreds of thousands of serial LevelDB round-trips per block.

**C2. O(total UTxO set) full scratch-trie rebuild every commit.**
`processMpfs` unconditionally recomputes `computeUtxoPayloadRoot` over the _entire_ live ledger (`mpf.ts:1867-1905`, "payload_root_check"), fed by unbounded `SELECT * FROM confirmed_ledger` (`database/utils/ledger.ts:120-133`, called from `workers/commit-block-header.ts:145`). Cost grows with chain history forever — throughput degrades over time even at constant block size.

**C3. No commit pipelining.** Single global commit worker under a phase lock + DB lease (`fibers/block-commitment.ts:463-497, 799-810`); block N+1 construction does not overlap block N's L1 confirmation wait, even though the node owns the state-queue tail UTxO and could chain unconfirmed txs.

**C4. Mempool candidate query is inverted and capped.**
`MempoolDB.retrieve`: `ORDER BY time_stamp_tz DESC LIMIT 100000` (`database/mempool.ts:163-165`). Newest-first means once backlog exceeds 100k rows, the _oldest_ txs are starved indefinitely; it also anchors block `endTime` on the wrong candidate (`commit-block-planner.ts:308-313`). Full re-read of pending tx CBOR every ~1 s commit tick.

**C5. Miscalibrated planner cost model.** `estimatedCommitBuildMsPerTx: 1` hard-coded (`commit-block-planner.ts:82`) vs. real disk-bound MPF cost — the planner packs batches it cannot build within its 30 s budget, causing timeouts instead of backpressure.

### Stage B — validation

**B1. Single-flight, single-thread validation.** One drain loop guarded by refs (`fibers/tx-queue-processor.ts:513-561`); Effect "concurrency" only interleaves synchronous CPU closures on one Node thread — no `worker_threads` anywhere in this path. Ed25519 verification (CML), double CBOR decode, and pure-JS UPLC/Plutus evaluation (`@harmoniclabs/plutus-machine` via `local-script-eval.ts:16-47`) all run serially on one core.

**B2. Hidden hard caps.** `VALIDATION_BATCH_HARD_CAP=1600`, `VALIDATION_MIN_BATCH=128`, `VALIDATION_PHASE_A_MAX_EFFECTIVE_CONCURRENCY=8` (`tx-queue-processor.ts:141-143`) silently override env config; 1600 txs per pass at a 500 ms poll (`commands/listen.ts:363`) caps ingestion ~3.2k/s before CPU cost.

**B3. Redundant work per tx.** Full CBOR decode in the HTTP handler (`listen-utils.ts:132-155`) repeated in Phase A (`phase-a.ts:242`); full `mempool_ledger` reload on cache-version miss (`tx-queue-processor.ts:252-276`); O(n²)-style conflict bucketing in Phase B (`phase-b.ts:605-623`).

### Stage A — admission

**A1. Per-tx locking transaction with live backlog COUNT.** Every `/submit` runs `SELECT…FOR UPDATE` + `COUNT(*) WHERE status IN ('queued','validating')` + `INSERT` in one transaction (`database/txAdmissions.ts:126-219`). The COUNT's cost grows with backlog — slower exactly when under load.

**A2. Hard-coded 20-connection Postgres pool** shared by every fiber in the process (`services/database.ts:27`); not exposed in config.

**A3. N+1 rejection updates.** `markRejected` loops one `UPDATE` per rejected tx, up to batch size, inside one transaction (`txAdmissions.ts:436-455`).

**A4. Write amplification.** Each accepted tx fans out to ~6–7 statements across 5 tables (`mempool`, `mempool_ledger`, `mempool_tx_deltas` delete+insert, spent-row delete, `address_history` select+insert) in `database/mempool.ts:87-142`.

### Stage D — DA / L1 settlement

**D1. L1 confirmation chaining (protocol-adjacent, implementation-worsened).** Each commit spends the prior state-queue tail and waits for confirmed-chain visibility before the next (`pending-journal.ts:115-177`). See C3 — the wait itself is Cardano-inherent; failing to overlap work with it is not.

**D2. Uncompressed, inline DA publish.** Full payload (up to 64 MiB cap, `midgard-core/src/da-transport.ts:28`) pushed inline to every committee peer with no compression, no publish-side chunking (`da/libp2p-producer.ts:576-684`). Egress = committee size × payload bytes per block.

**D3. Serialized merges.** One queued block merged per L1 tx, 10 s pacing, gated on DA attestation + ~20 s maturity buffer (`transactions/state-queue/merge-to-confirmed-state.ts`, `merge-readiness.ts:15-20, 394-422`). Queue drain rate caps sustained (not just burst) throughput.

Positive findings at the reviewed revision: core mempool/ledger inserts are
bulk multi-row, indexes cover the observed hot paths, and `SKIP LOCKED` claiming
supports multiple consumers. Header size is largely independent of L2
transaction count, but every deployment must check the current Cardano protocol
parameters and the complete commit/proof transaction shape rather than assuming
a permanent L1 byte limit.

---

## 3. Plan

Target math: at a realistic 20–40 s L1 confirmation cadence, 2,500 TPS ⇒ **50k–100k txs/block**. At ~300 B/tx canonical CBOR that is 15–30 MiB DA payload (fits 64 MiB; ~3–6× smaller with zstd). Every phase below is sized against that requirement.

### Phase 0 — Baseline & instrumentation (week 1)

Build a repeatable soak benchmark (extend existing stress scripts in `demo/midgard-node`) that reports per-stage rates: admit/s, validate/s, txs-per-committed-block, commit build ms broken down by MPF phase (timers already exist, e.g. `validation_mempool_insert_duration`, `tx-queue-processor.ts:131-139`). Record baseline sustained TPS. **Exit:** dashboard with per-stage ceilings measured, not estimated.

### Phase 1 — Configuration & query quick wins (weeks 1–3, low risk)

1. Make pool size configurable; raise default (A2). Split pools: one for latency-sensitive admission, one for batch/background work.
2. Replace per-admit `COUNT(*)` with a cached/estimated counter or periodic gauge (A1).
3. Collapse admission to a single statement: `INSERT … ON CONFLICT (tx_id) DO NOTHING RETURNING …` instead of SELECT-FOR-UPDATE + COUNT + INSERT (A1).
4. Batch `markRejected` via one `UPDATE … FROM unnest(...)` (A3).
5. Fix `MempoolDB.retrieve` to `ORDER BY … ASC` and select incrementally/keyset-paginated rather than full re-read (C4).
6. Expose the hidden validation caps as env config; decode CBOR once (drop the inline handler decode or pass the decoded form through) (B2, B3).
7. Make `address_history` and `mempool_tx_deltas` writes async/write-behind, out of the accept transaction (A4).

**Expected:** Stage A ceiling to ≥5k/s; Stage B claim-rate no longer scheduler-capped. Stage C unchanged — overall TPS still ~500; that is expected.

### Phase 2 — Parallel validation (weeks 3–7)

1. Move Phase A (CBOR decode + Ed25519 + native-script checks — embarrassingly parallel per tx) into a `worker_threads` pool sized to cores.
2. Keep Phase B (dependency graph, state patch) on the coordinator, but replace pairwise conflict bucketing with an input-key hash index (O(n)) and shard independent buckets across workers (B3).
3. Run UPLC script evaluation in workers; evaluate replacing the pure-JS CEK machine with a WASM/native UPLC evaluator (SDK also unconditionally sets `localUPLCEval: true` — `midgard-sdk/src/tx-completion.ts:8-20`).
4. Allow multiple concurrent drain loops (the `SKIP LOCKED` claim path already supports it) once Phase A is thread-safe.

**Expected:** Stage B ceiling ≥10k/s on an 8-core box (sig verify ~each core several k/s with CML/WASM).

### Phase 3 — MPF engine rework (weeks 4–10, highest effort, highest value)

1. **True batched trie writes:** apply all ops, compute root once at batch end; drop per-op `root()` (`mpf.ts:2444,2452`). This alone removes the dominant per-op cost.
2. **Memory-resident working trie:** operate on an in-memory store during block build; flush to LevelDB once per committed block. Eliminates per-op disk round-trips (C1).
3. **Kill or amortize the full-ledger `payload_root_check`** (C2): maintain the UTxO-payload root incrementally as a persisted trie updated with the block delta; keep the full rebuild only as an offline/audit job. Also stop unconditionally fetching `confirmed_ledger` when the fast path doesn't need it (`workers/commit-block-header.ts:137-224`).
4. Parallelize root computation across the independent tries (ledger, transactions, trace, event-to-step) in separate workers.
5. Recalibrate the planner cost model from Phase 0 measurements; raise `maxL2TxCount`/`maxLedgerOpCount`/`maxTransitionStepCount` to 100k/400k/400k once build time supports it (C5).

**Expected:** block build for 50k txs in <10 s; commit cost no longer grows with total chain state.

### Phase 4 — Pipelined commits & batched merges (weeks 8–14)

1. **Overlap block N+1 build with block N confirmation:** snapshot mempool and build MPF deltas speculatively against block N's predicted post-state; only the final header-tx submission waits on N's confirmation. Since the node owns the state-queue tail UTxO, evaluate 0-conf chaining of its own commit txs (submit N+1 spending N's unconfirmed output) with rollback handling — the pending-journal machinery (`commit-block-header/pending-journal.ts`) is a natural foundation (C3, D1).
2. Batch merges: merge k>1 queued blocks per merge tx if the on-chain validator permits; otherwise pipeline merge txs the same way (D3).
3. Move the inline deposit/withdrawal "ingestion barrier" fetches (`workers/commit-block-header.ts:588-597`) off the commit critical path.

**Expected:** effective block cadence approaches L1 block time (~20 s) or better with chaining; combined with Phase 3, Stage C ceiling ≥2,500–5,000 TPS.

### Phase 5 — DA hardening (weeks 10–14, parallel track)

1. zstd-compress DA payloads before publish (D2) — ~3–6× byte reduction on CBOR.
2. Chunked, streaming publish with per-peer parallelism; return at threshold-ACK rather than all-peer completion.
3. Re-establish an operational 50k envelope gate after the canonical V1
   capability-floor/proof-completion follow-up supplies bounded publication or
   continuation support. Canonical consolidation measured the complete newest
   V1 inner shape at 71,049,618 bytes, above the retained 67,108,864-byte DA
   bound, so the pre-consolidation V3/V2 fixture and active runner were
   invalidated rather than relabeled or accepted through a compatibility path.
   The historical procedure remains documented in
   [`docs/benchmark-scenarios/phase-5-da-50k-distribution.md`](../benchmark-scenarios/phase-5-da-50k-distribution.md).

### Phase 6 — Verification & soak (weeks 14–16)

Run the Phase 0 benchmark at 2,500 TPS offered load for ≥1 hour on production-shaped infra (real Cardano testnet L1, real DA committee): assert p99 admission latency, zero tx starvation (oldest-tx age bounded), block cadence stability, and — critically — **no degradation as the UTxO set grows** (C2 regression guard). Then a 24 h soak at 2,500 TPS and a burst test to 2× target.

---

## 4. Risks

The 0-conf commit chaining in Phase 4 is the main protocol-sensitive item; if L1 rollback handling proves unsafe, the fallback is speculative build + submit-on-confirm, which still removes build time (not confirmation time) from the critical path — making Phase 3's <10 s build target mandatory. The MPF rework must preserve root equivalence with the on-chain fault-proof semantics; keep the full-rebuild check as a CI/audit invariant while removing it from the hot path. Multi-block merges depend on the Aiken validator's linked-list rules; confirm before committing to that design.

## 5. Summary

Midgard today is capped at roughly **250–500 TPS** by design, not by tuning: a 10k-tx block cap multiplied by an unpipelined ~20–40 s L1-confirmation cadence, with a block builder that performs sequential per-tx disk-backed trie writes and an every-block full-ledger root rebuild that gets slower forever. Validation and admission add secondary single-thread and per-tx-SQL ceilings near the 2,500 target. The path to 2,500+ TPS is: measure (P0), unblock the cheap limits (P1), parallelize validation (P2), rebuild the MPF engine for batched in-memory operation (P3), pipeline commits against L1 confirmation with larger blocks (P4), compress DA (P5), and prove it with sustained soak tests (P6).
