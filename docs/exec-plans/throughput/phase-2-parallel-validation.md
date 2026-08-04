# ExecPlan: Throughput Phase 2 — Parallel Validation (B1–B3)

**Status:** In progress — architecture/implementation/correctness gates are green; sustained throughput exit remains open
**Effort:** weeks 3–7
**Owner:** TBD
**Depends on:**

- Throughput Phase 1 (admission quick wins — in particular the batched `markRejected`, admission single-statement insert, and DB pool split; this plan assumes the accepted-tx DB write path can absorb ≥10k rows/s in bulk without becoming the new binding stage).
- Phase 0 baseline (measured per-stage ceilings; the arithmetic in §2.2 and §4 must be recalibrated against Phase 0's measured per-tx Phase A cost before defaults are frozen).
- Parent plan: `THROUGHPUT-2500-TPS-PLAN.md` §Phase 2 — Parallel validation, bottlenecks B1/B2/B3 (§2 "Stage B — validation").

**Exit criterion:** ≥10,000 validated tx/s sustained on an 8-core box (Stage B in isolation, admission pre-loaded), with p99 per-batch validation latency ≤ 1 s and identical verdicts to the single-threaded reference path on the differential corpus (§6.1).

---

## 1. Current state (verified)

> **Correction (module location):** the parent plan cites `phase-a.ts` / `phase-b.ts` without a path. These files are **not** in `demo/midgard-node/src` — Phase A/Phase B live in the workspace package **`@al-ft/midgard-validation`** at `demo/midgard-validation/src/phase-a.ts` and `demo/midgard-validation/src/phase-b.ts`, imported by the node at `demo/midgard-node/src/fibers/tx-queue-processor.ts:1-9`. All parent-plan line numbers for these files were spot-checked and are accurate against that package. The package is also consumed by `demo/lucid-midgard/src/builder.ts` (client-side pre-validation), which constrains how we may change its API (§8.3).

### 1.1 The end-to-end Stage B data flow today

**Admission (HTTP handler, coordinator thread).** `POST /submit` in `demo/midgard-node/src/commands/listen-router.ts`: content-type gate (415 unless `application/cbor`, `:1854-1866`), raw body read (`:1869`), size check, then a **full canonical decode** via `normalizeSubmitTxCanonicalCborToNative` (`listen-router.ts:1901-1903` → `demo/midgard-node/src/commands/listen-utils.ts:132-155`, calling `decodeMidgardSubmittedTxFromCanonicalCbor` at `:137-139`). The decoded `MidgardSubmittedTx` is discarded except for `txId` and canonical bytes, persisted via `TxAdmissionsDB.admit` (`:1921-1926`); a new admission requests a processor wakeup (`:1930-1932`).

**Claim.** One processor tick (`txQueueProcessorAction`, `demo/midgard-node/src/fibers/tx-queue-processor.ts:337-511`): requeue expired leases + backlog count (`:351-353`), then `TxAdmissionsDB.claimBatch` (`demo/midgard-node/src/database/txAdmissions.ts:258-294`) — a CTE with `ORDER BY arrival_seq ASC FOR UPDATE SKIP LOCKED` (`:274-275`) that leases up to `batchSize` rows. **The claim path is already multi-consumer-safe**; nothing at the SQL layer prevents concurrent drain loops.

**Phase A (stateless, per-tx).** `runPhaseAValidation` (`demo/midgard-validation/src/phase-a.ts:328-352`) maps `validateNativeOne` over the batch. Per tx: a **second full CBOR decode** of the same bytes the HTTP handler already decoded (`phase-a.ts:242`), tx-id/validity/aux-data/network/min-fee checks (`:255-294`), input-set and validity-interval checks, Ed25519 signature verification via CML (`verifyVKeyWitness`, `:145-152` — `CML.PublicKey.from_bytes(...).verify(...)`), native-script checks, then `buildPhaseAValidatedTx` (`demo/midgard-validation/src/validation-candidate.ts:64-112`) which materializes the Phase B projections: per-output CBOR re-encode, CML-encoded outrefs (`:17-26` — note `midgardOutRefToCbor` itself calls CML, so **workers need CML for projections too**, not just signatures), value sums, and hex hash arrays.

Crucially, the "concurrency" here is fictional for CPU purposes: `Effect.forEach(..., Effect.sync(...), { concurrency })` (`phase-a.ts:333-339`) interleaves synchronous closures **on one Node thread**. There is no `worker_threads` usage anywhere in the validation path. This is **B1**.

**Phase B (stateful, dependency-aware).** Pre-state is a coordinator-cached `Map<outrefHex, outputBytes>` (`ensureCachedUtxoState`, `tx-queue-processor.ts:252-276`). `runPhaseBValidationWithPatch` (`demo/midgard-validation/src/phase-b.ts:905-1078`) builds an intra-batch dependency graph from produced outrefs (`buildNodes`, `:625-673`), rejects cycles (`:675-708`), then processes topological "ready waves". Each wave is split into **conflict buckets** (`:605-623`, excerpt below), each bucket validated with `Effect.forEach(..., { concurrency: bucketConcurrency })` (`:981-996`), decisions applied **sequentially in input order** (`:998-1044`): accepted txs mutate `spentByAccepted` and the state patch (`:1021-1033`), rejections cascade to descendants (`:878-903`), and accepted candidates sort by `arrivalSeq` (`:1065-1071`).

```ts
// phase-b.ts:605-623 — B3(b): greedy independent-set packing, pairwise conflict tests
const buildConflictBuckets = (readyNodes: readonly CandidateNode[]): CandidateNode[][] => {
  const buckets: CandidateNode[][] = [];
  for (const node of readyNodes) {
    const bucket = buckets.find(
      (candidateBucket) => !candidateBucket.some((bucketNode) => conflict(node, bucketNode)),
    );
    ...
```

`conflict` (`:216-219`) is spent∩spent, spent∩ref, ref∩spent set intersection (`hasIntersection`, `:158-167`). Worst case (all candidates conflict, or many buckets) this is O(n²) node-pair tests × O(k) per intersection. Verified — the parent plan's B3 "O(n²)-style conflict bucketing (phase-b.ts:605-623)" is accurate.

**UPLC evaluation runs inside Phase B, not Phase A.** `validateCandidateAgainstState` (`phase-b.ts:710-876`) calls `runLocalScriptEvaluation` for **every** candidate (`:848-856`) — even script-free txs go through execution discovery, extraneous-witness and script-integrity-hash checks (`:472-547`). For each non-native execution it builds a script context and calls `evaluateScriptWithHarmonic` (`:578-581`), the pure-JS CEK machine in `demo/midgard-validation/src/local-script-eval.ts:16-47` (`parseUPLC` at `:21`, `Data.to` context encode at `:22-24`, `Machine.eval` at `:25`; `@harmoniclabs/plutus-machine` 2.1.3 per `demo/midgard-node/package.json:87`). Budget enforcement against declared exUnits at `:589-599`.

**Persist.** Rejections → `TxAdmissionsDB.markRejected` (`tx-queue-processor.ts:464-468`); accepted → `TxAdmissionsDB.markAccepted` (`:480-484`) which wraps `MempoolDB.insertMultiple` (`demo/midgard-node/src/database/mempool.ts:87-142`: mempool rows, mempool_ledger produced inserts, spent-row deletes, deltas, address history — one transaction). Then the state patch is applied to the coordinator cache (`:493-495`). Any thrown error releases the whole batch lease for retry (`:503-510`).

### 1.2 B1 — single-flight, single-thread

The drain loop is single-flight by construction: `txQueueProcessorDrainOnce` (`tx-queue-processor.ts:531-548`) checks `TX_QUEUE_PROCESSOR_ACTIVE` (`demo/midgard-node/src/services/globals.ts:79`) and coalesces concurrent wakeups into `TX_QUEUE_WAKE_REQUESTED` (`globals.ts:80`); `txQueueProcessorDrainLoop` (`:513-529`) re-runs while wake requests arrive. Every CPU-heavy step (decode, Ed25519, projections, conflict analysis, CEK evaluation) executes on the main thread that also serves HTTP, all Effect fibers, and Postgres I/O.

Shared mutable state that blocks running N of these loops today:

1. **Module-level cache** `cachedUtxoState` / `cachedUtxoStateVersion` (`tx-queue-processor.ts:145-146`), read in `ensureCachedUtxoState` (`:252-276`), mutated at `:493-495`. Two loops would race on it and corrupt the pre-state.
2. **`TX_QUEUE_PROCESSOR_ACTIVE` boolean** (`globals.ts:79`, enforced `:538-547`) — hard single-flight gate.
3. **Implicit batch serialization as the double-spend firewall.** Phase B's `spentByAccepted` and the state patch only cover one batch; cross-batch double-spend protection today relies on batches running strictly one-after-another against the shared cache. Two concurrent Phase B runs over overlapping pre-state could both accept txs spending the same outref (`MempoolLedgerDB.clearUTxOs` in `mempool.ts:124-125` deletes spent rows but nothing fails the second batch). This is the correctness reason Phase B must remain a serialized critical section (§2.5).

Ed25519 is WASM-backed: `CML` is re-exported by `@lucid-evolution/lucid` 0.5.5 (`package.json:96`) and resolves to **`@anastasia-labs/cardano-multiplatform-lib-nodejs` 6.2.0-1** — a wasm-bindgen package with a 3,191,423-byte `cardano_multiplatform_lib_bg.wasm` loaded synchronously at import (verified in `demo/node_modules/.pnpm/@anastasia-labs+cardano-multiplatform-lib-nodejs@6.2.0-1/.../package.json`). Per-worker init therefore costs one ~3.2 MB WASM compile+instantiate (tens of ms, once per worker at startup) plus a private WASM linear memory per worker (§7.1).

> **Correction (library):** the task brief guessed `@dcspark/cardano-multiplatform-lib`; the actual dependency is the Anastasia Labs fork. Same API family, same WASM characteristics.

### 1.3 B2 — hidden caps and the poll

```ts
// tx-queue-processor.ts:141-143
const VALIDATION_BATCH_HARD_CAP = 1600;
const VALIDATION_MIN_BATCH = 128;
const VALIDATION_PHASE_A_MAX_EFFECTIVE_CONCURRENCY = 8;
```

`selectValidationBatchSize` (`:288-304`) clamps the configured `VALIDATION_BATCH_SIZE` (env, default 1000 — `demo/midgard-node/src/services/config.ts:255-257`) to the 1600 hard cap; `selectPhaseAConcurrency` (`:309-331`) clamps `VALIDATION_PHASE_A_CONCURRENCY` (env, default 32 — `config.ts:261-263`) to 8 and further degrades it to 1/2/4 for batches under 256/512/1024. So the env knobs silently lie: configured 32-way concurrency is really ≤8-way interleaving, and configured 4000-tx batches are really 1600.

The processor fiber is scheduled at a hard-coded 500 ms (`txQueueProcessorFiber(mkSchedule(500))`, `demo/midgard-node/src/commands/listen.ts:363`, `mkSchedule` at `:333-334`).

> **Correction (poll severity):** the parent plan's "1600 txs per pass at a 500 ms poll caps ingestion ~3.2k/s" overstates the poll's role. There is already an event-driven wakeup: every new durable admission calls `requestTxQueueProcessorWakeup` (`listen-router.ts:1930-1932` → `tx-queue-processor.ts:550-561`), and the drain loop re-runs back-to-back while wake requests keep arriving (`:513-529`, `validation_coalesced_wakeup_count` counter). Under sustained load the binding limit is **batch_size ÷ single-threaded batch wall time**, not the poll. The 500 ms schedule only matters for recovery sweeps (expired leases, restarts). We still expose it as config (§4) but the poll change is a minor item, not the fix.

### 1.4 B3 — redundant work

- **(a) Double decode.** Same bytes fully decoded twice: HTTP handler (`listen-utils.ts:137-139`) and Phase A (`phase-a.ts:242`). The handler decode cannot simply be deleted — it is the admission-time canonicality gate and the only source of the txId returned to the client — and the decoded object cannot flow to the validator through the durable Postgres queue (`tx_admissions` stores bytes; the validating tick may be in a different process epoch). §2.4 states exactly which decode each design keeps.
- **(b) Conflict bucketing** — verified O(n²)-style, §1.1 excerpt.
- **(c) Full `mempool_ledger` reload on cache-version miss.** `ensureCachedUtxoState` (`tx-queue-processor.ts:252-276`) compares against `MEMPOOL_LEDGER_VERSION` (`globals.ts:75`) and on any mismatch re-runs `MempoolLedgerDB.retrieveSpendable` (`demo/midgard-node/src/database/mempoolLedger.ts:211-228`) — a full-table select of every spendable UTxO. The version is bumped by exactly three mutators, each of which knows precisely which rows it touched: deposit projection (`demo/midgard-node/src/fibers/project-deposits-to-mempool-ledger.ts:128`, entries enumerated at `:96-104`), deposit-bearing block confirmation (`demo/midgard-node/src/fibers/block-confirmation.ts:125-130`), and genesis (`demo/midgard-node/src/genesis.ts:57`). A handful of deposit UTxOs thus triggers an O(entire ledger) reload; cost grows with L2 state forever.

### 1.5 Existing worker conventions (must be followed)

- Build: `demo/midgard-node/package.json:13` — `"build": "tsup src/index.ts --minify --format esm && tsup src/workers/* --minify --format esm"`. Every top-level file in `src/workers/` becomes a self-contained ESM bundle in `dist/` (existing entries: `commit-block-header.ts`, `confirm-block-commitments.ts`, `corpus-chain-builder.ts`; helper modules live in `src/workers/commit-block-header/` and `src/workers/utils/`, bundled into their entry).
- Resolution: `resolveWorkerEntry(import.meta.url, "<name>.js")` (`demo/midgard-node/src/fibers/resolve-worker-entry.ts:5-26`) probes sibling dirs and `dist/`.
- Spawn: `node:worker_threads` `new Worker(resolveWorkerEntry(...), { workerData })`, results via a single `parentPort` message, wrapped in `Effect.async` with `message`/`error`/`exit` handlers and terminate-on-interrupt (`demo/midgard-node/src/fibers/block-commitment.ts:568-633`; same pattern in `block-confirmation.ts:218-219`, `stress-corpus-generate.ts:357-358`). Existing workers import lucid/CML and bundle fine under tsup — precedent that our worker entry can too (`workers/commit-block-header.ts:6-9` imports SDK + lucid + `worker_threads`).
- These are **one-shot** workers (spawn → one result message → terminate). Our pool keeps workers **long-lived** with correlated request/response messages — a deliberate, documented extension of the convention (§2.1), reusing the build/resolve machinery unchanged.
- `piscina` is **not** a dependency (verified absent from `demo/midgard-node/package.json` and `demo/midgard-validation/package.json`).

### 1.6 What Phase B actually consumes from Phase A (the wire contract)

`PhaseAValidatedTx` (`demo/midgard-validation/src/types.ts:77-107`). Field-by-field consumption, verified against `phase-b.ts` and the persist path:

| Field                                                                                             | Consumed at                                                                                                                                                                                                                                                                                |
| ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `ledgerTx.txId`                                                                                   | rejections/keys throughout; `processedTxFromValidatedTx` (`validation-candidate.ts:117`)                                                                                                                                                                                                   |
| `ledgerTx.fee`                                                                                    | value preservation `phase-b.ts:859-864`; script context `:456`                                                                                                                                                                                                                             |
| `ledgerTx.validityIntervalStart/End`                                                              | slot checks `:725-743`; native scripts `:516-519`; context `:457-458`                                                                                                                                                                                                                      |
| `ledgerTx.scriptIntegrityHash`                                                                    | integrity check `:535-547`                                                                                                                                                                                                                                                                 |
| `ledgerTx.outputs`                                                                                | context building + protected-output checks `:402-436`                                                                                                                                                                                                                                      |
| `ledgerTx.scriptWitnesses`                                                                        | inline script sources `:244-253, :481`; extraneous-witness check `:494-508`                                                                                                                                                                                                                |
| `ledgerTx.redeemers`                                                                              | duplicate/extraneous checks `:296-307, :438-446`; execution matching `:335-344, :549-552`                                                                                                                                                                                                  |
| `derived.*` (all nine fields)                                                                     | `witnessKeyHashHexes :747`, `nativeScriptHashHexes :748-750`, `plutusScriptHashHexes :751-753`, `requiredObserverHashHexes :390, :784`, `mintPolicyHashHexes :378, :794`, `mintDelta :377, :860`, `outputSum :862`, `redeemerWitnessHash :536`, `requiresScriptEvaluation` (informational) |
| `graph.spentOutRefHexes` / `referenceOutRefHexes`                                                 | node construction `:640-641`; patch `:1021-1024`                                                                                                                                                                                                                                           |
| `graph.produced`                                                                                  | producer index `:630-633`; patch upserts `:1026-1033`; persist (`validation-candidate.ts:122-127`)                                                                                                                                                                                         |
| `submission.arrivalSeq`                                                                           | final ordering `:1065-1071`                                                                                                                                                                                                                                                                |
| `submission.txCbor`                                                                               | persist (`validation-candidate.ts:118`)                                                                                                                                                                                                                                                    |
| `ledgerTx.vkeyWitnesses`, `ledgerTx.spendInputs/referenceInputs`, `ledgerTx.requiredSignerHashes` | **not consumed after Phase A** (signatures already verified; graph hexes supersede raw outrefs)                                                                                                                                                                                            |

Serialization hazards for a thread boundary (verified types): `MidgardValue` is plain `{ lovelace: bigint, assets: ReadonlyMap<string, ReadonlyMap<string, bigint>> }` (`demo/midgard-core/src/codec/value.ts:21-24`) — structured-clone-safe. `MidgardAddress = Buffer` (`codec/address.ts:16`) and every hash field is a `Buffer` — structured clone degrades `Buffer` to `Uint8Array`, and Phase B calls Buffer-only methods (`.equals` at `phase-b.ts:539`, `.toString("hex")` throughout), so **rehydration must re-wrap all binary fields**. `redeemers[].data` is decoded lucid Plutus data (`ledger-tx/codec.ts:623-631` → `redeemerDataFromCborHex` = `Data.from(cborHex)`, `demo/midgard-validation/src/midgard-redeemers.ts:147-148`), which contains `Constr` **class instances** that structured clone silently flattens — it must cross the boundary as `dataCborHex` and be re-decoded on arrival (§3, item 2).

---

## 2. Architecture decisions

### 2.1 B1: a long-lived `worker_threads` pool, hand-rolled, following the tsup/resolve convention

**Decision:** Add one new worker entry `demo/midgard-node/src/workers/validation.ts` (built automatically by the existing `tsup src/workers/*` script, resolved via `resolveWorkerEntry(import.meta.url, "validation.js")`), managed by a small hand-rolled fixed-size pool (`demo/midgard-node/src/services/validation-pool.ts`, ~300 LOC) of **long-lived** workers. Pool size defaults to `max(1, os.availableParallelism() - 2)`.

_Reservation rationale (the “−2”):_ one core for the coordinator thread — it still runs the HTTP server, all Effect fibers, Phase B decision logic, Postgres I/O, and DA/commit fibers — and one core for the Stage C commit worker (`block-commitment.ts:568-633`), which is CPU/IO-heavy for seconds at a time and must not be starved: Stage C is the parent plan's binding constraint, and stealing its core to make Stage B faster would be self-defeating. On an 8-core box: 6 validation workers.

_Why `worker_threads`, not `cluster`/`child_process`:_ (1) zero-copy `ArrayBuffer` transfer lists and cheap structured clone vs. pipe serialization; (2) in-process lifecycle — crash detection, terminate-on-interrupt — matching the existing Effect wrappers (`block-commitment.ts:589-632`); (3) it is the repo's established convention (three existing worker entries); (4) `cluster` would multiply Postgres pools and Globals state for no benefit since workers here are pure CPU and never touch the DB.

_Why hand-rolled, not piscina:_ piscina is not a dependency (§1.5) and buys little here: we need one pool, two job kinds, fixed sizing, FIFO dispatch with per-worker single-flight, and Effect integration — ~300 lines against the repo's own spawn/resolve/error conventions. Piscina's extras (work-stealing, atomics wait, cancellation) are unneeded and its interaction with the tsup `--minify` ESM worker bundling is unverified in this repo. **Rejected:** piscina; `@effect/platform` Worker wrappers (unused in the repo — the raw `Effect.async` pattern is the convention).

_Why long-lived, not one-shot like existing workers:_ per-job spawn would pay the ~3.2 MB CML WASM compile (§1.2) and module-graph load per batch — tens of ms against batch budgets of ~100-400 ms. Workers initialize CML (and `@harmoniclabs` modules) once at startup by virtue of top-level imports, then serve jobs until shutdown.

### 2.2 Job protocol: transfer raw bytes in, structured verdicts out

**Decision:** Phase A jobs carry raw tx bytes packed into **one transferable arena `ArrayBuffer` per chunk** (offset table + one buffer, moved via the postMessage transfer list — zero copy); verdicts return as structured-clone messages containing the full serialized `PhaseAValidatedTx` (per the consumption table in §1.6) plus rejections.

_Why an arena, not per-tx transfers:_ claimed rows' `tx_canonical_cbor` Buffers come from the Postgres driver and typically alias Node's shared 8 KB Buffer pool — transferring their underlying `ArrayBuffer`s would detach unrelated data. One fresh arena costs a single coordinator memcpy (needed anyway to escape the pool), then transfers copy-free; workers take zero-copy `Buffer` views via the offset table.

_Why not structured-clone decoded objects into workers instead:_ there is nothing decoded to send — the coordinator deliberately no longer decodes (that is the point of B1/B3a). Bytes in, verdicts out is also the smallest possible request message.

_Why return the full candidate rather than a slim verdict:_ Phase B consumes nearly all of `PhaseAValidatedTx` (§1.6 table) — the "compact verdict" and the candidate are the same object minus `ledgerTx.vkeyWitnesses` (raw keys/signatures, dead weight after verification, explicitly nulled on the wire and rehydrated as `[]`; a follow-up may formalize a narrowed Phase B input type, §8.3). Re-decoding accepted txs on the coordinator to avoid the clone would reinstate the exact cost we are removing. Response clone cost at 10k tx/s is roughly 15–30 MB/s of mostly-binary fields — measured, not assumed, in §6.3; if it shows up in profiles, the fallback is encoding candidates into a second transferable arena (noted as an optimization, not spec'd).

UPLC jobs (§2.3) are small both ways: `{ scriptBytes, contextCbor }` in (transfer list), `{ budget | detail }` out.

### 2.3 UPLC evaluation moves to the same pool; Phase B stays on the coordinator

**Decision:** Split `evaluateScriptWithHarmonic` at its natural seam: the coordinator keeps context **encoding** (`Data.to(scriptContext)` — `local-script-eval.ts:22-24`), and the worker runs `dataFromCbor` + `parseUPLC` + `Machine.eval` (`:21, :25`). `runPhaseBValidationWithPatch` gains an injected evaluator (`PhaseBConfig.evaluateScript?: (scriptBytes, contextCbor) => Effect<LocalScriptEvalResult>`, defaulting to the current inline implementation) so `@al-ft/midgard-validation` stays pure and pool-free — the pool-backed evaluator is wired in only by `midgard-node`. Within a conflict bucket/component, candidate validations already run under `Effect.forEach` concurrency (`phase-b.ts:981-996`), so multiple UPLC jobs are in flight against the pool concurrently while decision application stays sequential (`:998-1044`).

**Phase B itself stays on the coordinator, explicitly.** Rationale: it validates against a single mutable `Map` of the mempool ledger (hundreds of MB at scale — not shareable across threads without a SharedArrayBuffer re-design), its accept/reject decisions are order-dependent (double-spend winner = earlier candidate), and — with decode, signatures, and UPLC gone — its remaining per-tx cost is Map lookups, value accounting, and hash checks (~tens of µs). **Rejected:** sharding Phase B across workers by bucket (would require replicating or sharding the UTxO map and merging patches deterministically; complexity grossly disproportionate to the ~25–50k tx/s single-thread ceiling Phase B has once eval is offloaded).

**Follow-up item (documented, not spec'd here): WASM/native UPLC evaluator.** `@harmoniclabs/plutus-machine` is a pure-JS CEK machine; a Rust/WASM evaluator (note `@lucid-evolution/uplc` 0.2.21 is _already_ in the dependency tree, `demo/midgard-node/package.json:97`) typically gives 5–20×. Acceptance criteria for adopting one: (1) bit-identical accept/reject verdicts and **identical budget numbers** on the full differential corpus (§6.1) — budget parity is mandatory because declared-exUnits enforcement (`phase-b.ts:589-599`) is consensus-adjacent via fault proofs; (2) ≥5× median eval speedup on the Phase 0 script corpus; (3) memory per worker bounded. Related, out of scope but recorded: the SDK unconditionally sets `localUPLCEval: true` for the operator's own L1 tx building (`demo/midgard-sdk/src/tx-completion.ts:8-20`, flag at `:15`), which burns coordinator CPU during commit assembly — same evaluator swap would benefit it.

### 2.4 B3(a): which decode is eliminated, which are kept

**Decision:** three decode sites resolve as follows.

1. **Kept: HTTP-handler decode** (`listen-utils.ts:132-155`). It is the canonicality/size admission gate and the txId derivation for the client response, and it protects the durable queue from garbage. Its result still cannot flow through Postgres.
2. **Eliminated: the coordinator-side Phase A decode.** `phase-a.ts:242` no longer executes on the coordinator thread for the batch path — it moves inside the workers, in parallel, from raw bytes. This is the B3(a) elimination target: the coordinator goes from 2 full decodes per tx (handler + Phase A) to 1 (handler), and the second decode now costs wall-clock `batch/poolSize` instead of `batch`.
3. **Kept and embraced: the worker decode.** Workers re-decode from raw bytes by design — bytes are the cheapest thing to move across threads, and the decode is exactly the work we want off the coordinator.

Phase 1's "decoded-once contract" (parent plan §Phase 1 item 6) is thereby satisfied on the coordinator: after this plan, no code path decodes the same tx twice **on the same thread**.

### 2.5 Multiple drain loops with a serialized Phase B critical section

**Decision:** allow `VALIDATION_DRAIN_LOOPS` (default 4) concurrent loops. Each loop independently claims (`SKIP LOCKED` already safe, §1.1) and runs Phase A via the pool; the tail of the tick — cache read, Phase B, `markRejected`/`markAccepted`, patch apply, version sync — runs under a **single Effect semaphore (1 permit)** owned by a new coordinator-side cache service (§3 item 6). The state-blocking items enumerated in §1.2 are removed as follows: module cache → cache service; `TX_QUEUE_PROCESSOR_ACTIVE` boolean → active-loop counter capped at N; cross-batch double-spend safety → preserved _by construction_ because Phase B never runs concurrently with another Phase B or with a patch application.

_Why this is worth it even with Phase B serialized:_ pipelining. Loop 2's Phase A (worker pool) overlaps loop 1's Phase B + DB write (coordinator + Postgres). With stage times of the same order (§4 arithmetic), 2 loops move throughput from `batch/(tA+tB+tDB)` to ~`batch/max(tA, tB+tDB)`. Default 2 because Phase A vs. (Phase B + DB) is roughly an even split at target batch sizes; more loops mostly add lease contention and memory. **Rejected:** N fully independent loops with per-loop caches (breaks double-spend safety, §1.2 item 3); optimistic Phase B with DB-level conflict detection (no such constraint exists today, `mempool.ts:122-126`, and adding one puts conflict handling on the hot path).

**Determinism statement (for §7):** Phase B decision application remains sequential and in deterministic order _within a batch_. Across batches, which of two conflicting txs wins depends on batch assignment — exactly as it does today across sequential 1600-tx batches (arrival order is preserved by `claimBatch`'s `ORDER BY arrival_seq`, `txAdmissions.ts:274`, so cross-loop claim order remains arrival-ordered; only near-simultaneous conflicting txs straddling a batch boundary are timing-dependent, which is already true and is not consensus-relevant — mempool admission is operator-local; L1 fault proofs bind the committed state, not admission order).

### 2.6 B3(b): outref hash-index components replace pairwise buckets

**Decision:** replace `buildConflictBuckets` (`phase-b.ts:605-623`) with union-find **conflict components** built from two hash indexes over the ready wave: `spenders: Map<outrefHex, nodeIdx>` and `referencers: Map<outrefHex, nodeIdx[]>`. One pass per node: for each spent outref, union with the recorded spender and all recorded referencers of that outref, then record self as spender; for each referenced outref, union with the recorded spender only (ref∩ref is _not_ a conflict per `conflict`, `:216-219`), then record self as referencer. Cost O(total outrefs in wave × α) — linear in practice — versus the current O(n²·k).

Semantics change, verdicts don't: today's buckets are greedy _independent sets_ (members mutually non-conflicting, validated concurrently; conflicting nodes serialized by bucket order = first-seen order). Components are the _transitive closure of conflict_: members validated **sequentially in ready-wave order**, distinct components validated concurrently. In both schemes, of two conflicting candidates the one earlier in the wave order is decided first, so the earlier one wins the outref and the later one hits `spentByAccepted` (`phase-b.ts:804-806`) — identical verdicts, proven by the differential test (§6.1). Components are also the better shape for UPLC offload: independent components keep the worker pool fed while a conflict chain serializes only its own members. Decision application stays exactly as today (sequential, `:998-1044`).

### 2.7 B3(c): incremental cache invalidation via a delta journal

**Decision:** replace the bare `MEMPOOL_LEDGER_VERSION` integer with a bounded in-process **delta journal**: `Globals.MEMPOOL_LEDGER_DELTA_LOG: Ref<{ version: number; entries: ReadonlyArray<{ version: number; full: boolean; upserts: ReadonlyArray<readonly [outrefHex, Buffer]>; deletes: ReadonlyArray<string> }> }>` (ring, last `VALIDATION_LEDGER_DELTA_LOG_MAX = 64` versions). The three existing mutators publish deltas they already have in hand: deposit projection knows its exact `mempoolEntries` (`project-deposits-to-mempool-ledger.ts:96-104`, bump at `:128`); deposit-bearing confirmation publishes its projected entries (`block-confirmation.ts:113-130`); genesis publishes `full: true` (`genesis.ts:57` — rare, full reload acceptable). The cache service applies deltas `cachedVersion+1 … current` when all are present and none is `full`; otherwise it falls back to today's full `retrieveSpendable` reload — the fallback is never removed, so a forgotten future mutator degrades to correct-but-slow, not wrong. Any future direct writer of `mempool_ledger` MUST publish a delta or `full: true`; this rule gets a comment at the journal definition and a test (§6.2).

---

## 3. Implementation items

Ordered; each lands independently benchmarkable. "MV" = `demo/midgard-validation/src`, "MN" = `demo/midgard-node/src`.

**1. MV: export a single-tx Phase A validator.** Export `validateNativeOne` (rename `validatePhaseASingle`) from `MV/phase-a.ts:236-326` so the worker can drive per-tx validation without the Effect batch wrapper. `runPhaseAValidation` keeps its exact signature and behavior (it becomes the inline/reference path and stays the API `lucid-midgard/src/builder.ts` uses).

**2. MV: wire codec — new module `MV/wire.ts`.** `serializePhaseACandidate(c: PhaseAValidatedTx): WirePhaseACandidate` and `deserializePhaseACandidate(w: WirePhaseACandidate): PhaseAValidatedTx`. Explicit field-by-field mapping per §1.6: all `Buffer`s pass as `Uint8Array` and are re-wrapped with `Buffer.from(u8.buffer, u8.byteOffset, u8.byteLength)` (zero-copy views) on deserialize; `redeemers[].data` is replaced on the wire by `dataCborHex: string` (re-encoded via the existing canonical `plutusDataToCborHex`, decoded back with `redeemerDataFromCborHex`, `MV/midgard-redeemers.ts:147-148`); `vkeyWitnesses` serializes as `null`, deserializes as `[]`; `Map`/`bigint`/`Date` pass through structured clone natively. Round-trip property test in `MV/tests` (§6.2). No blanket structured clone of the whole object — every field is named, so a future field addition fails the exhaustiveness check (`satisfies`-based) instead of silently corrupting.

**3. MV: evaluator seam.** Split `local-script-eval.ts`: `encodeScriptContextCbor(scriptContext): Uint8Array` (the `Data.to` half, `:22-24`) and `evaluateUplcWithContextCbor(scriptBytes, contextCbor): LocalScriptEvalResult` (the `parseUPLC`/`Machine.eval` half, `:21, :25-46`). `evaluateScriptWithHarmonic` remains as the composition (back-compat). Add optional `PhaseBConfig.evaluateScript` (`MV/types.ts:148-152`) consumed at `MV/phase-b.ts:578-581`; absent ⇒ current inline behavior, bit-for-bit.

**4. MN: worker entry `MN/workers/validation.ts`** (bundles via the existing build line, resolves as `"validation.js"`). Message protocol (defined in `MN/workers/utils/validation-pool.ts`, imported by both sides):

```ts
export type ValidationWorkerInit = {
  readonly config: {
    readonly expectedNetworkId: bigint;
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
    readonly strictnessProfile: string;
  };
}; // via workerData, fixed per process life

export type PhaseAJobRequest = {
  readonly kind: "phase_a";
  readonly jobId: number;
  readonly arena: ArrayBuffer; // transferred (zero-copy)
  readonly txs: ReadonlyArray<{
    // offsets into arena
    readonly txIdOffset: number; // 32 bytes at offset
    readonly cborOffset: number;
    readonly cborLength: number;
    readonly arrivalSeq: bigint;
    readonly createdAtMs: number;
  }>;
};
export type UplcJobRequest = {
  readonly kind: "uplc";
  readonly jobId: number;
  readonly scriptBytes: ArrayBuffer;
  readonly contextCbor: ArrayBuffer; // both transferred
};
export type PhaseAJobResponse = {
  readonly kind: "phase_a";
  readonly jobId: number;
  readonly results: ReadonlyArray<
    | { readonly ok: true; readonly candidate: WirePhaseACandidate }
    | {
        readonly ok: false;
        readonly txId: Uint8Array;
        readonly code: RejectCode;
        readonly detail: string | null;
      }
  >; // index-aligned with request.txs
};
export type UplcJobResponse = {
  readonly kind: "uplc";
  readonly jobId: number;
  readonly result:
    | { readonly ok: true; readonly cpu: bigint; readonly memory: bigint }
    | { readonly ok: false; readonly detail: string };
};
export type WorkerFailure = {
  readonly kind: "job_failed";
  readonly jobId: number;
  readonly error: string;
};
```

Worker body: top-level imports of `@al-ft/midgard-validation` + lucid pull in CML WASM once at startup (precedent: `workers/commit-block-header.ts:6-9`); message loop dispatches on `kind`, never touches the DB or Globals, posts exactly one response per request. Any uncaught error posts `job_failed` for the in-flight jobId.

The worker's Ed25519 verifier is process-local rather than part of the
serializable `PhaseAConfig`: by default it uses Node/OpenSSL with a bounded
4,096-entry raw-key `KeyObject` LRU; `VALIDATION_WORKER_NODE_ED25519=false`
restores the CML verifier at runtime. Inline and browser callers omit the local
verifier context and remain the CML reference path. The Node verifier is gated
against the official C2SP Wycheproof `ed25519_test.json` pinned at commit
`fc24cd5b787d8e496bff31b0468af693a652b0f2` (file SHA-256
`70471c053c711731f2195ef4875b60ea7f5d6793939d99058ac12da810cb8e00`):
150/150 applicable vectors, 88 valid, 62 invalid, zero acceptable, and zero
Node/CML verdict divergence. The pre-integration 256-key diagnostic measured
Node at 3.22× CML for valid signatures, 3.15× for invalid signatures, and 3.14×
for a randomized valid/invalid mix, with zero verdict divergence.

**5. MN: pool service `MN/services/validation-pool.ts`.** An Effect service (`Layer.scoped`) exposing `runPhaseAChunk(txs): Effect<PhaseAResult>` and `evaluateScript(scriptBytes, contextCbor): Effect<LocalScriptEvalResult>`. Lifecycle:

- **Startup:** eagerly spawn `VALIDATION_WORKER_POOL_SIZE` workers during `listen` startup (fail fast if the entry doesn't resolve — same error surface as `resolve-worker-entry.ts:23-25`); a readiness ping (`kind: "phase_a"`, empty batch) confirms WASM init before the fiber tree reports ready.
- **Dispatch:** FIFO job queue; one in-flight job per worker (chunks are already batched, so per-worker pipelining adds nothing but reordering risk). Phase A batches are split into chunks of `VALIDATION_WORKER_CHUNK_SIZE` (default 64) so UPLC jobs interleave between chunks rather than waiting behind a whole batch (head-of-line mitigation).
- **Backpressure:** bounded queue (`poolSize × 4` jobs); enqueue past the bound blocks the calling fiber (Effect queue semantics) — drain loops naturally throttle claim rate.
- **Crash/respawn:** on `error`/nonzero `exit` (handler pattern per `block-commitment.ts:605-629`) or `job_failed`, all jobs assigned to that worker fail with `WorkerError`; the pool respawns a replacement (backoff 250 ms → 5 s, `validation_worker_restart_count` metric). A failed Phase A chunk fails the batch's tick, which already releases the lease for retry (`tx-queue-processor.ts:503-510`) — no new recovery machinery.
- **Job timeout:** `VALIDATION_WORKER_JOB_TIMEOUT_MS` (default 30 000, matching `VALIDATION_LEASE_MS` default, `config.ts:308-310`); a timed-out worker is terminated and respawned (a wedged CEK loop cannot be interrupted cooperatively).
- **Shutdown:** scope finalizer terminates all workers (mirrors `Effect.sync(() => worker.terminate())`, `block-commitment.ts:630-632`).
- **Inline fallback:** batches smaller than `VALIDATION_WORKER_INLINE_THRESHOLD` (default 32) run `runPhaseAValidation` inline on the coordinator — pool round-trip latency isn't worth it for trickle traffic, and this keeps the single-threaded reference path permanently alive for differential testing and rollback (`VALIDATION_WORKER_POOL_SIZE=0` forces it, §7.4).

**6. MN: cache service.** Move `cachedUtxoState`/`cachedUtxoStateVersion` (`tx-queue-processor.ts:145-146`) into `MN/services/mempool-ledger-cache.ts`: holds the Map, the version cursor, a `Semaphore(1)`, and `withPhaseBLock<A>(effect): Effect<A>`. Implements delta-journal consumption per §2.7 (apply deltas or full reload) inside the lock. Mutator changes: `project-deposits-to-mempool-ledger.ts:128`, `block-confirmation.ts:125-130`, `genesis.ts:57` publish `{upserts, deletes}` or `full: true` alongside the version bump.

**7. MN: rewire `txQueueProcessorAction`** (`tx-queue-processor.ts:337-511`): replace the `runPhaseAValidation` call (`:425-431`) with pool dispatch (chunk → `runPhaseAChunk` → concat, order preserved by chunk index); drop `selectPhaseAConcurrency` (`:309-331`) — its job is now pool sizing; wrap `:442-495` (cache read → Phase B → markRejected/markAccepted → patch → version sync) in `withPhaseBLock`; delete the `:141-143` constants in favor of config (§4). Replace the `TX_QUEUE_PROCESSOR_ACTIVE` boolean gate (`:538-547`) with an active-loop counter: `txQueueProcessorDrainOnce` starts a loop iff `active < VALIDATION_DRAIN_LOOPS`; wake coalescing behavior otherwise unchanged (`:550-561`).

**8. MV: conflict components** per §2.6 — replace `buildConflictBuckets` (`phase-b.ts:605-623`) with `buildConflictComponents`; inner loop of the wave processing (`:971-996`) becomes: components via `Effect.forEach(components, { concurrency: bucketConcurrency })` for the _validation_ stage, each component internally sequential; decision application unchanged (`:998-1044`). `conflict`/`hasIntersection` (`:158-167, :216-219`) survive only in tests as the oracle.

**9. MN: UPLC offload wiring.** `txQueueProcessorAction` passes `evaluateScript: pool.evaluateScript` in `PhaseBConfig` when `VALIDATION_UPLC_IN_WORKERS=true` (default). Coordinator still runs `encodeScriptContextCbor`.

**10. Config, metrics, poll:** §4 and §5; `listen.ts:363` becomes `txQueueProcessorFiber(mkSchedule(nodeConfig.TX_QUEUE_POLL_INTERVAL_MS))`.

**11. Tests & benchmarks:** §6, extending the existing `MN/tests/validation-parallelization.test.ts`, `MN/tests/benchmarks/validation-benchmark.bench.ts`, `MN/tests/benchmarks/native-phase-a.bench.ts`, `MV/tests/phase-a.test.ts`, `MV/tests/phase-b.test.ts`.

---

## 4. Config surface

All via Effect `Config.integer(...).pipe(Config.withDefault(...))` in `MN/services/config.ts`, declared in the `NodeConfig` type block (`config.ts:49-68` region), following existing style (`config.ts:255-269`).

| Env var                              | Default                                          | Replaces / notes                                                                                                           |
| ------------------------------------ | ------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------- |
| `VALIDATION_WORKER_POOL_SIZE`        | `0` = auto: `max(1, availableParallelism() − 2)` | new; `0` disables the pool entirely → inline single-thread path (rollback lever)                                           |
| `VALIDATION_WORKER_CHUNK_SIZE`       | `64`                                             | txs per Phase A job message                                                                                                |
| `VALIDATION_WORKER_INLINE_THRESHOLD` | `32`                                             | batches below this skip the pool                                                                                           |
| `VALIDATION_WORKER_JOB_TIMEOUT_MS`   | `30_000`                                         | aligned with `VALIDATION_LEASE_MS` default (`config.ts:308-310`)                                                           |
| `VALIDATION_WORKER_NODE_ED25519`     | `true`                                           | worker-local Node/OpenSSL verifier; `false` restores the CML reference verifier                                            |
| `VALIDATION_BATCH_HARD_CAP`          | `8_192`                                          | was hidden constant `1600` (`tx-queue-processor.ts:141`)                                                                   |
| `VALIDATION_MIN_BATCH`               | `128`                                            | was hidden constant (`:142`); now visible, same default                                                                    |
| `VALIDATION_BATCH_SIZE`              | raise default `1000` → `2_048`                   | existing var (`config.ts:255-257`); measured whole-system default                                                          |
| `VALIDATION_PHASE_A_CONCURRENCY`     | unchanged (`32`)                                 | now governs only the **inline** fallback path; the hidden `=8` clamp (`:143`) is deleted                                   |
| `VALIDATION_DRAIN_LOOPS`             | `4`                                              | new (§2.5); measured whole-system default                                                                                  |
| `VALIDATION_UPLC_IN_WORKERS`         | `true`                                           | escape hatch to inline CEK eval                                                                                            |
| `VALIDATION_LEDGER_DELTA_LOG_MAX`    | `64`                                             | delta journal ring size (§2.7)                                                                                             |
| `TX_QUEUE_POLL_INTERVAL_MS`          | `250`                                            | was hard-coded `500` (`listen.ts:363`); poll is a recovery sweep only — event wakeups drive steady state (§1.3 correction) |

**Sizing arithmetic, recalibrated with the whole-system rehearsal.** Six workers with 2,048-tx batches and four drain loops sustained 9,869.475 tx/s in the asserted two-replica short run, with p99 777 ms, Phase-A speedup 5.822×, and serialization at 0.469% of Phase A. A controlled 4,096-tx/two-loop rehearsal regressed to 7,794.933 tx/s and p99 1,047 ms, so the measured 2,048/four-loop topology is the default. It keeps multiple Phase-A claims in flight while limiting the claim, Phase-B, and terminal-persistence work per serialized batch. The 8,192 hard cap remains an explicit override envelope, not the default batch size; 2,048 rows still retain more than 30× margin under the 30 s lease at the measured p99.

---

## 5. Observability

Existing metrics kept as-is (names verified at `tx-queue-processor.ts:24-139`): `validation_phase_a_latency_ms`, `validation_phase_b_latency_ms`, `validation_batch_size`, `validation_accept_count`, `validation_reject_count`, `validation_queue_depth`, `validation_worker_utilization` (note: despite the name this measures _batch-capacity_ utilization, `:60-66` — keep for continuity, description updated), `validation_phase_a_effective_concurrency` (now reports pool size used), `validation_oldest_queued_tx_age_ms`, `validation_queue_wait_duration`/`_max_ms`, `validation_event_wakeup_count`, `validation_coalesced_wakeup_count`, and the timers `validation_batch_duration`, `validation_phase_a_duration`, `validation_phase_b_duration`, `validation_mempool_insert_duration`, `validation_rejection_insert_duration` (`:116-139`).

New, same naming family:

- `validation_pool_size` (gauge), `validation_pool_busy_workers` (gauge), `validation_pool_queue_depth` (gauge) — sampled at job enqueue/complete.
- `validation_pool_job_duration` (timer, labeled by job kind `phase_a`|`uplc` if the metric layer supports tags; else two timers `validation_pool_phase_a_job_duration`, `validation_pool_uplc_job_duration`).
- `validation_pool_serialize_duration` / `validation_pool_deserialize_duration` (timers) — the §2.2 clone-cost watchdogs.
- `validation_worker_restart_count` (counter), `validation_worker_job_timeout_count` (counter).
- `validation_phase_b_lock_wait_duration` (timer) — time drain loops spend waiting on the Phase B semaphore; the direct signal for tuning `VALIDATION_DRAIN_LOOPS`.
- `validation_ledger_cache_delta_apply_count` / `validation_ledger_cache_full_reload_count` (counters) — B3(c) regression guard: full reloads should be ~zero in steady state.
- `validation_drain_loops_active` (gauge).
- Per-worker heartbeat folded into readiness via the existing pattern (`HEARTBEAT_TX_QUEUE_PROCESSOR`, `globals.ts:92`): pool reports unhealthy if any worker has an in-flight job older than the timeout.

---

## 6. Test & verification plan

**6.1 Determinism / differential.** The single-threaded path survives intact (inline fallback, §3 item 5), so the harness is cheap: corpora via the `stress-corpus-generate` chain-builder machinery (script txs, conflicting spends, dependency chains, cycles, bad signatures, budget-exceeding scripts); assert byte-identical accepted sets, rejected sets (`txId`+`code`), and state patches — same batch, same pre-state — across (a) inline single-thread, (b) pool×2, (c) pool×6 + 2 drain loops (single batch injected, excluding batch-split effects by construction). Also: old `buildConflictBuckets` vs new `buildConflictComponents` verdict equivalence on randomized conflict graphs (property test, old code as oracle, `MV/tests/phase-b.test.ts`). Wire codec round-trip: `deserialize(serialize(c))` deep-equals `c` (Buffer semantics restored — `.equals`/`.toString("hex")` work — `Constr` prototypes reinstated, bigints/Maps intact).

**6.2 Unit.** Pool: dispatch order, chunk reassembly ordering, backpressure blocking, timeout→terminate→respawn, crash mid-batch fails the tick (assert `releaseForRetry` observed via lease state), shutdown terminates all workers. Cache service: delta application ordering, gap→full-reload fallback, `full:true` handling, concurrent `withPhaseBLock` mutual exclusion; a test that registers a fake mutator bumping the version _without_ a delta and asserts the full-reload fallback fires (guards the §2.7 contract). Extend `validation-parallelization.test.ts` to cover 2 drain loops claiming disjoint batches with overlapping conflicting txs and assert no double-accept across batches.

**6.3 Crash recovery.** Kill a worker (`worker.terminate()` injected) mid-Phase-A: batch lease released, txs re-claimed after `VALIDATION_RETRY_BACKOFF_BASE_MS`, eventually accepted; no tx lost, no tx double-inserted into mempool (idempotency via existing `tx_admissions` status machine). Kill the process mid-batch: lease expiry requeue (`requeueExpiredLeases`, `txAdmissions.ts:230-242` region) recovers — covered today, re-asserted with N loops.

**6.4 Benchmarks (extends `validation-benchmark.bench.ts`).** Assertions gated in CI-perf (8-core runner):

- Phase A pool throughput ≥ 4× inline single-thread throughput at 6 workers (scaling sanity).
- End-to-end Stage B (pre-loaded `tx_admissions`, admission off): **≥10 000 accepted tx/s sustained for 5 min**, p99 `validation_batch_duration` ≤ 1 s, `validation_pool_serialize_duration + deserialize` ≤ 10% of `validation_phase_a_duration`.
- Script-heavy corpus (every tx one Plutus spend): no coordinator-thread saturation (event-loop lag < 50 ms p99) — proves UPLC offload.
- B3(c): with a deposit projected every 5 s during the soak, `validation_ledger_cache_full_reload_count` stays 0 and throughput does not dip >5% at bump points.
- 24 h leak soak at 2 500 tx/s: aggregate process RSS growth < 10%/day and every stable worker slot's comparable footprint (`used_heap_size + external_memory`, including worker-local WASM/ArrayBuffer memory) grows < 10%/day; any worker replacement fails the gate (§7.1).

---

## 7. Risks & rollback

**7.1 Per-worker crypto memory growth.** Each worker still holds a CML instance for canonical Cardano decoding and the runtime rollback path; wasm-bindgen linear memory grows on demand and is never returned to the OS, and the CML API requires explicit `.free()` on many objects. The default Ed25519 hot path instead holds Node `KeyObject`s in a strict 4,096-entry LRU. Its production-bound unit probe imported 4,097 distinct raw keys, retained exactly 4,096, evicted one, and measured 40,108,032 bytes (38.25 MiB) RSS growth. Mitigations: exception-safe frees on the remaining CML hot path; bounded Node key cache with fail-closed imports; aggregate process-RSS plus stable-slot worker heap/external-memory sampling and leak soak (§6.4); optional worker recycling after N jobs (add later only if the soak fails — deliberately not spec'd now). Worst case: pool of 6 × ~100–300 MB steady-state is acceptable on the target box; unbounded growth is not, and recycling caps it.

**7.2 Verdict divergence between worker and inline paths.** Same package, same code — but the wire codec is a new seam where a missed field silently changes Phase B behavior (e.g., a dropped `derived` array turns a reject into an accept). Mitigations: exhaustive field-mapped codec (no blanket clone, compile-time exhaustiveness, §3 item 2), round-trip property tests, and the §6.1 differential suite run in CI on every change to `MV/`. Residual risk low but nonzero → the inline path remains the reference implementation and rollback target.

**7.3 Ordering/nondeterminism in conflict resolution.** Stated explicitly: **Phase B decision-making stays sequential** — one Phase B at a time process-wide (semaphore, §2.5), decisions applied in deterministic wave/component order within a batch (§2.6, `phase-b.ts:998-1044` unchanged). Parallelism exists only in per-tx Phase A (order-independent, reassembled in input order), read-only candidate validation within a wave, and UPLC evaluation (pure function of script+context). The only added nondeterminism is which batch a tx lands in under N>1 loops — the same boundary effect that exists today across sequential batches (§2.5), operator-local and not consensus-relevant. Differential tests (§6.1) pin everything else.

**7.4 Rollback levers** (all runtime, no redeploy): `VALIDATION_WORKER_POOL_SIZE=0` → inline single-thread Phase A (pre-plan behavior); `VALIDATION_WORKER_NODE_ED25519=false` → CML signature verification inside workers; `VALIDATION_UPLC_IN_WORKERS=false` → inline CEK eval; `VALIDATION_DRAIN_LOOPS=1` → single-flight; `VALIDATION_BATCH_HARD_CAP=1600` + `VALIDATION_BATCH_SIZE=1000` → old batch envelope. The delta journal's full-reload fallback (§2.7) is always present. Combined, the pre-Phase-2 configuration is exactly reproducible.

**7.5 Head-of-line / pool starvation.** UPLC jobs behind Phase A chunks: mitigated by 64-tx chunking (§3 item 5); if profiles still show UPLC latency spikes, split the pool into two sub-pools by job kind (config split of the same worker entry — noted, not spec'd). Commit-worker contention: the −2 reservation (§2.1); watch Stage C `commit_worker_duration` for regression during Stage B soaks.

---

## 8. Interface contracts

**8.1 What Phase 3 (MPF rework) and the commit planner rely on — unchanged by this plan.** The validated-batch shape reaching the block builder is untouched: `markAccepted` still receives `ProcessedTx[]` (`MV/ledger.ts:24-29`) via `processedTxFromValidatedTx` (`validation-candidate.ts:114-128`) and persists through `MempoolDB.insertMultiple` (`mempool.ts:87-142`) into the same five tables; accepted-tx ordering by `arrivalSeq` is preserved (`phase-b.ts:1065-1071`); `tx_admissions` status transitions (`queued → validating → accepted|rejected`) and lease semantics are unchanged. Phase 3 may assume Stage B can feed ≥10k tx/s into `mempool` and that `validation_*` metrics keep their names (§5).

**8.2 What this plan relies on from Phase 1.** (a) The accepted-path DB write (`markAccepted`/`insertMultiple`) sustaining ≥10k rows/s in bulk — if Phase 1's A4 write-behind items slip, the §4 arithmetic degrades and `VALIDATION_DRAIN_LOOPS` must rise to keep pipelining ahead of a slower DB stage; (b) batched `markRejected`; (c) the split/enlarged Postgres pool so N drain loops + workers' coordinator I/O don't exhaust the hard-coded 20 connections (parent plan A2). This plan deliberately does **not** touch the admission handler beyond reading its wakeup contract (`listen-router.ts:1930-1932`).

**8.3 What this plan constrains in `@al-ft/midgard-validation`.** The package stays pure and thread-agnostic: no `worker_threads`, no pool, no Node-only services — new exports are the single-tx validator, the wire codec, the evaluator seam, and `buildConflictComponents`. `runPhaseAValidation` / `runPhaseBValidationWithPatch` signatures stay backward-compatible (new `PhaseBConfig` field optional) because `demo/lucid-midgard/src/builder.ts` consumes them directly for client-side pre-validation and MUST keep producing verdicts identical to the node's (that equivalence is itself covered by `lucid-midgard/tests/local-validation-shared.test.ts`). A follow-up may introduce a narrowed `PhaseBCandidateTx` type (dropping `vkeyWitnesses` et al., §2.2) — coordinate with lucid-midgard before changing `PhaseAValidatedTx` itself.

---

## 9. Implementation evidence (2026-07-10)

The B1–B3 implementation, no-second-copy accepted payload path, worker-local
Node verifier with CML rollback, and whole-system benchmark topology are in the
worktree. Focused evidence is green: Phase-A/codec/strict-outref differential
40/40; verifier/pool/differential/crash 20/20 before the startup self-test and
14/14 after its addition; validation typecheck/build; exact validation-worker
bundle; node entry bundle; touched ESLint/Prettier; database focused suite
28/28. The post-optimization focused PostgreSQL acceptance suite is 7/7, and
the full node typecheck is green.

The pinned C2SP Wycheproof gate is exact (150/150; 88 valid, 62 invalid, zero
acceptable, zero Node/CML divergence). The production 4,096-key cache retained
exactly 4,096 of 4,097 imported keys, evicted one, and measured 39,059,456 bytes
RSS growth in the final focused run.

The stronger all-container benchmark preflight passed: `node:22` v22.22.2 and
`postgres:15.15-alpine`, both AutoRemove, on one private network and the same
eight distinct physical cores; PostgreSQL data on tmpfs; no published ports;
six validation workers. The asserted short two-replica run accepted all
51,200 transactions with zero loss/rejection, p99 777 ms, Phase-A speedup
5.822×, serialization ratio 0.469%, and 170.02 MB/worker RSS upper bound, but
throughput was only 9,869.475 tx/s (replicas 9,730.996 and 10,011.953), below
the 10,500 tx/s rehearsal margin and the 10,000 tx/s exit criterion in
aggregate. Its evidence JSON was fingerprinted before the subsequent tuning
run as SHA-256
`bf927a2c46e9aad14db3cfdf82bd225e7f87e7c826aa12c0a5e2d6520d63c7c4`.

One isolated tuning rehearsal then tested batch 4,096 with two drain loops,
holding the six-worker/chunk-128 topology constant. It also accepted all
51,200 transactions with zero loss/rejection, but regressed to 7,794.933 tx/s
(replicas 7,613.765 and 7,984.932) and p99 1,047 ms. Phase-A speedup remained
6.172×, serialization ratio was 0.722%, and the RSS upper bound was 180.03
MB/worker. That tuning evidence JSON was fingerprinted at SHA-256
`37989bdad72ea27729585f2e5497f0bec08c088aada315c2e6e9c5ef71ad449f`.
The tuning configuration is rejected; no further parameter search is justified
without reviewing the measured claim/persist path.

The measured write-path follow-up retained synchronous terminal durability and
replaced expanded accepted-id/spent-outref `IN` parameters with one binary-safe
array parameter per predicate. `@effect/sql-pg` 0.35.1 classifies raw
`Buffer[]` as `text[]`, so every element is encoded in PostgreSQL's canonical
`\xHEX` bytea text form before the explicit `bytea[]` cast. Real-PostgreSQL
coverage proved ordered high-bit/NUL/duplicate round trips, an actual 2,048-row
compact accept, deposit consumption parity, lease-mismatch rollback,
missing-payload rollback, and worker-crash reclaim/accept (7/7).

On that patch, a fresh asserted all-container batch-2,048/four-loop rehearsal
accepted all 51,200 transactions with zero loss/rejection at 10,267.714 tx/s
(replicas 9,942.027 and 10,615.461), a 4.04% improvement over the prior
9,869.475 tx/s run. P99 was 757 ms, Phase-A speedup 5.234×, serialization was
0.432%, and the RSS upper bound was 173.76 MB/worker. This clears the 10,000
tx/s rate on the short sample but misses the 10,500 tx/s rehearsal margin by
2.21%, so the five-minute gate was not started. The rehearsal explicitly used
`BENCH_PHASE2_CHUNK_SIZE=128`; production and `.env.example` retain the
script-latency-conservative chunk default 64. Therefore this evidence does not
prove throughput for the exact production-default chunk size, and that default
must not change without the script-heavy latency gate. The current evidence
JSON is SHA-256
`c5d00a2ac1ce383f28584c20e4840f1fb967897fdd054f82e7c219190c982de5`.
That artifact covers the accepted-id/spent-outref array patch only. A subsequent
fixed-shape produced-UTxO `UNNEST` optimization is staged in the worktree but
has not yet cleared its PostgreSQL correctness or performance reruns, so the
artifact must not be attributed to that later code.

Therefore the five-minute and 24-hour gates have **not** run and this plan is
not complete. The remaining blocker is sustained-throughput and soak evidence,
not the short-sample rate, Phase-A scaling, serialization, memory, correctness,
or benchmark topology.

The exact repeatable closure matrix is maintained in
`docs/benchmark-scenarios/phase-2-validation-gates.md`. Its fail-closed report
parser covers the three-control/three-candidate write-behind experiment, the
strictly interleaved three-replica chunk-64/chunk-128 experiment, the unchanged
production-default chunk-64 rehearsal, the full five-minute Stage B gate, the
script-heavy event-loop gate, and the 24-hour leak soak. The chunk experiment
requires every report and replica to reach 10,000 tx/s, a chunk-128 median of
at least 10,500 tx/s, and at least 3% median improvement over chunk 64. Its
timestamp-bound report identities prevent mixing retained artifacts from
different experiments. Passing it does not authorize changing the chunk-64
production default. The separate asserted five-minute chunk-128 script-heavy
candidate now requires an exact 128 chunk, all-Plutus-V3 transactions, worker
UPLC evaluation, exact inline verdict/state-patch parity, zero rejection, and
event-loop p99 below 50 ms. It independently rehashes the Stage B corpus and
records the same experiment, topology, and Node runtime identity. Only the
joint `authorize-chunk128-default` parser can authorize a default change, after
revalidating the six interleaved reports and the bound candidate together;
either standalone parser remains non-authorizing. These commands are evidence
tooling only; they do not alter production defaults.

### 9.1 Closure audit checkpoint (2026-07-10)

A fresh correctness review found and fixed two B3(c) edge cases. A full cache
reload now brackets its database snapshot with journal reads and replays deltas
published during the snapshot instead of stamping past them. Deposit projection
now publishes an empty incremental delta while an unconfirmed projected deposit
is intentionally hidden from `retrieveSpendable`; recovery of a missing deposit
already assigned to a confirmed header publishes the exact spendable upsert.
Focused cache tests cover both snapshot races, gap/full fallback, ordering, and
cross-loop double-spend protection. Real PostgreSQL coverage against disposable
database `midgard_phase1_goal_20260710` passed the two-loop disjoint claim,
worker-crash lease release/reclaim, hidden projection delta, and
header-assigned recovery upsert (4/4).

The worker differential now runs the complete adversarial corpus through pool
sizes 2 and 6 and compares accepted IDs, rejection code/detail, and the exact
state patch against inline Phase A/Phase B. It includes a shared-spend conflict,
a dependency chain, a defensive cycle, an invalid signature, a valid Plutus V3
spend, and a zero-budget Plutus spend. The focused pool/cache/verifier/parser
set passed 35/35; the validation package codec/Phase-A/Phase-B/canonical fast
path set passed 49/49; the pinned C2SP Wycheproof gate again passed all 150
vectors with zero Node/CML divergence. The integrated workspace build was green,
and validation plus node typechecks were green at the Phase 2 checkpoint.

Unpinned one-second diagnostic runs (evidence only, not exit gates) passed exact
verdict/state-patch comparison at batch sizes 64 and 2,048 with pool sizes 2 and 6. The 2,048/6 result was 10,876.209 tx/s, p99 248.507 ms, 4.740× Phase-A
speedup, and 6.297% serialization. The quick all-Plutus worker run accepted
1,088/1,088 with zero rejection and event-loop p99 14.860 ms. These samples do
not replace the pinned five-minute or script-heavy duration gates.

The corpus tooling previously generated and verified 1,650,688 rows at
`demo/midgard-node/logs/phase-1-full-corpus-20260709T002743Z`, but that artifact
is not a valid five-minute gate. Summing two independent ~165-second replicas
does not prove a continuous five-minute run. The formal parser and runner now
require each replica to sustain at least 10,000 accepted tx/s for at least 300
seconds, require at least 3,780,000 unique rows per replica (12,600 tx/s of
five-minute capacity), recompute throughput from accepted rows and elapsed
time, and bind the run to an exact operator-declared corpus SHA-256 and row
count that the preload path recomputes. The previously retained 3,063,808-row
live-bound corpus can cover only 10,212.69 tx/s for 300 seconds and therefore
remains insufficient for this private Stage B gate. Section 9.5 records the
newly verified isolated corpora that remove this fixture-capacity blocker
without mislabeling synthetic funding as live evidence. The remaining open
evidence is the exact-default rehearsal, pinned five-minute Stage B gate (including
5-second deposit delta bumps), pinned five-minute chunk-64 script-heavy gate,
the bound chunk-128 candidate if the chunk A/B experiment passes, and the
24-hour 2,500 TPS leak soak. Implementing the candidate gate does not itself
authorize or change the chunk-64 production default.

### 9.2 Pinned write-behind A/B result (2026-07-10)

The fail-closed three-control/three-candidate write-behind experiment ran in
the exact eight-core container topology: node and PostgreSQL shared one private
network and CPU set `0,2,4,6,8,10,12,14`; the node image reported v22.22.2;
PostgreSQL 15.15 used tmpfs with no published ports; and both containers were
fingerprinted before cleanup. Every replica accepted 51,200/51,200 transactions
with zero rejection or loss. Phase-A speedup ranged from 5.003x to 5.876x,
serialization stayed between 0.408% and 0.673%, and p99 was 782-822 ms.

The three `VALIDATION_WB_BATCH_SIZE=1000` controls measured 9,313.867,
9,820.233, and 9,872.244 tx/s (median 9,820.233). The three isolated
`VALIDATION_WB_BATCH_SIZE=2048` candidates measured 9,220.428, 9,600.449,
and 9,951.433 tx/s (median 9,600.449), a 2.238% regression. The report parser
also correctly failed the first control below its 10,000 tx/s floor. The
candidate is therefore rejected and the production write-behind default stays
at 1,000. Because the controls themselves did not clear the throughput gate,
the exact-default and full-duration runs were held; parameter tuning is not a
substitute for removing the measured serialized claim path.

Control artifact SHA-256 fingerprints, in run order, were
`0b778389b4f59bbec9da781950bf7f8ebc8b0426ab402d1b47421466639609d3`,
`9280e651f1b9d257282475e9d9dc7ee86bac2284e95cc2209439713cede92818`, and
`daaeb9086f6e7854ca9f8cbf627bfb970c2957c6015744343c79df4fde3d7589`.
Candidate fingerprints were
`add33fea09383b494bc55e4149f621377d16c0f2db5ebb001454006bd555301c`,
`af48d14a78d1660c39855a7bece360bc195971327384c92947211e0eb14cb3b8`, and
`e43658cba61310980ec40f715bd93838c687888831d34a3f6d15c915843b1daf`.

### 9.3 Ordered decision/persistence pipeline checkpoint (2026-07-10)

A pinned five-second CPU-only isolation at batch 2,048, six workers, and chunk
128 accepted 53,248 transactions with zero rejection. Phase A alone sustained
20,789.669 tx/s at 98.510 ms/batch; complete in-memory Phase A plus Phase B
sustained 10,323.630 tx/s. Native Phase A is therefore not the whole-system
Stage B floor.

Stage B now uses two exact ordered stages: Phase B decisions advance a
sequence-numbered speculative cache overlay while terminal persistence remains
strictly ordered. Failure or interruption at sequence N poisons N and every
later sequence, prevents later terminal effects, requeues their leases, waits
for every earlier persistence, reloads durable cache state, and advances to a
fresh epoch. A durable-base plus ordered-overlay cache preserves an unpersisted
N across an external full-reload marker. Deterministic tests prove injected
persistence failure, interruption while an earlier persistence is blocked,
zero later terminal writes, durable reload, and fresh-epoch recovery. The
focused cache/parallel suite passed 18/18, node typecheck and touched lint were
green, and the post-wrapper real-PostgreSQL suite passed 4/4.

The exact pinned production-default rehearsal (`WRITE_BEHIND_MAX_BATCH=1000`,
chunk 64, batch 2,048, four drains, six workers) accepted all 51,200
transactions with zero rejection or loss, p99 815 ms, 5.182x Phase-A speedup,
and 0.537% serialization, but sustained only 9,081.158 tx/s. Claim averaged
113.813-122.688 ms and terminal persistence 154.615-156.923 ms. Deferred
projection SQL accumulated 1,769-1,824 ms per replica and left 13,200-21,200
rows for the final flush. Its SHA-256 evidence fingerprint is
`e41fe4dee52e050bf43f4fcdbd35911ad0847be928bbd53855d28c94c5b3058d`.

One bounded fixed-shape `UNNEST` transport experiment for the two deferred
projection writers passed its binary/conflict/rollback/retry correctness gates
but reached only 9,305.729 tx/s (+2.47%), below the 10,500 rehearsal margin.
It was fully reverted, including experiment-only tests; projection writers,
queue behavior, and defaults are unchanged. The rejected artifact SHA-256 is
`878160de42912b82c3e1d1ae69159632b0c3104d43c1dfe73e827985ff73559b`.

No repeated rehearsal, full five-minute Stage B run, script-heavy duration
gate, or 24-hour soak followed these failures. The remaining blocker is the
whole-system claim/terminal/write-behind boundary, not Phase-A scaling,
correctness, topology, p99, serialization, or a tunable default.

A final exact-default, one-replica CPU attribution captured the coordinator
and all six worker isolates. Workers were balanced and 61.81-64.46% idle
(62.96% aggregate); their inclusive active paths were Phase A validation
(29.78% of all samples), submitted/native decode (18.02%/11.01%), hashing
(10.56%), and vkey verification (8.35%). The coordinator was 23.14% idle;
its largest self category was GC at 16.84%, followed by program/runtime at
8.19%, inspector overhead at 3.92%, and plain Phase-B component validation at
3.27%. PostMessage was 0.73% self and measured serialization was 0.534%.
There is no concrete removable coordinator or worker CPU cost large enough to
justify another architecture change.

The same run's wall metrics instead measured claim at 135.56 ms average,
ordered persistence at 170.46 ms, deferred projection SQL at 1,916 ms total,
and a 15,200-row final projection drain taking 248.7 ms. These overlapping
PostgreSQL-facing boundaries span essentially the complete 3.178-second
replica. The run accepted 25,600/25,600 with no rejection or loss; its CPU
profiling overhead reduced the diagnostic rate to 8,056.040 tx/s, so it is
attribution evidence rather than a throughput gate. The report SHA-256 is
`4d20793ce58541151ce9cb7486d6b9a6015964a2f837bf09d7cf97880a3b7961`;
coordinator and per-worker `.cpuprofile` artifacts are recorded beside it.

### 9.4 Closure gate hardening checkpoint (2026-07-14)

The script-heavy parser now requires the production gate to identify
`production_default_chunk64` exactly and requires every transaction to be
Plutus V3, exact inline state-patch parity, the exact `node:22.22.2` image
reference, and the immutable inspected `sha256:` image ID. The chunk-128
candidate retains its separate mode and must satisfy the same language,
state-patch, and image identity checks. Negative tests cover missing and
mismatched fields, including divergence from the operator-declared immutable
image ID.

The 24-hour leak gate now runs a separate five-minute steady-state warmup at
2,500 TPS before capturing either process or per-worker memory baselines; all
24 measured hours follow that exclusion window. Stage B reports must recompute
both replica and aggregate throughput from accepted counts and measured
duration, preventing padded elapsed-time or declarative TPS fields from
claiming a sustained pass. Focused report tests passed 30/30; touched lint,
formatting, the full node typecheck, and diff whitespace checks were green.
These are gate-tooling results only. They do not supply the still-missing live
A/B, production-default, full, script-heavy, conditional chunk-128, or 24-hour
evidence.

The independent fail-open review then tightened four remaining evidence
boundaries. Every Stage B report now carries the inspected immutable Node image
ID, must match the operator-declared ID, and must retain that identity across
write-behind/chunk A/B and conditional chunk-128 authorization. Write-behind
A/B now has the same canonical, 24-hour-bounded run/database pairing discipline
as chunk A/B, so retained runs cannot be mixed. The full gate binds the active
deposit-projection interval to each replica duration with only a bounded
five-second final-flush allowance before deriving the required bump count. The
leak gate records warmup and measured accepted/rejected/batch counts, recomputes
both rates, requires exact requested five-minute and 24-hour windows, and gives
the test timeout enough room for both consecutive windows. These hardenings do
not create any live evidence. The expanded adversarial report suite passed
31/31 under the exact `node:22.22.2` image; scoped verifier syntax, lint,
formatting, the full node no-emit typecheck, and diff whitespace checks were
also green. Every live gate listed above remains open.

### 9.5 Isolated Stage B corpus closure (2026-07-14)

The private Stage B corpus prerequisite is now closed under
`demo/midgard-node/logs/phase2-formal-corpora-20260714/`. Its synthetic funding
is domain-separated, uses 256 or 4,096 unique first outrefs, is valid only for
the emulator/private-PostgreSQL preload path, and is explicitly marked
`liveOrProviderVisibleFunding=false`. It is not Phase 1, provider, Preprod, or
live-funding evidence. The original source-wallet composite fingerprint stayed
unchanged, and all wallet-bearing artifacts remain ignored with private file
permissions.

- Short prerequisite: 25,600 rows, 256 chains, depth 100. Corpus SHA-256
  `365a445dd7262fc100040f11f48f7008c02768a7598ef91d37cfd31152aba633`;
  index SHA-256
  `dd891ffdfe16ae1341993b05cfa5b358ab232d5ec029d270831346e7ec124d54`.
  Automatic and standalone verification each rebuilt all 256 chains and all
  25,600 rows.
- Full prerequisite: 3,780,608 rows, 4,096 chains, depth 923. Corpus SHA-256
  `82bf270920e80a0a704817d9007672d3eab34f723302401d3b221388008581fb`;
  index SHA-256
  `bd4c66f33c58ce39d5021e8bdb2a274ff52c9d59a8d15c0463078a0cc2218232`.
  Automatic and standalone verification independently selected and rebuilt
  the same five chains and 4,615 rows.

The full corpus now exceeds the fixed 3,780,000-row five-minute capacity
floor. This closes fixture generation and provenance only; the private Docker
matrix, five-minute Stage B/script-heavy gates, and 24-hour leak soak have not
yet run and remain mandatory.

### 9.6 Current production-default rehearsal (2026-07-14)

The first fresh pinned Docker rehearsal against the verified 25,600-row short
corpus is a measured **NO-GO**. It used `node:22.22.2` image ID
`sha256:62e4daa6819762bbd3072af77cc282ab72c631c4aed30dd7980192babaf385b3`,
`postgres:15.15-alpine`, eight distinct physical cores, six validation
workers, four drains, chunk 64, write-behind batch 1,000, and two warmups. The
retained report is
`demo/midgard-node/tests/benchmarks/output/phase2-production_default.json`
(SHA-256
`077cf6719bafdc929c16398d2008653e0018f45401cd34b442630ca2c5d3f95b`).

All 51,200 transactions were accepted with zero loss, rejection, queue
residue, or ledger mismatch, but aggregate throughput was only 8,684.227
tx/s. The two replicas measured 8,235.992 and 9,184.059 tx/s, and aggregate
p99 batch latency was 1,005 ms. Both numeric acceptance gates therefore fail
(at least 10,000 tx/s per replica and p99 at most 1,000 ms). The ephemeral
benchmark PostgreSQL container and private network were removed after the
run. Do not run or relabel the formal five-minute matrix until a
correctness-preserving optimization clears this short production-default
rehearsal with margin.

The post-run audit also found that the `production-default` offline verifier
passed `minimumReplicaAcceptedTps=0` even though this plan requires every
replica to clear 10,000 tx/s. The verifier now passes an explicit 10,000 tx/s
replica floor, and its 31/31 focused suite includes an aggregate-pass fixture
whose 9,900 tx/s replica is rejected. This closes the fail-open verifier gap;
it does not change the benchmark threshold or rehabilitate the failed report.

### 9.7 Claim-boundary optimization and diagnostic closure (2026-07-14)

The next correctness-preserving candidate removes two redundant PostgreSQL
lookups without changing lease or ordering semantics. The claim CTE now locks
the oldest eligible physical tuples and joins the statement-local update by
`ctid`; the payload loader scans the already indexed validating lease-owner
set rather than adding a second `tx_id = ANY(...)` filter. It still fails
closed unless the returned length and unique transaction-ID set exactly match
the claimed batch, and it retains canonical `(arrival_seq, tx_id)` ordering.
An independent review found zero P1/P2 issues in the query shape. Focused real
PostgreSQL coverage now includes an explicitly held oldest-row lock proving a
second claimant returns later rows through `SKIP LOCKED` before that lock is
released, plus disjoint claims, set mismatch, missing payload, expiry, and
crash-release cases. The current environment could not initialize the
disposable PostgreSQL layer, so those seven tests remain pending execution;
static lint, formatting, and the full node typecheck passed.

Stage-B reports now expose successful write-behind flush count/rows,
transaction duration including commit, deterministic tx-delta preparation and
CBOR time, delta SQL time, address-history SQL time, residual transaction
overhead, and inline-fallback activation count. The final snapshot is taken
only after the explicit tail flush. This instrumentation changes no SQL,
batching, queue topology, defaults, or gate thresholds. Independent review
found zero P1/P2 issues; the focused telemetry/verifier suite passed 36/36,
the broader pool/cache/parallelization/verifier set passed 62/62, the
validation package set passed 49/49, and the full node typecheck passed under
Node 22.22.2.

Neither the query candidate nor the new telemetry is performance evidence.
The exact short production-default Docker rehearsal must be rerun with the
same pinned image/topology and must clear every per-replica throughput and p99
gate with margin before the five-minute matrix is authorized.
