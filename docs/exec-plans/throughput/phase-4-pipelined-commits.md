# ExecPlan: Throughput Phase 4 — Pipelined Commits (C3, D1)

**Status:** Implemented behind a strict opt-in; local final-tree gates pass, post-fix live acceptance and one-hour cadence evidence pending
**Effort:** weeks 8–14 of the throughput plan
**Owner:** TBD
**Depends on:**

- **Throughput Phase 3 (in-memory MPF overlay) — hard dependency.** Speculative build only pays off if building block N+1 fits inside block N's confirmation window; Phase 3's exit criterion (50k-tx block build <10 s, no O(total-ledger) rehydration per attempt) is what makes that true. This plan additionally _requires_ an overlay-lifecycle API from Phase 3 (§8.1) — fork/promote/discard on top of a base root — which Phase 3 must expose even if its own hot path doesn't need it.
- **Throughput Phase 1 (keyset-paginated `MempoolDB.retrieve`)** — the builder stage snapshots candidates repeatedly; a full `ORDER BY … DESC LIMIT 100000` re-read per attempt (`demo/midgard-node/src/database/mempool.ts:156-171`, the C4 bug) makes rebuild-after-invalidation needlessly expensive and starves old txs.
- Parent plan: `THROUGHPUT-2500-TPS-PLAN.md` §Phase 4 (items 1 and 3; item 2 — batched merges / D3 — is **out of scope** here, see §8.3).

**Exit criterion:** effective block cadence ≤ L1 confirmation latency + submit time (build time fully off the critical path, measured as overlap efficiency ≥90% at steady state); combined with Phase 3, Stage C ceiling ≥2,500 TPS at 50k-tx blocks.

---

## 1. Current state (verified)

All line numbers verified against the working tree on branch `tx-validation`, 2026-07-09. Where the parent plan's citations have drifted, a **Correction:** note is given.

### 1a. The commit trigger loop: phase lock, DB lease, worker thread

`blockCommitmentFiber` repeats `blockCommitmentAction` on a fixed schedule of `WAIT_BETWEEN_BLOCK_COMMITMENT` = 1,000 ms default (`src/commands/listen.ts:340-342`, `src/services/config.ts:234-236`). Each tick runs, in order (`src/fibers/block-commitment.ts:772-819`):

1. **Idle short-circuit** — skip if no mempool txs, no processed-unsubmitted txs, no pending user events, and no local finalization pending (`block-commitment.ts:184-212`, predicate at `:168-182`).
2. **Slot-aware due-work gates** — registered scheduler due-work can veto the tick (`:301-363`), fresh pre-lease scheduler evidence can register new due-work (`:376-394`), and a pre-lease scheduler alignment may run under its own pipeline phase (`:405-452`, `:509-521`).
3. **In-process phase lock** — `COMMIT_PIPELINE_PHASE` is a `Ref<CommitPipelinePhase>` in `Globals` (`src/services/globals.ts:6,38`); `acquireCommitPipelinePhase` CAS-es it from `"idle"` to `"scheduler_alignment"` or `"mutation_worker"` (`block-commitment.ts:463-473`; mutation-worker acquire/release at `:484-507`). This lock is **per-process only**: it prevents the scheduler-alignment probe and the mutation worker from overlapping inside one node process.
4. **DB lease** — `StateQueueMutationLeasesDB.tryWithLease("block_commitment", …)` wraps the whole build+submit attempt (`block-commitment.ts:799-810`). The lease is a Postgres row with `scope='state_queue'`, acquired via `INSERT … ON CONFLICT DO NOTHING` (`src/database/stateQueueMutationLeases.ts:197-237`), TTL-expired by a sweeper (`:104-118`, default TTL 10 min at `:17`), kept alive by a forked renewal fiber (`:323-344`), and released/failed on completion (`:382-443`). **This is the cross-process/cross-node mutual exclusion.** The same lease scope is also taken by the merge fiber (`src/fibers/merge.ts:340`), so commits and merges serialize against each other through Postgres, not through the in-process phase Ref.
5. **Worker thread** — `buildAndSubmitCommitmentBlockAction` spawns `commit-block-header.js` as a `worker_threads.Worker`, passing `availableConfirmedBlock`, `currentBlockStartTimeMs`, the lease token, base snapshot id, and processed-so-far counters (`block-commitment.ts:568-633`, input assembly `:573-585`). Before spawning, when local finalization is not pending, it refreshes the live state-queue snapshot and adopts the **live tail** as the commit base (`:547-562`).

### 1b. The worker: plan → barrier → hydrate → process → submit-or-defer

`databaseOperationsProgram` (`src/workers/commit-block-header.ts:521-972`) is the entire build sequence:

- **Candidate snapshot:** full `MempoolDB.retrieve` (`:535`) — `ORDER BY time_stamp_tz DESC LIMIT 100000` (`src/database/mempool.ts:165`) — plus `ProcessedMempoolDB.retrieve` (`:539`); `selectCommitTxCandidates` prioritizes already-processed (deferred) txs over new mempool txs (`:540-543`; planner at `src/workers/utils/commit-block-planner.ts:315`).
- **Pre-ingestion scheduler preflight** (`:556-598`) — may return `RegisteredDueWorkOutput` and end the attempt before any heavy work.
- **Inline ingestion barrier** (`:599-615`): `fetchAndInsertDepositUTxOsForCommitBarrier(new Date())`, then withdrawal and tx-order barriers chained on the returned timestamps. Each barrier is a _full_ L1 reconciliation of the visible user-event UTxO set (`src/fibers/fetch-and-insert-deposit-utxos.ts:239-258` → `runCommitTimeUserEventIngestionBarrier`, `src/fibers/user-event-ingestion.ts:71-105`). `userEventOnlyEndTime` is the **earliest** of the three barrier timestamps (`:609-615`). **Correction:** the parent plan cites this at `commit-block-header.ts:588-597`; it now lives at `:599-615`.
- **Scheduler window fit** (`:617-664`), scheduler-aware selection (`:654-680`), and **batch budgets** (`:681-696`) — `DEFAULT_COMMIT_BATCH_BUDGET_LIMITS` still caps `maxL2TxCount: 10_000` and models build cost at `estimatedCommitBuildMsPerTx: 1` (`commit-block-planner.ts:70-83`; Phase 3 item 5 recalibrates these).
- **Defer / recovery / idle short-circuits** (`:700-768`).
- **Base hydration** (`:770-784`): `resolveCommitBaseLedgerEntries` (`:137-224`) picks the base UTxO set from, in priority order: the **pending-finalization journal of the state-queue tip** (matched by tip header hash, `:180-210`, via `materializeConfirmedLedgerSnapshot`), the `confirmed_ledger` table (`:214-223`), or configured genesis UTxOs (`:153-177`). `alignCommitMpfsToBase` (`:226-266`) then compares the live LevelDB ledger-MPF root against the base root and, on mismatch, does a **full reset + re-insert of every base entry** (`:237-255`; `hydrateLedgerMpfFromLedgerEntries` at `src/workers/utils/mpf.ts:249-257`).
- **MPF processing** (`:786-813`): `processMpfs` over the candidate txs with the barrier times as visibility bounds.
- **The fork at `:879`** — this is the heart of C3/D1:
  - If `availableConfirmedBlock === ""` (previous submitted block not yet confirmed): the computed payload is **deferred** — txs move from `MempoolDB` to `ProcessedMempoolDB` and the worker returns `SkippedSubmissionOutput` (`:879-900`; `deferProcessedCommitPayloadUntilConfirmation` at `src/workers/commit-block-header/submission.ts:1063-1096`). **No header is assembled, nothing is submitted.**
  - Otherwise: `submitTxBackedCommit` (`submission.ts:642-1061`) or `submitDepositOnlyCommit` (`:315-640`).

**Correction (important):** the parent plan describes "the confirmation wait (~commit-block-header.ts:868-889)". The production worker **never blocks waiting for L1 confirmation**. The only in-worker `waitForTxConfirmation` (`commit-block-header.ts:343-377`, 120 s timeout at `:87`) belongs to `commitExplicitBlockHeaderProgram`, the operator fault-proof-drill command (`:419-519`) — not the production path. In production, "waiting" manifests as the defer branch at `:879-900` plus the confirmation fiber's gating of `AVAILABLE_CONFIRMED_BLOCK` (§1d). The pipelining problem is therefore not "remove a blocking wait" but "make the work done during the gated interval durable and submission-ready instead of throwaway."

### 1c. Submission path and the pending journal

`submitTxBackedCommit` (`submission.ts:642-1061`), per attempt:

1. `revalidateStateQueueLease` (`:833`; helper at `src/workers/commit-block-header/pending-journal.ts:300-306`) — fails hard without a live lease token.
2. `resolveLiveTailCommitBase` (`:834`; `pending-journal.ts:138-177`) — re-fetches the live tail from L1; aborts with "Commit base is stale" if the tail moved to a different logical header. **Correction:** the parent plan cites `pending-journal.ts:115-177` as "the confirmation wait"; those lines are actually the stale-base guards `assertLiveTailCommitBase` (`:115-136`) and `resolveLiveTailCommitBase` (`:138-177`) — pre-submit staleness detection, not confirmation machinery.
3. `buildUnsignedCommitTx` (`:836-847`) — assembles the O(1)-size header tx against the tail.
4. `PendingBlockFinalizationsDB.preparePendingSubmission` (`:876-923`) — writes the **journal**, status `pending_submission`, _before_ submission.
5. Re-validate lease + `assertLiveTailCommitBase` + `signAndSubmitProgram` (`:926-931`).
6. `markSubmitted` on success (`:1034-1046`) — status → `submitted_local_finalization_pending` (`src/database/pendingBlockFinalizations.ts:826-849`); the worker returns `SubmittedAwaitingConfirmationOutput`, and the parent fiber clears `AVAILABLE_CONFIRMED_BLOCK`, records `UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH`/`_SINCE_MS` (`block-commitment.ts:696-717`).

**What the journal actually records (verified, this matters for §2.3):** `buildPendingJournalMetadata` (`pending-journal.ts:54-95`) stores the lease token, base snapshot id, **base tail outref + header hash + full datum CBOR + base roots**, block start/end times, and expected roots/counts. `preparePendingSubmission` (`pendingBlockFinalizations.ts:632-819`) additionally stores complete member sets: every included mempool tx (id + CBOR), deposit/forced-tx/withdrawal entries, transition-trace and event-to-step members, and — critically — the **entire post-state UTxO set** of the new block (`utxoEntries`, populated from `utxoPayloadEntries` at `submission.ts:921` and `:535`). Completeness is asserted before submit (`assertPendingJournalCompleteness`, `pending-journal.ts:179-298`). It enforces a **single active journal row** at any time (`pendingBlockFinalizations.ts:707-719`: "Refusing to prepare a new pending block while another active pending-finalization record exists"; statuses at `:81-88`).

**Consequence:** the moment block N is _submitted_, this node durably knows N's full post-state — the journal's UTxO member set, whose root was asserted equal to N's header `utxosRoot`. That is exactly what `resolveCommitBaseLedgerEntries` consumes _after_ confirmation (`commit-block-header.ts:184-210`, refusing mismatched roots at `:192-199`). The parent plan's claim that the pending journal is "a natural foundation" for pipelining is confirmed, and stronger than stated: **no new durable state is needed to know N+1's base pre-confirmation — it is already in N's journal.**

### 1d. Confirmation detection

`blockConfirmationFiber` runs every `WAIT_BETWEEN_BLOCK_CONFIRMATION` = 10,000 ms default (`listen.ts:343-345`, `config.ts:237-239` — verified exactly as the parent plan cites). Each tick spawns `confirm-block-commitments.js` (`src/fibers/block-confirmation.ts:215-268`), which:

- Fetches the **entire committed state-queue UTxO set** from L1 and searches it for the pending journal's header hash, polling every 2 s for up to `BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS` = 12 s default (`src/workers/confirm-block-commitments.ts:30-77`; `config.ts:240-242`, retries `:243-245`).
- On match: `SuccessfulConfirmationOutput`; the parent action cross-checks header hash and all five roots against the journal (`block-confirmation.ts:333-387`), marks user events projected, transitions the journal (`observeConfirmedPendingBlock`, `:105-151`), and finally sets `AVAILABLE_CONFIRMED_BLOCK` to the serialized tip (`:453-460`) — which un-gates the next submit.
- On canonical advance past the pending header without including it, or validity-bound expiry + grace: `StaleUnconfirmedRecoveryOutput` (`confirm-block-commitments.ts:259-275` for expiry, `:308-314` for canonical-advance; recovery assembly `:120-140`).

### 1e. Today's L1-rollback handling (baseline the speculative design must not degrade)

`grep -riE "rollback"` over `demo/midgard-node/src/**/*.ts` matches **no files** — there is no explicit rollback module. Rollback of a submitted commit tx is handled _implicitly_ through the confirmation worker's canonical-tip reconciliation: a rolled-back (or mempool-evicted, or never-propagated) commit tx eventually manifests as either (a) canonical advance without the pending header, or (b) validity expiry — both yield `StaleUnconfirmedRecoveryOutput`. The parent action then abandons the journal, clears projected-header assignments on deposits/forced-txs/withdrawals, zeroes the unconfirmed-block globals, and resumes from the live tip (`block-confirmation.ts:468-511`; `abandonPendingBlockIfPresent` `:153-171`; unsubmitted variant `:173-196`). The tx payload is _not_ lost: mempool/processed-mempool rows are only cleared during local-finalization recovery against a **confirmed** block (`recoverLocalFinalizationAgainstConfirmedBlock`, `submission.ts:1098-1167` → `successfulLocalFinalizationRecoveryProgram`), so an abandoned block's txs remain in `mempool`/`processed_mempool` and are naturally re-selected for the next block. This retention-by-default property is the anchor for speculation safety in §2.2.

### 1f. Why cadence is what it is — the measured critical path

Putting 1a–1e together, the steady-state cycle for block N+1 after submitting block N is:

```
submit N ──[L1 confirmation latency: ~20–40 s]──▶ tx on chain
        ──[up to 10 s: confirmation fiber period]──▶ confirmation tick starts
        ──[≤12 s poll until match]──▶ AVAILABLE_CONFIRMED_BLOCK set
        ──[≤1 s: commit fiber period]──▶ worker tick
        ──[full rebuild: base hydrate O(ledger) + processMpfs O(backlog)]──▶
        ──[buildUnsignedCommitTx + journal + sign + submit: ~1–3 s]──▶ submit N+1
```

Two aggravations verified in code:

1. **The waiting-period work is throwaway.** During the gated interval, every 1 s commit tick runs the _entire_ pipeline — inline L1 barriers, full base hydration, full `processMpfs` — and then discovers `availableConfirmedBlock === ""` at `:879` and defers. Worse, `withMpfRootTransactions` **preserves** the MPF roots for `SkippedSubmissionOutput` (`shouldPreserveCommitMpfRoots`, `commit-block-header.ts:268-284`; transaction wrapper `mpf.ts:1986-2013`, invoked at `commit-block-header.ts:995-999`), so the next tick's `alignCommitMpfsToBase` sees root ≠ base and re-hydrates from scratch (`:237-255`). And the deferred pass's base is `confirmed_ledger` — the last _merged_ state — not N's post-state, so the roots it computes could never be submitted for N+1 anyway. The defer loop is pure heat.
2. **The final build happens after confirmation, serially.** Only once `AVAILABLE_CONFIRMED_BLOCK` points at N does `resolveCommitBaseLedgerEntries` materialize N's post-state from N's journal (`:184-210`) and rebuild — adding the full build time _on top of_ the confirmation wait.

Phase 3 shrinks the build terms; this phase moves them off the critical path entirely and shrinks the detection terms (10 s + 12 s + 1 s of pure scheduling latency around a ~20 s L1 event).

---

## 2. Architecture decisions

### 2.1 Speculative build with submit-on-confirm; 0-conf chaining deferred to a gated spike

**Decision: while block N is submitted-but-unconfirmed, build block N+1 completely — candidate snapshot, user-event roots, MPF deltas against N's post-state, transition commitments, chosen endTime — and hold it as a memory-resident "ready candidate." When N's confirmation is observed, immediately run only the tail of the submit path (`buildUnsignedCommitTx` → journal → sign → submit). Nothing is emitted to L1, and no durable row is written, before N is confirmed.**

Why this and not 0-conf chaining (submitting N+1 spending N's unconfirmed state-queue output): 0-conf collides with three verified invariants — (a) the single-active-journal constraint (`pendingBlockFinalizations.ts:707-719`) structurally forbids two unfinalized journals, and the entire confirmation/recovery machinery (§1d, §1e) assumes exactly one in-flight header; (b) `resolveLiveTailCommitBase`/`assertLiveTailCommitBase` define correctness as "my base is the _live L1_ tail" (`pending-journal.ts:115-177`) — chaining on an unconfirmed tail inverts that definition; (c) rollback blast radius doubles and today's implicit rollback handling (§1e) has no notion of cascading abandonment. These are protocol-adjacent design questions, not engineering tasks. §9 defines the spike; nothing in this plan's implementation depends on its outcome. The payoff calculus also favors this split: with Phase 3's <10 s build and ~20–40 s confirmation, speculative build alone lifts the Stage C ceiling to `50k / (confirm + submit)` ≈ 2,000–2,400 TPS at 25 s confirm and ≥2,500 TPS at ≤19 s — 0-conf is only _required_ if measured confirmation latency stays above ~19 s at 50k-tx blocks (see exit criterion sensitivity in §6.5).

**Rejected — "just block inside the worker on `awaitTx`":** turning the defer branch into an in-worker confirmation wait (like the drill path's `waitForTxConfirmation`) would hold the mutation lease and the pipeline phase across the entire confirmation window, starving merges (which share the lease, `merge.ts:340`) and removing none of the build time from the critical path.

### 2.2 Speculation state is memory-only; the journal records only confirmed-base facts

**Decision: the ready candidate for N+1 lives exclusively in process memory (Phase 3 overlay + a plain JS candidate record). The pending journal keeps exactly its current semantics: one active row, written at submit time, describing a block whose base is the live L1 tail. Crash-mid-speculation recovery = today's recovery, with speculation simply restarting from scratch.**

Verified compatibility: the journal is written inside `submitTxBackedCommit` at `submission.ts:876-923`, _after_ `resolveLiveTailCommitBase` — nothing before that point persists anything about the block being built. A crash while a candidate is in memory leaves the DB in exactly today's states: N's journal in `submitted_local_finalization_pending` (or earlier), mempool/processed rows intact (§1e). Startup recovery (`canonical-journal-recovery` via the confirmation action's revive path, `block-confirmation.ts:198-213, 407-449`) is untouched. The candidate is rebuilt after restart at the cost of one build (<10 s post-Phase 3) — an acceptable crash penalty.

**Rejected — persisting speculative candidates (a second journal row in a new `speculative` status):** would break the single-active-journal invariant that four separate code paths assert (`:707-719`; confirmation invariant checks `block-confirmation.ts:303-328`; commit-base journal match `commit-block-header.ts:184-210`; lease inspection `stateQueueMutationLeases.ts:178-183`), forcing schema and recovery changes for a benefit (saving one rebuild after a crash) that is negligible once Phase 3 lands. Persisting speculation is a prerequisite for 0-conf chaining, not for this phase — it moves to §9.

**Rejected — journaling the candidate's MPF overlay to LevelDB:** the ledger MPF store is already shared, root-transactional state (`withMpfRootTransactions`, `mpf.ts:1986-2013`); adding a second uncommitted root lineage to it reintroduces exactly the root-mismatch/full-rehydration churn of §1f. Phase 3's overlay gives us cheap fork/discard in memory; use it.

### 2.3 N+1 builds against N's post-state from N's own journal — the Phase 3 overlay dependency made explicit

**Decision: the builder derives N+1's base as (base root = N's header `utxosRoot`, base entries = N's journal UTxO members), i.e., the same data `resolveCommitBaseLedgerEntries` uses today after confirmation (`commit-block-header.ts:184-210`) — just consumed at submit time instead of confirmation time. With Phase 3, the in-memory working trie _already sits at exactly this root_ when N's build finishes (the block's `utxoPayloadEntries` are what got journaled, `submission.ts:921`), so in the common case the builder forks the live overlay rather than re-materializing entries.**

The dependency, stated as an interface requirement rather than an assumption (§8.1): Phase 3 must guarantee that after a successful `SubmittedAwaitingConfirmationOutput`, the retained working-trie state equals N's post-state root, and must expose `fork()` of that state. Fallback when the guarantee doesn't hold (fresh process, root mismatch): materialize from N's journal via `materializeConfirmedLedgerSnapshot` and verify the root against N's expected `utxosRoot` exactly as the existing code does (`:192-199`) — correctness never depends on the fast path.

**Note on the parent plan's premise "the node owns the state-queue tail UTxO":** verified _conditionally_. The node deterministically knows N's output (it built and signed the tx; the new tail outref is `submittedTxHash#idx`), but Midgard is multi-operator — the scheduler rotates active operators (`resolveCurrentOperatorSchedulerWindow`, consumed at `commit-block-header.ts:617-664`), and another operator's commit legitimately moves the tip, which today surfaces as the "Commit base is stale" abort (`pending-journal.ts:127-135, 170-176`). Ownership holds only inside this node's active scheduler window. Speculation must therefore treat "confirmed tip ≠ my pending header" as a first-class invalidation trigger (§3.4), not an error.

### 2.4 Pipeline depth 1 — exactly one ready candidate, never more

**Decision: the builder/submitter queue is a bounded buffer of depth 1 (a `Ref<Option<ReadyCandidate>>`). The builder does not start N+2 until N+1 is submitted.**

Justification: (a) **bounded speculation risk** — at most one block's build work is ever discarded on invalidation; (b) **no compounding rollback** — every candidate's base is a block this node has _submitted_, never a block that is itself speculative, so invalidation never cascades; (c) **no additional memory pressure** — one overlay of one block delta (Phase 3 sizes this); (d) **it is sufficient** — the pipeline has exactly two serial resources (CPU build, L1 confirmation), so depth 1 achieves the theoretical cadence `max(build, confirm) + submit`; depth ≥2 only helps if build > confirm, in which case building faster (Phase 3) is the fix, not deeper speculation.

**Rejected — depth ≥2 (speculating N+2 on unconfirmed-N+1-on-unconfirmed-N):** N+2's base would be a _speculative_ state; an invalidation of N discards two blocks of work and requires cascade-aware bookkeeping; and it yields zero cadence improvement under submit-on-confirm because submission is still serialized on confirmations. It is strictly the 0-conf problem in memory-only clothing — rejected for the same reasons, without the payoff.

### 2.5 Builder needs no lease; submitter keeps today's lease discipline unchanged

**Decision: the builder stage acquires neither the DB lease nor the `mutation_worker` phase — it is read-only against Postgres and touches only its private overlay. The submitter stage runs under exactly today's protections: `mutation_worker` phase → `tryWithLease("block_commitment")` → lease revalidation at journal-write and at sign/submit (`submission.ts:833, 926-931`).**

This preserves the lease's meaning verbatim: _the lease serializes durable state-queue mutations (journal writes + L1 submissions) across processes and against merges._ Two nodes may both speculate — harmlessly, since speculation is memory-only — but only one can journal-and-submit, exactly as today. A new `speculative_build` value is added to `CommitPipelinePhase` (`globals.ts:6`) so the in-process phase machine can distinguish "builder busy" from "idle" without blocking submission: **the submitter may preempt the builder** (interrupt the build fiber, submit the previous ready candidate) but never vice versa. The one lease-adjacent change: the candidate carries no lease token at build time; `buildPendingJournalMetadata`'s token requirement (`pending-journal.ts:97-113`) is satisfied by the _submitter's_ freshly acquired token, which is the semantically correct holder (the journal row records who performed the durable mutation, not who did arithmetic in RAM).

**Rejected — holding the lease across build+wait+submit ("long-lease pipelining"):** blocks merges for the entire confirmation window (shared scope, `merge.ts:340`), turns lease TTL into a cadence-coupled tuning hazard, and adds nothing — speculation needs no exclusivity.

### 2.6 Ingestion barrier off the critical path, with an explicit freshness bound

**Decision: replace the inline commit-time barrier fetches with a background _barrier refresher_ that continuously runs the same three full reconciliations (deposits, withdrawals, tx-orders) and publishes a monotone watermark triple `(depositBarrier, withdrawalBarrier, txOrderBarrier)` in `Globals`. The builder reads the latest watermarks instead of fetching inline; the submitter re-checks cheaply at submit time.**

What the inline barrier actually guarantees today (verified): each call locks an `inclusionTimeUpperBound = now` and performs a full visible-set reconciliation into the DB (`user-event-ingestion.ts:71-105`; full-set rationale documented at `fetch-and-insert-deposit-utxos.ts:166-171` — repeated full reconciliation tolerates indexer visibility lag). The block's user-event end time is then the earliest barrier timestamp (`commit-block-header.ts:609-615`), and `processMpfs` uses the barrier times as visibility bounds (`:798-806`); finally `assertCommitInputsWithinBlockEndTime` refuses any included entry with `inclusion_time > blockEndTimeMs` (`submission.ts:93-166, 869-875`). The correctness property is therefore: **the deposits/withdrawals/forced-tx roots must be complete for all events with `inclusion_time ≤ blockEndTime`**, and the mechanism is "reconcile fully up to T, then never let the block's end time exceed T." Deposits carry an on-chain inclusion deadline (they must appear in a block within the protocol's event-wait window), so the barrier cannot simply be dropped — but nothing requires it to run _inside_ the commit worker; it only requires `blockEndTime ≤ latest completed reconciliation upper bound`.

The refresher preserves this exactly: it is the existing fiber trio (`fetchAndInsertDepositUTxOsFiber` etc., already scheduled at `listen.ts:346-354` on `WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES` = 10 s default, `config.ts:405-407`) upgraded to run the _barrier-locked_ variant on a faster interval (`USER_EVENT_BARRIER_REFRESH_MS`, default 2,000 ms) and to publish watermarks (extending the existing `LATEST_DEPOSIT_FETCH_TIME` ref, `globals.ts:66`). The builder sets `userEventOnlyEndTime = min(watermarks)` — structurally identical to today's `:609-615`, sourced from the refresher. **Freshness bound:** block end times lag real time by at most `USER_EVENT_BARRIER_REFRESH_MS + reconciliation duration`; this bound must be kept well under the protocol deposit-inclusion deadline (an on-chain parameter — record the deployed value in config validation as a startup assertion, item 7 in §3). **Late-visibility guard:** since an indexer can surface a deposit with `inclusion_time ≤ watermark` _after_ the watermark was taken (same exposure exists today between barrier and submit), the submitter re-runs the cheap `pendingUserEventCountUpTo(candidate.endTime)` count (`commit-block-header.ts:109-129`) and compares against the candidate's included counts — mismatch invalidates the candidate (§3.4) instead of submitting a provably incomplete root.

### 2.7 Cadence control: make confirmation detection event-shaped, then let the loop self-pace

**Decision: with pipelining, `WAIT_BETWEEN_BLOCK_CONFIRMATION` stops being a cadence _floor_ and becomes pure detection latency — so shrink it and sharpen it. (a) Default `WAIT_BETWEEN_BLOCK_CONFIRMATION` drops 10,000 → 2,000 ms; the worker's inner 2 s poll (`confirm-block-commitments.ts:39-67`) already makes finer outer periods cheap-ish, but (b) when `UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH` is set, the confirmation worker should first do a targeted `awaitTx(submittedTxHash)`-style probe before the full state-queue set fetch, since the full fetch is O(queue length) against the provider. (c) On confirmation success, the confirmation action directly signals the submitter (a `Deferred`/queue offer) instead of waiting for the next 1 s commit-fiber tick to notice `AVAILABLE_CONFIRMED_BLOCK`.**

Resulting steady-state cadence: `L1 confirmation latency + detection (≤~2 s) + submit tail (~1–3 s)` — with build fully overlapped whenever build ≤ confirm (guaranteed by Phase 3's exit criterion at 50k txs). `WAIT_BETWEEN_BLOCK_COMMITMENT` keeps its 1 s default but its tick becomes a cheap no-op whenever a candidate is ready or being built (no more per-tick full rebuilds — §1f aggravation 1 is deleted, which also removes ~30 s/window of wasted CPU and LevelDB churn).

---

## 3. Implementation items

Ordered; each lands independently behind the `SPECULATIVE_COMMIT_BUILD` flag
(§4). The flag remains strict `false` by default. Gated live, process, and
one-hour validation runs opt in with `SPECULATIVE_COMMIT_BUILD=true`; the
production default must not change until every one of those gates passes and
its evidence is reviewed.

### 3.1 Refactor: split `databaseOperationsProgram` into plan/build vs. submit stages

`src/workers/commit-block-header.ts`: extract from `databaseOperationsProgram` (`:521-972`) a pure-ish **`planAndBuildBlockCandidate`** covering candidate snapshot → scheduler fit → budgets → base alignment → `processMpfs` → event roots (the code currently spanning `:535-877` minus the inline barriers), returning a serializable-in-memory:

```ts
type ReadyCandidate = {
  baseTail: { outRef: string; headerHash: string; datumCbor: string }; // = N as submitted
  baseUtxosRoot: string;                    // N's header utxosRoot
  endTime: Date; blockEndTimeCapMs?: number;
  roots: { utxoRoot; txRoot; depositsRoot; withdrawalsRoot; forcedTransactionsRoot;
           transitionTraceRoot; eventToStepRoot };
  transitionCommitments: …;                 // makeEventCommitments output
  payload: { processedMempoolTxs; mempoolTxHashes; sourceTable;
             includedDeposit/Forced/Withdrawal entries + eventIds;
             transitionTraceMembers; eventToStepMembers; utxoPayloadEntries };
  overlayHandle: LedgerOverlayHandle;       // Phase 3 (§8.1)
  builtAtMs: number; barrierWatermarks: { deposit; withdrawal; txOrder };
  invalidationKey: string;                  // baseTail.headerHash + endTime + watermark digest
};
```

and a **`submitReadyCandidate`** that is today's `submitTxBackedCommit`/`submitDepositOnlyCommit` tail unchanged (`submission.ts:642-1061`), taking the candidate instead of recomputing. The existing single-shot path (flag off) composes the two back-to-back — behavior-identical, and the refactor is testable against `runCommitBlockHeaderWorkerProgram`'s existing emulator harness (`commit-block-header.ts:974-1009`; used by `tests/deposit-flow-emulator.test.ts`).

### 3.2 Builder stage

New module `src/fibers/speculative-commit-builder.ts`:

- Trigger: after any `SubmittedAwaitingConfirmationOutput`/`SubmittedAwaitingLocalFinalizationOutput` is applied by the commit fiber (`block-commitment.ts:674-717`), and on candidate invalidation, if `SPECULATIVE_CANDIDATE` (new `Ref<Option<ReadyCandidate>>` in `Globals`) is `None` and `UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ≠ ""`.
- Acquires the new `speculative_build` pipeline phase (extend `CommitPipelinePhase`, `globals.ts:6`; acquire/release parallel to `block-commitment.ts:463-507`). Does **not** touch `StateQueueMutationLeasesDB`.
- Base resolution: `PendingBlockFinalizationsDB.retrieveActive()` (`pendingBlockFinalizations.ts:527`) → N's journal; base tail = journal's _own_ block (header hash + header CBOR + `submittedTxHash#0` outref reconstructed from `SUBMITTED_TX_HASH`); base entries via Phase 3 overlay fork, fallback `materializeConfirmedLedgerSnapshot` + root check (mirror `commit-block-header.ts:188-199`).
- Candidate snapshot via Phase 1 keyset retrieve; user-event visibility from barrier watermarks (§3.5); then `planAndBuildBlockCandidate`; publish to `SPECULATIVE_CANDIDATE`.
- Runs in a worker thread like today's commit worker (same `resolveWorkerEntry` pattern, `block-commitment.ts:568-588`) so a 50k-tx build never blocks the main loop; the overlay handle crossing the thread boundary is a Phase 3 interface requirement (§8.1 — either overlays are worker-owned with the candidate carrying only the delta, or Phase 3 provides a transferable representation).

### 3.3 Submitter stage + confirmation-triggered wake

- `block-confirmation.ts` success path (`:453-466`): after setting `AVAILABLE_CONFIRMED_BLOCK`, offer a wake signal to the submitter (new `Deferred`/`Queue` in `Globals`) carrying the confirmed tip's header hash.
- Submitter (inside the existing commit fiber action, before the idle short-circuits at `block-commitment.ts:777-789`): if a `ReadyCandidate` exists and the confirmed tip header hash equals `candidate.baseTail.headerHash`, acquire `mutation_worker` phase + lease (`:790-810` unchanged) and run `submitReadyCandidate` in the worker thread. Pre-submit revalidations retained verbatim: lease revalidation, `resolveLiveTailCommitBase`, `assertLiveTailCommitBase`, `assertCommitInputsWithinBlockEndTime`, `assertPendingJournalCompleteness`, plus the new user-event recount (§2.6). Scheduler-window fit is re-resolved at submit time (`resolveCurrentOperatorSchedulerWindow` + `resolveCommitEndTimeFit`, today at `commit-block-header.ts:622-652`) because the window that was current at build time may have rotated — a fit failure is an invalidation, not an error.
- If the tip hash does not match the candidate base: durably record the `(replaced base, foreign tip)` reconciliation context before invalidating (§3.4), including canonical header CBOR, the exact `(start,end]` window, category roots/counts, and any subsequently verified canonical DA. Every later non-speculative build rescans both awaiting and resolved evidence windows, so an indexer-late event can still be classified after restart, after the live tip advances, or after the ordinary DA row is pruned. Only events proven absent by an empty foreign event root or by retained canonically decoded DA whose header binding, category roots, counts, and membership all verify become replayable. Missing/invalid DA yields `AwaitingForeignDaOutput`; a foreign-present candidate event reopens the marker and defers until foreign-finalization semantics are available. Awaiting markers fail readiness and are exposed as a pipeline gauge. No general overdue-event allowance is introduced.

### 3.4 Invalidation

Single choke point `invalidateSpeculativeCandidate(reason)`: swap `SPECULATIVE_CANDIDATE` to `None`, `overlayHandle.discard()`, bump `speculation_invalidations_total{reason}`, wake the builder. Triggers:

| #   | Trigger                                                                                                           | Detected at / by                                                                            | Verified hook                                                                                     |
| --- | ----------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| T1  | N abandoned: canonical queue advanced past N, or validity expiry + grace                                          | Confirmation action `StaleUnconfirmedRecoveryOutput` handling                               | `block-confirmation.ts:468-511`; worker decisions `confirm-block-commitments.ts:259-275, 308-314` |
| T2  | Confirmed tip header ≠ candidate base (another operator committed; L1 reorg replaced N with a different header)   | Submitter tip-hash check (§3.3); also `resolveLiveTailCommitBase` "stale" abort as backstop | `pending-journal.ts:138-177`                                                                      |
| T3  | User-event recount mismatch (late-visible deposit/withdrawal/forced-tx with `inclusion_time ≤ candidate.endTime`) | Submitter pre-submit recount                                                                | `commit-block-header.ts:109-129`; `submission.ts:93-166`                                          |
| T4  | Scheduler-window fit failure at submit time                                                                       | Submitter re-fit                                                                            | `commit-block-header.ts:622-652`                                                                  |
| T5  | `RESET_IN_PROGRESS`                                                                                               | Both stages' entry guards                                                                   | `block-commitment.ts:776-777`, `block-confirmation.ts:280-283`                                    |
| T6  | Confirmation timeout beyond `UNCONFIRMED_BLOCK_MAX_AGE_MS` escalating to T1's expiry recovery                     | Existing confirmation worker paths (no new mechanism)                                       | `config.ts:246-248`; `confirm-block-commitments.ts:276-280`                                       |
| T7  | Crash / restart                                                                                                   | Nothing persisted; candidate simply doesn't exist after restart                             | §2.2                                                                                              |

On every invalidation the payload is safe by construction: candidate txs are still rows in `mempool`/`processed_mempool` (cleared only by local finalization against a confirmed block, §1e). Candidate user events remain durable rows. For T2 only, the durable mismatch marker retains the authenticated header window and reusable verified evidence; resolved history is rescanned rather than discarded. Events in `(header.startTime, header.endTime]` are changed from `Awaiting` to replayable `Projected` only after verified absence evidence, transactionally with deposit-ledger reconciliation. Events at/before the foreign start remain strict overdue failures, and foreign-present events are never blindly replayed. Rebuild is bounded by `SPECULATIVE_REBUILD_MAX_ATTEMPTS` per base; past it, the node degrades to the exact current non-speculative behavior (flag-off path) until the next confirmed block — guaranteeing the plan can never be _worse_ than today under invalidation storms.

### 3.5 Barrier refresher

`src/fibers/user-event-ingestion.ts` + the three fetch fibers: add `runBarrierRefresherPass` = today's `fetchAndInsert*ForCommitBarrier` chain (`commit-block-header.ts:599-608`) executed on its own fiber every `USER_EVENT_BARRIER_REFRESH_MS`, publishing watermarks to new `Globals` refs (pattern: `LATEST_DEPOSIT_FETCH_TIME`, `globals.ts:66`). `planAndBuildBlockCandidate` consumes watermarks; the inline barrier calls are removed from the worker (flag-on) or retained (flag-off). Startup assertion: `USER_EVENT_BARRIER_REFRESH_MS + p99 reconcile duration ≪` protocol deposit-inclusion deadline (deadline value sourced from deployed hub-oracle params; wire into `config.ts` validation).

### 3.6 Delete the defer-loop churn

With the flag on, the commit-fiber tick while unconfirmed-N exists reduces to: heartbeat + builder-trigger check. The defer branch (`commit-block-header.ts:879-900`) remains as the flag-off path and as the fallback when speculation is degraded (§3.4). `ProcessedMempoolDB`'s role (durable holding pen for selected-but-unsubmitted payloads) is unchanged — the candidate's `sourceTable` bookkeeping keeps using it so failure paths (`preserveTxPayloadForRetryAfterSubmitFailure`, `submission.ts:781-821`) work verbatim.

### 3.7 State machine (normative)

```
                 ┌──────────────────────────────────────────────────────┐
                 ▼                                                      │
  ┌──────┐ submit N ok  ┌──────────────────────┐ build done ┌────────────────────┐
  │ Idle │─────────────▶│ Building(N+1|base=N) │───────────▶│ ReadyToSubmit(N+1) │
  └──────┘  (start bld) └──────────────────────┘            └────────────────────┘
     ▲                        │        ▲                          │
     │                        │ T1–T7  │ rebuild (≤max attempts)  │ N confirmed
     │                        ▼        │                          ▼ (wake)
     │                  ┌─────────────┐│                    ┌────────────┐
     │◀── degraded ─────│ Invalidated │┘                    │ Submitting │──▶ submit ok:
     │  (fall back to   └─────────────┘                     └────────────┘   N+1 becomes N;
     │   today's path)        ▲                                   │          → Building(N+2)
     │                        └────── T2/T3/T4 at submit ─────────┘
     │
  AwaitingConfirm(N) is not a distinct machine state: it is the invariant
  "UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ≠ ''" that holds throughout
  Building/ReadyToSubmit — the confirmation fiber owns it (globals.ts:61-64).
```

### 3.8 Files touched (summary)

`workers/commit-block-header.ts` (split, §3.1) · `workers/commit-block-header/submission.ts` (accept candidate) · `fibers/block-commitment.ts` (submitter wiring, phase enum use) · `fibers/block-confirmation.ts` (wake signal, T1 hook) · new `fibers/speculative-commit-builder.ts` · `fibers/user-event-ingestion.ts` + 3 fetch fibers (refresher) · `services/globals.ts` (`SPECULATIVE_CANDIDATE`, watermarks, wake queue, phase enum) · `services/config.ts` (§4) · `commands/listen.ts` (fiber registration) · Phase 3's overlay module (consume API, §8.1).

---

## 4. Config surface

| Var                                                                                 | Default                         | Meaning                                                                                                                     |
| ----------------------------------------------------------------------------------- | ------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `SPECULATIVE_COMMIT_BUILD`                                                          | `false`                         | Strict default; `false` restores today's exact behavior (defer loop included).                                              |
| `SPECULATIVE_REBUILD_MAX_ATTEMPTS`                                                  | `3`                             | Invalidation-driven rebuilds per base before degrading to non-speculative for that window.                                  |
| `USER_EVENT_BARRIER_REFRESH_MS`                                                     | `2000`                          | Barrier refresher period; validated against the deposit-inclusion deadline at startup (§3.5).                               |
| `USER_EVENT_BARRIER_MAX_STALENESS_MS`                                               | `15000`                         | Builder refuses to pick an endTime if watermarks are older than this (refresher wedged ⇒ degrade, don't build stale roots). |
| `WAIT_BETWEEN_BLOCK_CONFIRMATION`                                                   | `10000` → **`2000`**            | Now pure detection latency (§2.7). Existing var, new default; release notes required.                                       |
| `WAIT_BETWEEN_BLOCK_COMMITMENT`                                                     | `1000` (unchanged)              | Tick becomes cheap when building/ready.                                                                                     |
| `BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS` / `_RETRIES` / `UNCONFIRMED_BLOCK_MAX_AGE_MS` | unchanged (`config.ts:240-248`) | Interact with T1/T6 only.                                                                                                   |

Lease TTL/renew (`STATE_QUEUE_MUTATION_LEASE_TTL_MS`, `_RENEW_INTERVAL_MS`, `block-commitment.ts:806-809`) are intentionally untouched: the lease is now held only for the (short) submit tail, so if anything these could later shrink — out of scope.

This matches the current strict source default (`config.ts:357-360` and
`.env.example:51`; operator guidance at `README.md:255-257`). `true` is an
explicit opt-in only for gated live, process, and one-hour validation. The
default must remain `false` until all three gate classes pass and their evidence
is reviewed.

---

## 5. Observability

New metrics (naming per existing `Metric.*` conventions in `block-commitment.ts:59-90`):

- `commit_cadence_ms` (histogram) — time between consecutive `markSubmitted` events. **The exit-criterion metric**; target p50 ≤ confirm-latency + 5 s.
- `speculative_build_duration_ms` (timer) — builder stage wall time (compare against Phase 3's `commit_worker_duration`, `block-commitment.ts:87-90`).
- `speculation_hit_total` / `speculation_invalidations_total{reason=T1..T7}` (counters) — hit = candidate submitted as built. Target steady-state hit rate ≥95%; T3 (late user events) is the expected dominant residual.
- `speculation_overlap_efficiency` (gauge, 0–1) — `min(build_ms, confirm_wait_ms) / build_ms` per cycle (zero-duration build = 1), where confirm_wait = submit-N→confirmation-observed. Target ≥0.9 (i.e., at least 90% of build work is hidden). If this is high but cadence misses target, detection/submit tail is the culprit — see next two.
- `confirmation_detection_lag_ms` — tx-on-chain time (from provider block metadata) → `AVAILABLE_CONFIRMED_BLOCK` set. Target ≤ `WAIT_BETWEEN_BLOCK_CONFIRMATION` + poll interval.
- `submit_after_confirm_ms` — wake → `markSubmitted`. Target ≤3 s (this is the `buildUnsignedCommitTx` + journal + sign + submit tail).
- `user_event_barrier_staleness_ms` (gauge) — now − min(watermarks). Alert at `USER_EVENT_BARRIER_MAX_STALENESS_MS`.
- `pipeline_trace` log line extensions: `phase=candidate_build_started/candidate_ready/candidate_invalidated/candidate_submitted` with `invalidation_key`, matching the existing `pipeline_trace phase=…` convention (`commit-block-header.ts:532-534, 694-696, 815-817`).

---

## 6. Test & verification plan

### 6.1 Unit

- State machine (§3.7) as a pure reducer: every (state, event) pair, including double-invalidation, wake-without-candidate, candidate-ready-after-tip-moved. New file `tests/speculative-commit-planner.test.ts` alongside the existing planner tests (`tests/commit-block-planner.test.ts`).
- Invalidation-decision function: table-driven over T1–T7 inputs.
- `planAndBuildBlockCandidate` ≡ legacy path: property test asserting the refactor (§3.1) with flag off produces byte-identical roots/journal input for fixture blocks (fixtures exist under `tests/fixtures`).

### 6.2 Emulator integration (fast, deterministic)

The commit worker core is already exercised in-process against the Lucid emulator without worker-thread bootstrap (`runCommitBlockHeaderWorkerProgram` exported for exactly this, `commit-block-header.ts:974-1009`; pattern in `tests/deposit-flow-emulator.test.ts`). Likewise the confirmation action takes an **injectable worker runner** (`buildBlockConfirmationAction(runWorker)`, `block-confirmation.ts:270-276`) — this is the forced-rollback lever:

- **Speculation happy path:** submit N on the emulator; run builder; assert `ReadyCandidate.baseTail.headerHash` = N's header hash and roots match a from-scratch build on N's post-state; advance emulator, feed real confirmation; assert submit tail runs without re-hydration and cadence = confirm + submit.
- **Forced rollback (T1):** submit N; build candidate; then feed a synthetic `StaleUnconfirmedRecoveryOutput` through the injectable runner (simulating L1 rollback/eviction). The synthetic action-level test asserts journal abandonment, projected-assignment clearing (existing behavior, `block-confirmation.ts:153-171`), candidate discard with `reason=T1`, mempool/processed retention, recovered-tip handoff in globals, and byte-for-byte DB/global equivalence with the flag-off path. It must not attempt a real submit on the pre-N tip because the Lucid emulator chain itself was not rolled back. A matched-snapshot local-devnet test performs the real next-build-on-recovered-tip assertion.
- **Foreign commit (T2):** commit a block from a second emulator wallet between N's confirmation and submit; assert the production tip-decision seam records the mismatch before returning `InvalidateSpeculativeCandidate(T2)`, neither parked MPF resumes, cleanup is exact-once, and no candidate journal/DA/header submit occurs. An empty foreign event root proves the retained candidate events absent; the durable catch-up transition makes them replayable and rebuilds on the foreign tip. Pure/DB tests cover present, absent, missing, invalid DA; exact `(start,end]` boundaries; restart and later-tip rescans of resolved evidence; retained non-empty DA after ordinary payload pruning; readiness while evidence is unresolved; and strict rejection of unrelated older `Awaiting` rows.
- **Late deposit (T3):** insert a deposit with `inclusion_time < candidate.endTime` after candidate-ready; assert recount mismatch → invalidate; assert the rebuilt block includes it (this is the freshness-bound property from §2.6 made executable).

### 6.3 Crash-mid-speculation recovery

Via the e2e process harness (`src/e2e/runner.ts`, `service-supervisor.ts`, `logged-child-process.ts`): kill the node process (a) mid-build, (b) candidate-ready-but-unconfirmed, (c) between confirmation-wake and journal write. Assert after restart: no journal row beyond N's (single-active invariant holds), confirmation/recovery proceeds exactly as an unmodified node (compare against a flag-off control run), and a fresh candidate is built. (c) is the sharpest case: the wake was consumed but nothing durable happened — restart must re-detect N's confirmation through the normal first-run/no-pending snapshot path (`confirm-block-commitments.ts:142-178`).

### 6.4 Lease contention

Two node processes against one Postgres + one emulator/devnet: both speculate; assert exactly one `preparePendingSubmission` succeeds per block (the loser sees `Busy`, `block-commitment.ts:811-817`, or the single-active-journal refusal `pendingBlockFinalizations.ts:707-719`), and the loser's candidate invalidates via T2 on the next confirmed tip. Also: kill the winner between journal write and submit; assert lease expiry + the unsubmitted-recovery path (`confirm-block-commitments.ts:185-248`) unblocks the survivor.

### 6.5 Cadence verification (exit criterion)

Extend the Phase 0 soak benchmark: at 50k-tx blocks on production-shaped infra, record `commit_cadence_ms`, `speculation_overlap_efficiency`, `speculation_hit_total` rate over ≥1 h. Pass: p50 cadence ≤ measured L1 confirmation latency + 5 s; overlap efficiency ≥0.9; hit rate ≥95%; and Stage C throughput = blockTxCount / cadence ≥2,500 TPS **or** a documented finding that confirmation latency > ~19 s is the binding term (which is the §9 spike's entry evidence, not this plan's failure).

The fail-closed operator command, complete artifact/fingerprint requirements,
and report verifier are maintained in
`docs/benchmark-scenarios/phase-4-pipelined-one-hour.md`.

---

## 7. Risks & rollback

1. **Candidate/base divergence bug class** (wrong roots submitted): mitigated by keeping every existing pre-submit assertion in the submit tail (§3.3) — the L1 validator and the journal-root cross-checks at confirmation (`block-confirmation.ts:362-387`) are unchanged backstops; a divergent candidate produces a rejected/abandoned block, never corrupted local state, and T1/T2 recovery already handles that outcome.
2. **Invalidation storms** (busy multi-operator windows, flaky provider): bounded by `SPECULATIVE_REBUILD_MAX_ATTEMPTS` + degradation to the flag-off path (§3.4) — worst case equals today's performance.
3. **Phase 3 overlay API slippage** (no transferable fork across worker threads): fallback is journal-materialization per build (§2.3), costing one O(block-payload) rebuild per candidate — still off the critical path, but memory/CPU heavier; flagged as the top cross-plan coordination risk (§8.1).
4. **Faster confirmation polling load**: 2 s outer period × full state-queue fetch is O(queue length) per poll against the provider; the targeted-probe optimization (§2.7b) must land with the default change, or providers with rate limits will throttle. Rollback: env-revert `WAIT_BETWEEN_BLOCK_CONFIRMATION=10000`.
5. **Barrier refresher wedge** (provider outage): `USER_EVENT_BARRIER_MAX_STALENESS_MS` halts _building_ (not the node); deposits stall exactly as they would today if the inline barrier fetch failed — no new failure mode, but now it is visible as a gauge.
6. **Operational rollback:** `SPECULATIVE_COMMIT_BUILD=false` restores the current code path in full (defer loop included); the refactor (§3.1) is the only non-flaggable change and is covered by the equivalence property test (§6.1).

---

## 8. Interface contracts

### 8.1 Phase 3 (in-memory MPF overlay) — required API

```ts
interface LedgerOverlayHandle {
  rootHex(): Effect<string>;
  fork(): Effect<LedgerOverlayHandle>; // O(1)/COW snapshot at current root
  applyBlockDelta(ops): Effect<string>; // returns new root; overlay-local
  promote(): Effect<void>; // overlay becomes the durable working state
  discard(): Effect<void>; // drop overlay, free memory
}
```

Guarantees Phase 3 must provide: (G1) after `SubmittedAwaitingConfirmationOutput`, the retained working state's root equals the submitted block's `utxosRoot` (this is implied by today's `shouldPreserveCommitMpfRoots` semantics, `commit-block-header.ts:268-284`, but must survive Phase 3's rewrite as an explicit invariant); (G2) `fork()` is cheap enough to run per candidate (§2.4 caps live overlays at 1); (G3) either overlays are usable inside a worker thread that also runs the build, or a transferable delta representation exists (§3.2). This plan's budget assumption: 50k-tx candidate build <10 s = Phase 3's exit criterion.

### 8.2 Phase 1 (keyset retrieve)

Builder consumes the keyset-paginated ascending `MempoolDB.retrieve` replacement (parent plan Phase 1 item 5) with an explicit snapshot bound (`time_stamp_tz ≤ builder start`) so rebuilds after invalidation are incremental, and block `endTime` is the maximum `time_stamp_tz` of every included candidate (the last candidate under the required ascending order). This preserves Phase 1's binding C4 rule when speculative builds re-slice a candidate set.

### 8.3 Merge path (D3) — interface note only

Multi-block/batched merges are a separate assessment (dependent on unverified Aiken linked-list validator rules — parent plan §4). The only contract this plan imposes: the submitter's lease usage stays scope-compatible with `mergeFiber`'s (`merge.ts:340`), and speculation never holds the lease (§2.5) — so a future merge-batching plan inherits _more_ lease availability than today, not less. `shouldSkipIdleCommitBehindUnmergedTail` (`commit-block-header.ts:736-752`) is preserved in the builder's skip checks so speculation does not race an over-long unmerged queue.

## 9. Gated extension: 0-conf commit chaining (design spike)

Not spec'd for implementation. A time-boxed (≤1 week) design spike, to be scheduled only if §6.5 measures L1 confirmation latency as the binding term (>~19 s sustained at 50k-tx blocks).

**Spike questions:**

1. **Rollback blast radius.** If N+1 is submitted spending N's unconfirmed output and N rolls back: enumerate every recovery path in §1d/§1e that assumes ≤1 in-flight header; design cascading abandonment (journal chain rewind, user-event unprojection ordering, `BLOCKS_IN_QUEUE` accounting `block-confirmation.ts:502`). What is the maximum safe chain depth (proposed: 2, i.e., exactly one unconfirmed parent)?
2. **Journal semantics under chained unconfirmed txs.** The single-active-journal invariant (`pendingBlockFinalizations.ts:707-719`) must become a _chain_ of journals with parent pointers. What migrations, and how do `retrieveActive`, lease inspection (`stateQueueMutationLeases.ts:178-183`), and canonical-journal recovery generalize?
3. **L1 mempool behavior.** Do the deployed submit providers (Blockfrost / Kupmios / local node — `commands/l1-provider-preflight.ts`) accept chained-unconfirmed submissions? What are eviction/TTL semantics for the child when the parent is delayed, and what resubmission policy follows?
4. **Validity intervals and operator scheduling.** N+1's tx validity window and scheduler-window checks (`commit-block-header.ts:617-664`) are evaluated against chain state that doesn't yet include N — can a chained tx even be built to satisfy the CommitBlockHeader validator's expectations of the tail datum it spends, and does operator rotation mid-chain invalidate the child?
5. **Watcher/fault-proof exposure.** Two unconfirmed headers visible in the L1 mempool: any implications for watchers, DA attestation timing, and the merge-readiness maturity buffer?

**Exit criteria for the spike:** (a) written answers to 1–5 with file:line/protocol citations; (b) a rollback-cascade recovery design reviewed by the protocol team; (c) an emulator/devnet prototype that submits a 2-chain, rolls back the parent, and recovers to a clean single-tip state with zero payload loss; (d) measured cadence delta vs. this plan's speculative-only baseline.

**Evidence that would unlock implementation:** all exit criteria met, **and** §6.5 data showing ≥20% additional Stage C headroom from chaining at the deployment's real confirmation latency, **and** provider support confirmed for the production submit path. Absent any of the three, speculative build + submit-on-confirm (this plan) remains the terminal design for Phase 4.

## 10. Implementation and verification record (2026-07-14)

The speculative pipeline, invalidation/recovery paths, process harness, matched
snapshot tooling, offline report verifiers, and one-hour fail-closed runner are
implemented. The local-devnet bootstrap is explicit rather than startup
implicit: it is authorized only by the dedicated Phase 4 token, requires the
run-scoped Custom-network/Kupmios/Postgres identity, rejects protected database
ports, pins zero L2 fees for the isolated process gate, disables checkout
dotenv loading, and seeds the exact complete configured `GENESIS_UTXOS` set.
It mutates only an empty ledger or accepts a complete byte-identical existing
set; partial, extra, or mismatched state fails before acceptance proceeds.

Final-tree local evidence under Node 22.22.2:

- Five focused Vitest files passed 65/65, including the real two-process
  SIGKILL/lease-contention harness (run outside the restricted process sandbox).
- The Phase 4 report suite passed 9/9 under Vitest.
- The shell/assets, custom-chain, and process-summary verifier suites passed
  34/34 under `node --test`.
- Focused type checking and shell syntax checks passed after the explicit
  genesis, fee, port, and dotenv boundaries landed.

These results establish local correctness only. The earlier matched snapshot
was captured before explicit L2 genesis seeding and is not acceptance evidence
for this final tree. Closeout still requires a final immutable build, a fresh
seeded snapshot capture, the full process acceptance run, its offline verifier,
and the one-hour cadence gate. `SPECULATIVE_COMMIT_BUILD` therefore remains
`false` by default.

An independent final-tree safety rereview found zero P1/P2 implementation
defects in the default-off pipeline, depth-one state, barrier checks,
confirmation-time lease/journal fencing, restart reconstruction, or T1/T2
recovery. Its current Node 22 non-process matrix passed 178/178 across 17
Vitest files; the asset/custom-chain/process-summary suite passed 34/34; the
report suite passed 9/9; and the corrected semantic L1-control-plane source
assertion passed 26/26. The real-process SIGKILL/lease-contention suite did not
produce checkpoint markers inside the restricted process sandbox (one failed,
four skipped), and the required outside-sandbox rerun was denied by the
platform execution limit. The historical 65/65 run is therefore retained as
prior evidence only, not substituted for a current final-tree process gate.

The rereview also records a non-safety fidelity risk: an active builder may be
waited on for up to 12 seconds rather than immediately preempted when
confirmation arrives. The one-hour cadence gate must determine whether this is
material; it is not grounds to change the strict default or waive the process
gate.
