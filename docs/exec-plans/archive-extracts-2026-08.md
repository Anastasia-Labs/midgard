# Archive extracts — 2026-08 docs cleanup

Still-live items salvaged from the July-2026 throughput ExecPlans
(`docs/exec-plans/throughput/`, deleted in the 2026-08 cleanup) before their
deletion. Each item cites the deleted file and its last git commit so the full
context is recoverable from history. Everything else in the deleted documents
was delivered milestone narrative or is superseded by the kept gate runbooks in
`docs/benchmark-scenarios/`, the plan docs in this directory, and the
`GOAL_PROGRESS.md` ledger. The 19 session-diary `*checkpoint-2026-07-2*.md`
files deleted in the same pass required no extraction: every "remaining gate"
they recorded is tracked by `cardano-capability-proof-completion.md`,
`cardano-capability-p2-closure-matrix-2026-07-26.md`, and the Goal ledger.

## DA transport / payload version decision — never taken; V1 stays pinned

From `throughput/phase-5-da-transport-version-proposal.md` (last commit
`1cf606531`) and `throughput/phase-5-da-hardening.md` (`1cf606531`).

- Production remains fail closed on `DA_TRANSPORT_LIMITS_V1.maxPayloadBytes =
  64 MiB`; the committee enforces `payloadVersion === 1`
  (`demo/da-committee-node/src/domain.ts`). Any limit change is a
  protocol-version decision, not an environment override.
- Measured blockers at 100k txs: operational inner payload 84,506,373 B (over
  64 MiB), compression ratio 2.9787x (below the 3x criterion), so the 100k
  one-hour soak is unrunnable without bypassing the pre-submit safety gate;
  threshold-publication ≤2 s p99 was never established (exact-50k samples span
  1.903–2.493 s, individual observations only). The 50k distribution gate's
  NO-GO status lives in `docs/benchmark-scenarios/phase-5-da-50k-distribution.md`.
- The proposal's recommendation, if the capacity work is ever resumed: keep V1
  pinned; the durable design is a delta-payload inner schema (block delta +
  header/root bindings) with content-addressed, chunked checkpoint/bootstrap —
  periodic full-state checkpoints alone eventually exceed any single-artifact
  cap. The two 256 MiB options (single 256 MiB transport cap, or 64 MiB stored
  / 256 MiB inner) are transitional at best and require a new protocol
  version, decode-memory admission controls, and mixed-version rollout tests;
  never an in-place change to V1 constants. Approval must name the cap model,
  committee memory budget, decode concurrency, migration epoch, mixed-version
  threshold policy, and rollback boundary.

## Phase 4 0-conf commit chaining — unscheduled design spike

From `throughput/phase-4-pipelined-commits.md` (last commit `c4e0ac9a7`), §9.

- Not spec'd for implementation. A ≤1-week spike is warranted only if the
  one-hour cadence gate measures L1 confirmation latency as the binding term
  (>~19 s sustained at 50k-tx blocks).
- The five unanswered protocol questions: (1) rollback blast radius and
  cascading abandonment when a chained parent rolls back (proposed max chain
  depth 2); (2) generalizing the single-active-journal invariant to a chain of
  journals with parent pointers; (3) whether deployed submit providers accept
  chained-unconfirmed submissions, plus child eviction/resubmission semantics;
  (4) whether a child tx can satisfy the CommitBlockHeader validator's tail
  datum and scheduler-window checks against chain state that excludes its
  parent; (5) watcher/DA-attestation/merge-maturity exposure of two unconfirmed
  headers.
- Unlock requires the spike exit criteria (written answers, reviewed rollback
  design, emulator 2-chain rollback-recovery prototype, measured cadence
  delta), ≥20% additional Stage C headroom at real confirmation latency, and
  confirmed provider support. Absent any of the three, speculative build +
  submit-on-confirm is the terminal Phase 4 design. `SPECULATIVE_COMMIT_BUILD`
  stays `false` by default until the acceptance gates in
  `docs/benchmark-scenarios/phase-4-pipelined-one-hour.md` pass; that gate must
  also judge the recorded fidelity risk that an active builder can be waited on
  up to 12 s instead of being preempted on confirmation.

## D3 multi-block merge — deliberately never planned

From `throughput/README.md` (last commit `8ddd0ff88`), bottleneck map.

- Merging k>1 blocks per L1 tx was deliberately given no ExecPlan: it depends
  on whether the Aiken state-queue linked-list validator permits multi-node
  advances, which was never assessed. That validator-rules assessment is the
  prerequisite for any future work. Fallback if k>1 is disallowed: pipeline
  merge txs with the Phase 4 speculative pattern (the Phase 4 lease/skip
  contracts intentionally leave merge extra lease headroom). Do not start
  before Phase 4's lease semantics are accepted.

## Architecture G default flip — remaining closure sequence

From `throughput/phase-3-architecture-g-production-integration.md` (last
commit `2b755a776`), §6. GOAL_SPEC.md explicitly excludes this flip from the
current Goal; it remains opt-in (`MPF_ENGINE=architecture_g`).

- Remaining order before any default flip: final locked native binary +
  recorded identities; full differential/protocol/crash/promotion suite rerun
  on the final tree; formal retained-growth root gate over fresh 100k/300k/1M
  fixtures and the formal 50k gate (nearest-rank p95 < 10 s) with their
  cross-bound production commit-candidate gates — the runners are checked in
  at `demo/midgard-node/scripts/mpf-architecture-g-gate.mjs` and
  `mpf-architecture-g-commit-candidate-gate.mjs`; release-image boundary
  inspection; one clean opt-in deployment through
  deposit/L2/DA/merge/restart/recovery plus the 24-hour soak; independent
  final review. Only then a default or cap/model/root-check change, in a
  separate reviewed commit. The operator runbooks for the non-performance
  surfaces and the soak are kept at
  `docs/benchmark-scenarios/phase-3-architecture-g-closure.md` and
  `docs/benchmark-scenarios/phase-3-architecture-g-soak.md`.
