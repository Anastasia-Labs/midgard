# E2E Stress Test: Evaluation & Execution Plan

**Status:** Historical benchmark parent analysis from 2026-07-08; do not execute
this file as a current runbook. The throughput phase plans and
`demo/midgard-node/scripts/throughput-valid-stress.mjs` own the current workflow.

**Last reviewed:** 2026-07-22
**Verdict:** **Fix, don't scrap** — but the fix is a redesign of _which component is the benchmark_, not a patch to the component currently reporting "TPS."
**Companion doc:** `THROUGHPUT-2500-TPS-PLAN.md` (node-side bottlenecks; this plan provides the measurement instrument that plan needs).

---

## 1. Diagnosis: the ~10 tx/s number is a harness artifact

Your suspicion is confirmed by both code and run artifacts. The node was never saturated.

**The arithmetic that produces "10 tx/s."** The default stress path (`src/commands/e2e-stress-l2-throughput.ts`) is a **closed-loop** generator: each worker lane awaits build → submit → _poll-until-accepted_ before submitting its next tx (`e2e-stress-l2-throughput.ts:1926-1962, 2109-2128`). The acceptance poll sleeps a fixed **2,000 ms** per tick (`DEFAULT_POLL_INTERVAL_MS`, `:303, :1123`), concurrency defaults to **1** and is hard-capped at **16** lanes (`:301, :309, :758-762`), count is capped at 500 (`:308`). 16 lanes ÷ ~2 s cycle ≈ **8 tx/s** — the observed ceiling is built into the client. The code even labels this path `classification: "closed_loop_smoke"` (`:2278`).

**Three additional client-side serializers:** each tx is built inline in the timed loop — fresh `/utxos` HTTP fetch + full lucid/CML tx build + sign, single-threaded (`submit-l2-transfer.ts:547-596, 785-882`); each lane must re-observe its own change output via `/utxos` before building the next tx (an implicit serial chain per wallet); and bare `fetch` with no connection pooling.

**The run artifacts prove the node wasn't the limiter.** In `logs/e2e-fanout-independent40-20260705T172245Z/`, the literal 10.08 tx/s figure is **12 txs ÷ 1.19 s** — a burst window in a run where 28/40 wallets failed client-side before submitting (`Insufficient lovelace: required 1003110, available 1000000` — a funding-margin bug, plus a `STRESS_WALLET_SEED_PHRASE_01` vs `_0001` env-var naming bug). Across every examined run: **zero** HTTP 503s, **zero** validation rejections (`rejected_by_code=[none]` in every node batch log), and node-side DB-timestamped admission ran at **27.8–81.5 tx/s** on tiny 12–40-tx bursts — i.e., 3–8× the client-reported number, on bursts far too small to find any ceiling. All submissions land in a ~15 ms–3 s burst; the remaining 4–9 minutes of each run is the harness idly polling for L1 commit.

**What the low "committed TPS" actually measured:** real node-side problems, not throughput — commit windows of 35–60 s for any batch size (L1-confirmation-gated single commit worker; see companion doc), a state-queue mutation lease held up to 10 minutes starving merges, and in the one 500-tx run, the commitment/confirmation workers **crashed** (`DatabaseInitializationError`, Kupmios timeouts) leaving 500 accepted / 0 committed. These are genuine defects the new benchmark must keep surfacing — but they are pipeline-latency and reliability findings, not a 10 TPS capacity measurement.

**The repo already knows.** `e2e-stress-l2-throughput.ts` and its focused tests now separate acceptance from finality and preserve the measured closed-loop ceiling evidence (1.88 tx/s = 4 workers × 2.13 s cycles). The formerly referenced reliability plan is no longer retained; the executable throughput phase plans supersede it.

## 2. Why fix rather than scrap

The correct architecture already exists in the repo in two partially-finished pieces; scrapping would rebuild exactly these:

| Component                                                                         | State                                                                                                                                                                    | Keep?                                                                                                                                                     |
| --------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `e2e-stress-l2-throughput.ts` closed-loop (`serial-chain`, `parallel-fanout`)     | Working, but structurally a smoke test                                                                                                                                   | Keep **as smoke test only**; stop reporting TPS from it                                                                                                   |
| `stress-open-loop.ts` (`--load-model open-loop-upper-bound`)                      | Real open-loop: fixed-rate scheduler, `maxInFlight` window (`:361-497`)                                                                                                  | Keep — but unusable today: requires a pre-built CBOR corpus and **no corpus generator exists anywhere** (grep confirms only consumers reference `corpus`) |
| `demo/midgard-node/scripts/throughput-valid-stress.mjs`                           | Most capable engine: undici connection pool, prebuilt tx chains, `closed/open/ramp/find-max` modes, offered-rate guard, 503/429 retry | Canonical benchmark engine; use the current phase plans for configuration and acceptance criteria. |
| `stress-stage-metrics.ts`                                                         | Right stage model (durable admission → l2 admission → L1 commit → finality), honest caveat notes                                                                         | Keep — but primary metric falls back to client-observed poll timestamps when DB sources aren't wired (`:404-423`); must be DB-grounded                    |
| e2e orchestration (`.agents/skills/midgard-e2e-acceptance/SKILL.md`, `src/e2e/*`) | Solid runbook/step-runner; environment is single-host with load-gen, node, Postgres, cardano-node, Kupo/Ogmios and full observability stack sharing one machine          | Keep runbook; fix environment isolation for benchmark runs                                                                                                |

Estimated fix effort is ~3–4 engineer-weeks against ~8+ to rebuild equivalents from scratch, with the redesign work (plan 42) already specced in-repo.

## 3. Execution plan

### Phase 0 — Stop the bleeding (days, no redesign)

1. Rename/relabel: closed-loop output must report `burst_cycle_rate`, never `tx/s`/`TPS`; keep it as the acceptance smoke gate it already claims to be (`closed_loop_smoke`).
2. Fix the known run-killing bugs: seed-phrase env naming (`_01` vs `_0001`), funding margin (fund wallets with fee headroom: transfer amount + max fee, not exactly 1,000,000), and make funding failures abort the run loudly instead of silently shrinking the sample from 40 → 12.
3. Make `pollUntilAccepted` adaptive (50–100 ms initial, exponential to 1 s) so even smoke runs stop paying the 2 s floor.

### Phase 1 — Corpus generator (week 1) — the missing piece

New command `stress-corpus-generate`:

1. Inputs: N sender wallets × chain depth D, transfer template, target = `rate × duration × 1.1` txs (2,500 TPS × 10 min ⇒ ~1.65 M txs).
2. Pre-derive each chain's UTxO lineage locally (deterministic change outputs) so txs chain **without** any `/utxos` round-trip; build + sign across `worker_threads` (CML signing parallelizes cleanly); emit the NDJSON `OpenLoopCorpusRow` format `stress-open-loop.ts:10-21` already consumes.
3. Wallet automation: extend `stress-wallets.ts` to derive and fund hundreds–thousands of wallets from one treasury in batched L2 transfers (fan-out tree), replacing the manual 16-seed-env-var workflow. Chain-capacity math: offered TPS ≤ chains ÷ submit-latency; at p99 ≈ 750 ms observed, 2,500 TPS needs ≥ ~2,000 independent chains — size N accordingly.

### Phase 2 — One canonical benchmark engine (weeks 1–3)

Promote `demo/midgard-node/scripts/throughput-valid-stress.mjs` to the canonical engine (it already has the pool, modes, and guard rails) and close its gaps:

1. Consume the Phase 1 corpus (drop inline building entirely from the timed path).
2. Client calibration stage per plan 42: measure generator max rate against a no-op/echo endpoint first; refuse to report node TPS unless offered load ≥ 1.2× measured node rate (proves the client wasn't the ceiling).
3. Raise defaults for benchmark profile (`STRESS_MODE=ramp`, `maxChains` sized from corpus, `submitConcurrency`/connections per calibration).
4. Emit `events.ndjson` in the schema `stress-stage-metrics.ts` consumes, then retire the TS `open-loop-upper-bound` path or reduce it to a thin wrapper that shells out to the engine — one engine, not three.

### Phase 3 — Ground-truth metrics (weeks 2–4)

1. Wire `collectStageMetricSources` to Postgres so every stage rate comes from DB timestamps (`tx_admissions` created/accepted, mempool insert, block inclusion, L1 confirmation) — never from client poll observations.
2. Steady-state windowing: trim warm-up and drain; report offered vs durably-admitted vs validated vs committed TPS separately, with p50/p95/p99 latency per stage. Per-stage reporting is essential: the commit stage is capped at ~250–500 TPS by node design (companion doc), and a single blended "TPS" number would mask ingestion/validation improvements behind it.
3. Environment fingerprint in every report: git SHA, image digests, host CPU/RAM, whether load-gen was co-hosted, calibration proof, config profile hash. A TPS number without this is inadmissible.

### Phase 4 — Environment realism (weeks 3–4, parallel)

1. Run the load generator **off-host** (plan 42's own requirement) — today it competes with node, Postgres, cardano-node, Kupo, Ogmios, and the full Grafana/Loki/Tempo/cAdvisor stack on one machine.
2. Add a `benchmark` env profile distinct from the e2e-acceptance profile: production-representative `WAIT_BETWEEN_*`/validation settings, admission backlog caps raised or explicitly recorded, observability stack optional.
3. Document (don't hide) Preprod L1 variance: acceptance-TPS benchmarks (stages A–B) are reproducible; full-pipeline committed-TPS runs are hostage to Preprod block times — report them as separate scenario classes.

### Phase 5 — Scenarios, gates, and defect tracking (weeks 4–5)

1. Standard scenarios: `find-max` ramp (discover current ceiling), 10-min soak at discovered max, 2,500 TPS offered-load acceptance run (the target gate for the throughput plan), burst 2× target, and a mixed workload with multi-input/output txs (current corpus of minimal transfers understates per-tx cost).
2. CI regression tracking of per-stage TPS across commits.
3. File the node defects this analysis surfaced as first-class bugs with reproductions via the new harness: commit-worker crash at 500 accepted txs (`DatabaseInitializationError`), mempool drain failure, 10-minute mutation-lease starvation of merges.

## 4. Success criteria

The benchmark is fixed when: (1) a calibration report proves offered load exceeded measured node throughput; (2) all stage rates come from DB timestamps over a trimmed steady-state window; (3) a run at 2,500 TPS offered load completes and reports per-stage acceptance/validation/commit rates without client-side failures; (4) results are reproducible within ±10% across three consecutive runs with identical fingerprints; and (5) the closed-loop smoke path no longer emits anything labeled TPS.
