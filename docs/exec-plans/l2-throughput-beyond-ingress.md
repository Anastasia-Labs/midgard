# L2 throughput beyond ingress: block production and read surfaces

**Status:** Proposal. No owner rulings yet. Nothing here is implemented.

**Last reviewed:** 2026-11-06, against tree `dcbecd108`.

**Related:**
[`docs/exec-plans/public-submit-ingress-design.md`](public-submit-ingress-design.md)
covers `POST /submit` up to durable admission.
[`docs/research/l2-tx-ingress-throughput.md`](../research/l2-tx-ingress-throughput.md)
covers what production L2s do.

## Scope and goal

The goal is 1,000 L2 TPS end to end. This plan covers what limits throughput
after a tx is durably admitted: block production, chiefly. It also covers the
read surfaces clients need at that rate.

## Block production

These findings come from a code read and were spot-checked at the lines cited.
None were measured. Paths are under `demo/midgard-node/src/`.

| Constraint | Evidence | Effect |
| --- | --- | --- |
| The planner caps the commit tx at 128 KiB, estimated as `512 + 32 × N` bytes. That gives **4,080 txs per block**, below the consensus 10,000. The commit tx carries roots and counts, not data per tx, so the 32 B/tx model looks stale. | `workers/utils/commit-block-planner.commit-scheduler-evidence-key.ts:121,130`, `workers/utils/commit-block-planner.select-commit-tx-candidates.ts:194,224` | Node-local cap. |
| One block is in flight at a time. The next build waits until the previous block lands on L1 (depth 1), then for a recovery tick. | `fibers/block-commitment.build-and-submit-commitment-block-action.ts:335-353`, `workers/commit-block-header.build-on-verified-base-program.ts:380-392` | A cycle takes about 28–30 s on preprod (~20 s mean L1 block). |
| Validation pauses while local finalization is pending. | `fibers/tx-queue-processor.tx-queue-processor-action.ts` (the `localFinalizationPending` early return) | Validation catches up in a burst of about 5 s per cycle. |
| The commit tx spends the state-queue tail. | midgard-sdk `state-queue-transactions.build-commit-block-header-tx-program.ts:212` | At most 1 commit per L1 block unless commits are chained. |
| Commit-base hydration loads the whole `confirmed_ledger` and recomputes the MPF roots in TS. The commit worker re-runs phase A serially. | `workers/commit-block-header.resolve-commit-base-ledger-entries.ts:134,184`, `mpf/process.process-mpfs.ts:687-702` | The build cost grows with the size of the UTxO set: about 3.6M new UTxOs per hour at 1,000 TPS. |
| Shift handover: no commit in the last 120 s of a shift, and the successor replays the predecessor's blocks first. | `services/history-commit-window.ts:80` | About 3 % lost, plus a gap at each handover. |

**Today's ceiling:** about 140 TPS at 4,080 txs per ~29 s cycle.

**Hard ceilings without chaining:**
- With the cap fixed: about 285 TPS.
- With perfect pipelining at one commit per L1 block: 10,000 / ~20 s =
  500 TPS.

1,000 TPS means about 20,000 txs per L1 block on average. L1 block gaps follow
an exponential distribution, so a 60 s gap holds 60,000 txs.

### Options

- **B1** Replace the 32 B/tx commit-size estimate with the commit tx's real
  size model. Node-local.
- **B2** Keep validating while local finalization is pending, and gate only
  the commit. Check first why validation pauses.
- **B3** Chain commits: build N+1 on N's unconfirmed output so that several
  commits fit in one L1 block. Off-chain. It must be checked against:
  - the commit validator's validity-range and event-depth rules;
  - the ex-unit and size limits of an L1 block;
  - DA attestation applies that spend the tail;
  - the rollback and liveness rules.
- **B4** Hydrate the commit base in O(touched entries), and drop the full root
  recompute in TS. Run the commit worker's phase A in parallel, or reuse the
  pipeline's results.
- **B5** Raise `maxL2TransactionCount` above 10,000 per block. This is a
  consensus change that moves fraud-proof sizing. **Owner decision.** It is
  an alternative to B3.

Other things to check:
- merge throughput must match the commit cadence;
- commit, confirmation and merge share one L1 control-plane mutex;
- DA upload sits on the critical path;
- the 20,000 `maxValidationTraceCount` for script-heavy load.

## Read surfaces

Today `POST /tx-status` is the `batch` class at 4/s globally, and all GET
reads, including `/utxos`, share 100/s globally (`services/http-rate-limits.ts:71-78`).
At 1,000 TPS, clients cannot learn outcomes or fetch UTxOs.

- **R1** Charge `/tx-status` per id, not per request.
- **R2** Add a server-sent events stream of tx outcomes, keyed by tx id.
- **R3** Move reads onto the ingress design's fixed-memory limiter. Size
  `read` for several reads per submitted tx. The public nginx edge also caps
  every route at 100 r/s for the whole server
  (`scripts/public-ingress.mjs:63-78`); the ingress design's D7 removes that
  for reads too.

## Rollout and acceptance

1. Measure the commit cycle, build ms per tx and hydration cost at a
   realistic ledger size.
2. B1, B2, B4.
3. B3 (or B5).
4. R1–R3 alongside.

**End-to-end acceptance:**
- 1,000 TPS for 1 hour on the public profile, from at least 1,000 anonymous
  clients (there are no API keys, owner 2026-11-06), with 20 % invalid
  traffic.
- Inclusion p99 under 2 L1 blocks.
- The admission backlog stays below 50 %.

## Decision for the owner

Chained commits (B3, off-chain) or more txs per block (B5, consensus).
