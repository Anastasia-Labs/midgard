# 0002 — Canonical V1 Goal economics and margin decision record (F04)

- **Status:** PROVISIONAL — values unblock local work per `GOAL_SPEC.md` §7
  F04; owner approval is required before CG5 binds them into the release
  identity. Approving this record flips Status to ACCEPTED.
- **Owner/approver:** repository owner (Philip DiSarro).
- **Date:** 2026-07-29.
- **Consumers:** Q53, Q61, Q63, W04, W12, W31, C74, C80 (`GOAL_SPEC.md` §3.3,
  §7). No task may invent a value this record owns.

## 1. Fixed protocol constants (recorded, not chosen)

| Constant | Value | Source |
| --- | --- | --- |
| Canonical V1 block maturity | 604,800,000 ms (7 days) | `docs/consensus-profile-v1.md` (exact V1 maturity) |
| Compiled 32-round dispute schedule, derived minimum maturity | 39,600,000 ms (11 h) | `docs/consensus-profile-v1.md` |
| Execution reserve | ≥ 20% below measured protocol limits | `docs/consensus-profile-v1.md` §10; `GOAL_SPEC.md` §3.3 |
| Maturity fit | complete worst-case correction path ≤ ½ maturity (302,400,000 ms) | `GOAL_SPEC.md` §3.3 |

The compiled dispute schedule (11 h) fits half maturity (84 h) with a 7.6×
margin before DA fetch/construction/confirmation overhead; W04/C74 must prove
the complete measured path still fits.

## 2. Economics (PROVISIONAL; replaces the zero placeholders in `onchain/aiken/env/{default,testnet}.ak`)

| Parameter | Value (lovelace) | Rationale |
| --- | --- | --- |
| `slashing_penalty` | 500,000,000 (500 tADA) | Must exceed worst-case prover cost plus reward so fraud is never profitable to absorb; sized ≥ 5× the provisional prover-cost ceiling below. |
| `fraud_prover_reward` | 400,000,000 (400 tADA) | Must exceed measured worst-case prover cost by ≥ 3×; C74 confirms the multiple, else this value rises. |
| `inactivity_slashing_penalty` | 100,000,000 (100 tADA) | Strictly less than `slashing_penalty` so a partially slashed operator still funds the full prover reward (see env TODO note). |
| `required_bond` | `slashing_penalty + fraud_prover_reward` = 900,000,000 | Formula preserved from env; bond always covers slash + reward. |
| Provisional prover-cost ceiling | 100,000,000 (100 tADA) | Planning bound for full interactive-game fees/collateral; C74 measurement supersedes. |
| Per-transaction collateral | 5,000,000 | Standard; W31 enforces reservation. |
| Operator wallet floor | 1,000 tADA | Registration bond + fee headroom. |
| Prover/watcher wallet floor | W31-computed worst-case sweep funding | C80 verifies before any state-changing step. |

## 3. Finality, retries, deadlines (PROVISIONAL)

| Parameter | Value | Rationale |
| --- | --- | --- |
| `finalityDepth` (local_node and external_providers) | 30 blocks (~10 min Preprod) | Within watcher config bounds (1–2,160); deep enough that finalized rollback is incident-grade, shallow enough for a ≤ 48 h acceptance sweep. |
| Rollback handling below depth | pending-state rewind (W13) | Spec §3.1.8. |
| Submission retry budget | 5 attempts, exponential backoff capped at 120 s | Bounded by §3.3 maturity fit; W33 reconciles ambiguity before any retry. |
| DA availability-challenge response deadline | 3,600,000 ms (1 h) | Fits drill inside the acceptance window; retention makes longer response unnecessary. |
| `da_attestation_timeout` (Q61) | 3,600,000 ms (1 h) | A live committee attests in seconds; 1 h cannot trigger accidentally yet keeps the head-of-line unblock drillable and the queue live. Timeout removal does not slash (D-L1 recommendation). |
| DA retention (`RETENTION_DAYS`, `minimumRetentionDays`) | 15 days | ≥ maturity (7 d) + worst-case proof time + margin; matches `LIBP2P_DA_MIN_RETENTION_DAYS = 15`. |

## 4. DA-governor floors (Q63, PROVISIONAL)

- `da_threshold` ≥ max(2, ⌈2·committee_len/3⌉); the governor rejects any
  update below the floor.
- `update_threshold` ≥ max(2, ⌈2·owner_len/3⌉); single-key capture of either
  threshold is unrepresentable.
- Mid-flight committee rotation must leave partially signed attestations
  rescuable/refundable (Q63 acceptance).

## 5. Resource ceilings (C80, PROVISIONAL)

| Container class | Memory | CPU | PIDs |
| --- | --- | --- | --- |
| midgard-node | 8 GiB | 4 | 512 |
| DA committee node (each) | 4 GiB | 2 | 256 |
| midgard-watcher | 4 GiB | 2 | 256 |
| Postgres (each) | 4 GiB | 2 | 256 |
| Whole acceptance topology | ≤ 28 GiB / ≤ 14 vCPU total | | |

## 6. Acceptance-window check (`GOAL_SPEC.md` §7 F04)

With the 1 h availability/attestation deadlines, 30-block finality, and
journal-resumable parallel drills (C83/Q57 single-execution rule), the
complete C83–C87 sweep is planned ≤ 48 h. Any value change that breaks this
bound or a §3.3 threshold reopens this record.

## 7. Open items binding this record

- C74 measured worst-case prover cost → confirms or raises §2 values.
- W31 worst-case sweep funding computation → concrete wallet floors.
- Owner approval → Status ACCEPTED before CG5.
