# 0002 — Canonical V1 Goal economics and margin decision record (F04)

- **Status:** PROVISIONAL — values unblock local work per `GOAL_SPEC.md` §7
  F04; owner approval is required before CG5 binds them into the release
  identity. Approving this record flips Status to ACCEPTED.
- **Owner/approver:** repository owner (Philip DiSarro).
- **Date:** 2026-07-29.
- **Consumers:** Q53, Q61, Q63, W04, W12, W31, C74, C80, and W46 (production
  hardware floor, §5.2) (`GOAL_SPEC.md` §3.3, §7). No task may invent a value
  this record owns.
- **Owner-accepted exceptions:** §2.1 (public preprod launch economics) and
  §5.2 (production hardware floor) are ACCEPTED by owner direction on
  2026-07-29; the rest of this record remains PROVISIONAL pending approval.

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

## 2. Economics (replaces the zero placeholders in `onchain/aiken/env/{default,testnet}.ak`)

### 2.1 Public preprod launch economics (ACCEPTED — owner-directed 2026-07-29)

These are the values for the public preprod deployment opened once the
system is feature-complete. They are owner-set and not provisional.

| Parameter | Value | Rationale |
| --- | --- | --- |
| `slashing_penalty` | 25,000 ADA | Penalty component of the forfeited bond. |
| `fraud_prover_reward` | 75,000 ADA | ≥ 7.5× the prover-cost ceiling below; makes proving strongly profitable. |
| `inactivity_slashing_penalty` | 10,000 ADA | Strictly less than `slashing_penalty`, preserving the env constraint that a partially slashed operator still funds the full prover reward. |
| `required_bond` | `slashing_penalty + fraud_prover_reward` = 100,000 ADA | Env formula preserved exactly; a fraudulent operator forfeits the full 100k bond, so fraud is unprofitable unless the expected gain exceeds it. |
| Prover-cost ceiling | 10,000 ADA | Planning bound for full interactive-game fees/collateral; C74 measurement supersedes and the reward must stay ≥ 3× the measured cost. |
| Operator wallet floor | `required_bond` + fee headroom | Headroom sized by the W31 worst-case computation. |
| Prover/watcher wallet floor | W31-computed worst-case sweep funding | C80 verifies before any state-changing step. |
| Per-transaction collateral | 5 ADA | Standard; W31 enforces reservation. |

### 2.2 Bounded-acceptance profile (PROVISIONAL, tADA — this Goal's drills only)

The Goal's target-testnet acceptance run SHOULD deploy the §2.1 values. Only
if Preprod faucet supply genuinely constrains the drill sweep may it use this
scaled profile, which preserves the structural relations (`required_bond =
slashing_penalty + fraud_prover_reward`; `inactivity_slashing_penalty <
slashing_penalty`; reward > measured prover cost with margin):

| Parameter | Scaled value |
| --- | --- |
| `slashing_penalty` | 500 tADA |
| `fraud_prover_reward` | 400 tADA |
| `inactivity_slashing_penalty` | 100 tADA |
| `required_bond` | 900 tADA |

The release evidence must record exactly which profile the acceptance
deployment used; the public launch uses §2.1 verbatim under its own
deployment identity, and no acceptance shortcut lowers §2.1.

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

## 5. Resource ceilings and hardware floors

### 5.1 Local acceptance-topology ceilings (C80, PROVISIONAL — this Goal only)

These are enforced *containment caps* for the bounded target-testnet
acceptance run on the owner's workstation. They exist to keep the acceptance
topology from consuming the host and to prove the workload is boundable. They
are deliberately small and are **not** hardware requirements, sizing
guidance, or a performance claim; W46 and the readiness document must never
present them as production specs.

| Container class | Memory | CPU | PIDs |
| --- | --- | --- | --- |
| midgard-node | 8 GiB | 4 | 512 |
| DA committee node (each) | 4 GiB | 2 | 256 |
| midgard-watcher | 4 GiB | 2 | 256 |
| Postgres (each) | 4 GiB | 2 | 256 |
| Whole acceptance topology | ≤ 28 GiB / ≤ 14 vCPU total | | |

### 5.2 Production hardware floor (ACCEPTED — owner-directed 2026-07-29)

For production operation of a high-throughput L2 node, the minimum
recommended hardware is:

| Role | Floor |
| --- | --- |
| midgard-node (operator) | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage |
| DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

This floor is owner-set and ACCEPTED (unlike the PROVISIONAL sections of
this record). W46 operational documentation and `public_testnet_readiness.md`
must carry it verbatim; C86 bounded-stress results refine the non-node role
sizing but cannot lower the node floor.

## 6. Acceptance-window check (`GOAL_SPEC.md` §7 F04)

With the 1 h availability/attestation deadlines, 30-block finality, and
journal-resumable parallel drills (C83/Q57 single-execution rule), the
complete C83–C87 sweep is planned ≤ 48 h. Any value change that breaks this
bound or a §3.3 threshold reopens this record.

## 7. Open items binding this record

- C74 measured worst-case prover cost → confirms the §2.1 reward multiple
  (raise-only) and validates the §2.2 scaled profile's margin.
- W31 worst-case sweep funding computation → concrete wallet floors and the
  §2.2-vs-§2.1 faucet-feasibility determination for the acceptance run.
- Owner approval of the remaining PROVISIONAL sections → full ACCEPTED
  status before CG5. §2.1 and §5.2 are already ACCEPTED by owner direction.
