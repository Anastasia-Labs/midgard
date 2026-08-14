# 0002 — Canonical V1 Goal economics and margin decision record (F04)

- **Status:** ACCEPTED — owner-approved in full on 2026-08-04. CG5 may bind
  these values into the release identity after the named consumers pass.
- **Owner/approver:** repository owner (Philip DiSarro).
- **Date:** 2026-07-29; final approval and routing/idempotency disposition:
  2026-08-04.
- **Consumers:** Q53, Q54, Q61, Q63, W04, W12, W31, C74, C80, and W46 (production
  hardware floor, §5.2) (`GOAL_SPEC.md` §3.3, §7). No task may invent a value
  this record owns.
- **Approval history:** §2.1 (public preprod launch economics), the §3 finality
  rows (`finalityDepth` 30 with the automated deep-rollback-to-`k` condition),
  and §5.2 (production hardware floor) were accepted on 2026-07-29. The owner
  accepted the complete record, including §§2.2–2.4 exact routing and
  idempotency semantics, on 2026-08-04.

## 1. Fixed protocol constants (recorded, not chosen)

| Constant                                                     | Value                                                             | Source                                                  |
| ------------------------------------------------------------ | ----------------------------------------------------------------- | ------------------------------------------------------- |
| Canonical V1 block maturity                                  | 604,800,000 ms (7 days)                                           | `docs/consensus-profile-v1.md` (exact V1 maturity)      |
| Compiled 32-round dispute schedule, derived minimum maturity | 39,600,000 ms (11 h)                                              | `docs/consensus-profile-v1.md`                          |
| Execution reserve                                            | ≥ 20% below measured protocol limits                              | `docs/consensus-profile-v1.md` §10; `GOAL_SPEC.md` §3.3 |
| Maturity fit                                                 | complete worst-case correction path ≤ ½ maturity (302,400,000 ms) | `GOAL_SPEC.md` §3.3                                     |

The compiled dispute schedule (11 h) fits half maturity (84 h) with a 7.6×
margin before DA fetch/construction/confirmation overhead; W04/C74 must prove
the complete measured path still fits.

## 2. Economics (replaces the zero placeholders in `onchain/aiken/env/{default,testnet}.ak`)

### 2.1 Public preprod launch economics (ACCEPTED — owner-directed 2026-07-29)

These are the values for the public preprod deployment opened once the
system is feature-complete. They are owner-set and not provisional.

| Parameter                     | Value                                                  | Rationale                                                                                                                                       |
| ----------------------------- | ------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `slashing_penalty`            | 25,000 ADA                                             | Penalty component of the forfeited bond.                                                                                                        |
| `fraud_prover_reward`         | 75,000 ADA                                             | ≥ 7.5× the prover-cost ceiling below; makes proving strongly profitable.                                                                        |
| `inactivity_slashing_penalty` | 10,000 ADA                                             | Strictly less than `slashing_penalty`, preserving the env constraint that a partially slashed operator still funds the full prover reward.      |
| `required_bond`               | `slashing_penalty + fraud_prover_reward` = 100,000 ADA | Env formula preserved exactly; a fraudulent operator forfeits the full 100k bond, so fraud is unprofitable unless the expected gain exceeds it. |
| Prover-cost ceiling           | 10,000 ADA                                             | Planning bound for full interactive-game fees/collateral; C74 measurement supersedes and the reward must stay ≥ 3× the measured cost.           |
| Operator wallet floor         | `required_bond` + fee headroom                         | Headroom sized by the W31 worst-case computation.                                                                                               |
| Prover/watcher wallet floor   | W31-computed worst-case sweep funding                  | C80 verifies before any state-changing step.                                                                                                    |
| Per-transaction collateral    | 5 ADA                                                  | Standard; W31 enforces reservation.                                                                                                             |

### 2.2 Bounded-acceptance profile (ACCEPTED, tADA — this Goal's drills only)

The Goal's bounded target-testnet acceptance run uses this scaled profile
exactly. It is a drill-only deployment identity and is never a public launch
profile. This removes a runtime/profile-selection decision from C80 and
preserves the structural relations (`required_bond =
slashing_penalty + fraud_prover_reward`; `inactivity_slashing_penalty <
slashing_penalty`; reward > measured prover cost with margin):

| Parameter                     | Scaled value |
| ----------------------------- | ------------ |
| `slashing_penalty`            | 500 tADA     |
| `fraud_prover_reward`         | 400 tADA     |
| `inactivity_slashing_penalty` | 100 tADA     |
| `required_bond`               | 900 tADA     |

The values in lovelace are respectively `500_000_000`, `400_000_000`,
`100_000_000`, and `900_000_000`. Release evidence records the bounded
acceptance deployment identity and profile. The public launch uses §2.1
verbatim under a distinct deployment identity; no acceptance shortcut lowers
or aliases the public profile.

The aligned Preprod funding snapshot at slot `130_139_556` does not satisfy
either profile: the operator wallet holds `871_478_082` lovelace, which is
`28_521_918` lovelace below the bounded bond before fee headroom, and the
largest observed role wallet (`27_142_152_817` lovelace) is below the public
`100_000_000_000`-lovelace bond. C80 must fail closed until W31 computes the
complete headroom and the correctly credentialed operator, prover/watcher, DA,
collateral, and fee wallets are funded to their resulting floors. Existing
role-wallet funds are not silently reassigned or counted across credentials.

### 2.3 Exact fraud-slash value routing (ACCEPTED)

The operator-directory node holds exactly one slashable bond tranche. It is
the sole source of the protocol reward and penalty; wallet balancing, change,
or the submitting wallet is not a reward-routing rule.

- For a fully bonded node, the tranche is exactly `required_bond`. The
  successful fraud-slash transaction pays exactly `fraud_prover_reward` as an
  ADA-only output to the enterprise address whose payment credential is the
  `fraud_prover` verification-key hash preserved in the authentic fraud-proof
  datum, and sets the transaction fee to exactly `slashing_penalty`.
- For a node previously inactivity-slashed by exactly
  `inactivity_slashing_penalty`, the remaining tranche is exactly
  `required_bond - inactivity_slashing_penalty`. The fraud-prover output stays
  exactly `fraud_prover_reward`; the transaction fee is exactly
  `slashing_penalty - inactivity_slashing_penalty`. Across the two slash
  transactions, the total penalty is therefore exactly `slashing_penalty`.
- The prover payment credential must match the immutable computation-thread
  prover identity, which `Init` authenticates with an extra signatory. A
  submitter, change address, stake credential, CLI flag, or first transaction
  signer cannot redirect the reward. The same prover must be an extra
  signatory of the reward-bearing slash transaction. On-chain enforcement, not
  an SDK preflight, establishes both bindings.
- Every reward-bearing fraud-slash reason uses this allocation. Bad-state
  slashing obtains the claimant from the authentic fraud-proof datum. Bad-
  settlement disproof and duplicate-registration slashing add an explicit
  bounty claimant to their canonical redeemer, require that claimant's extra
  signature, and pay the claimant's enterprise address. They never infer the
  claimant from a caller-supplied destination or change address.
- The reward output and exact fee conserve the complete designated bond
  tranche. Inputs used for unrelated minimum ADA, collateral, or construction
  fees balance independently and cannot reduce, supplement, or receive the
  reward. Registration and every directory transition preserve exactly the
  expected tranche rather than accepting an unclassified bond surplus.
- Only the transaction that both consumes the operator-directory node and
  removes the fraud-proved target block can pay the reward. Removing dependent
  successor links and the `OperatorAlreadySlashed` path pay no reward and do
  not levy the same penalty again.

The public-profile full/partial allocations are therefore
`100_000_000_000 = 75_000_000_000 + 25_000_000_000` lovelace and
`90_000_000_000 = 75_000_000_000 + 15_000_000_000` lovelace. The bounded
acceptance allocations are `900_000_000 = 400_000_000 + 500_000_000` and
`800_000_000 = 400_000_000 + 400_000_000` lovelace.

### 2.4 Duplicate-token and duplicate-reward prevention (ACCEPTED)

The deterministic claim identity is
`(deployment identity, fraud category ID, fraudulent header hash)`. Q53 must
make `Init` ledger-idempotent for that identity through a deployment-bound
singleton claim-lock/registry transition:

- at most one live computation thread exists for a claim identity;
- a concurrent or repeated `Init` while live rejects instead of minting a
  second copy of the deterministic asset name;
- cancellation atomically burns the live thread token and reopens the claim;
- success atomically burns that token, mints exactly one eternal fraud-proof
  token, and closes the claim permanently; and
- a closed claim, a second terminal mint, a replayed reward claim, or a reward
  attempt after the operator node has been consumed rejects on-chain.

A random nonce or a fresh wallet input is not a uniqueness guard: it would
only create multiple claim identities for the same fault. Durable watcher
submission IDs remain useful for retry reconciliation, but are not protocol
authorization. The singleton operator node plus the closed claim gives two
independent idempotency boundaries: at most one terminal proof token per
claim, and at most one reward from an operator's slashable bond.

### 2.5 Implementation evidence and Q53 replacement boundary

The approval is a decision, not evidence that Q53 is implemented. The
2026-08-04 audit found the following surfaces that Q53 must replace and test:

- `onchain/aiken/env/default.ak:19-33` and `env/testnet.ak:18-24` still compile
  all four economics values as zero.
- `lib/midgard/operator-directory.ak:295-306` checks only a fee floor and
  assumes the signer receives the remainder; it does not authenticate a prover
  datum or reward output. Registration likewise accepts `>= required_bond` at
  `validators/operator-directory/registered-operators.ak:138-140`.
- `validators/computation-thread.ak:76-126` derives the deterministic asset
  name and constrains one current transaction, but consumes no one-shot claim
  state. `validators/fraud-proof.ak:28-56` constrains the terminal burn/mint in
  one transaction only. Neither prevents a later duplicate mint.
- `validators/state-queue.ak:720-729` authenticates the proof-token policy and
  header suffix, but not its prover datum or a payout. The submitter preflight
  at `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts:2253-2258`
  checks prover/signature identity off-chain and constructs no exact reward
  output.
- The unreachable settlement helper's 60% remainder rule and the unrelated
  1/2-ADA SDK penalties (`demo/midgard-sdk/src/settlement.ts:686-734` and
  `protocol-parameters.ts:28-41`) are not economics authorities and must not be
  revived. Node 5-ADA bond/0.2-ADA slash examples and fixed 25-ADA activation
  headroom are replaced by the deployment-bound profile and W31 computation.

Until Q53 closes these gaps, an F04-approved profile must not be described as
deployed or economics-complete.

## 3. Finality, retries, deadlines (status per row)

| Parameter                                                        | Value                                                                                                                                                                                                                      | Rationale                                                                                                                                                                                                                     |
| ---------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `finalityDepth` (local_node and external_providers)              | 30 blocks (~10 min Preprod) — **ACCEPTED** (owner-directed 2026-07-29) for testing and, conditionally, for public launch                                                                                                   | Within watcher config bounds (1–2,160). The public-launch acceptance is conditional on the automated deep-rollback requirement below.                                                                                         |
| Cardano security parameter `k` (maximum credible rollback depth) | 2,160 blocks (Shelley genesis `securityParam`, mainnet and Preprod; ≈ 12 h at mainnet density)                                                                                                                             | Recorded fact, not a choice. The watcher config maximum (`finalityDepth`/`rollbackDepth` max `2_160`) already matches it.                                                                                                     |
| Automated deep-rollback handling — **ACCEPTED owner condition**  | A rollback deeper than `finalityDepth` but within `k` triggers automated W13 rewind/replay recovery plus W33 submission reconciliation and an explicit incident record; verification resumes without manual state surgery. | This automation is the condition under which `finalityDepth` 30 is acceptable for public launch: shallow finality for latency, full-`k` automated recovery for safety. W44 must include a deeper-than-finality rollback case. |
| Rollback handling below `finalityDepth`                          | pending-state rewind (W13)                                                                                                                                                                                                 | Spec §3.1.8.                                                                                                                                                                                                                  |
| Submission retry budget                                          | 5 attempts, exponential backoff capped at 120 s                                                                                                                                                                            | Bounded by §3.3 maturity fit; W33 reconciles ambiguity before any retry.                                                                                                                                                      |
| DA availability-challenge response deadline                      | 3,600,000 ms (1 h)                                                                                                                                                                                                         | Fits drill inside the acceptance window; retention makes longer response unnecessary.                                                                                                                                         |
| `da_attestation_timeout` (Q61)                                   | 3,600,000 ms (1 h)                                                                                                                                                                                                         | A live committee attests in seconds; 1 h cannot trigger accidentally yet keeps the head-of-line unblock drillable and the queue live. Timeout removal does not slash (D-L1 recommendation).                                   |
| DA retention (`RETENTION_DAYS`, `minimumRetentionDays`)          | 15 days                                                                                                                                                                                                                    | ≥ maturity (7 d) + worst-case proof time + margin; matches `LIBP2P_DA_MIN_RETENTION_DAYS = 15`.                                                                                                                               |

## 4. DA-governor floors (Q63, ACCEPTED; AMENDED 2026-08-11 and 2026-08-13)

- `da_threshold` ≥ ⌈2·committee_len/3⌉ for committee_len ≥ 1; the governor
  rejects any update below the floor.
- `update_threshold` ≥ ⌈2·owner_len/3⌉ for owner_len ≥ 1; the governor holds no
  separate owner-set minimum, so a lone owner governs at update_threshold 1.
- Mid-flight committee rotation must leave partially signed attestations
  rescuable/refundable (Q63 acceptance).

### 4.1 Amendments — the 1-of-1 prohibition is lifted (two owner rulings)

The two floor rows above carried a `max(2, …)` lower clamp, and the governor
carried an owner-set minimum of two beside them. Both are retired, by two
separate repository-owner rulings quoted verbatim below so this record carries
its own provenance.

**Amendment 1 — the committee floor (2026-08-11 owner session ruling 4**,
recorded on issue #593, comment of 2026-08-12T01:43Z**)**

> **Single-key attest-loop: accepted** with a rate-limited explanatory log;
> two-key committees are the standing test configuration. **The 1-of-1
> prohibition is lifted at both layers:** governor floors become `ceil(2n/3)`
> with n ≥ 1, bootstrap warns instead of rejecting, F04 §4 amended in lockstep
> with the Q63 gate that re-reads it; governor hash change rides #579.

**Amendment 2 — the owner-set minimum (2026-08-13 owner ruling, in-session,
recorded on #602)**

> **Option B — the owner-set minimum drops to 1.** A genuinely single-key
> deployment bootstraps end-to-end with warnings only; single-key governance
> rotation becomes representable and is ACCEPTED behavior by owner decision.

What the amendments change, and what they deliberately do not:

- The floor differs from the retired `max(2, ⌈2n/3⌉)` at exactly one set size,
  n = 1, where it is now 1 instead of 2. For every n ≥ 2 the two-thirds ceiling
  is already ≥ 2, so no other set size moves.
- A one-member DA committee attesting 1-of-1 is therefore representable, which
  is the single-key attest loop the first ruling accepted. Two-key committees
  remain the standing test configuration, and the single-key shape carries a
  rate-limited explanatory log in the attesting node.
- Under the second ruling the owner set follows the same shape. The governor no
  longer declares an owner-set minimum at all: at a minimum of one the check
  could not fail, because the sorted-unique length walker already aborts on an
  empty set, so the non-emptiness refusal is structural and the redundant guard
  is deleted rather than kept vacuous.
- Single-key _governance_ — one key rotating the committee and both governed
  thresholds on its own signature — is therefore representable, and is accepted
  behaviour by owner decision rather than an oversight. It is pinned positively
  by test, not merely permitted by the absence of a rejection.
- What no ruling licensed, and what still refuses: an owner set of zero, a
  threshold of zero at any set size (the floor is ≥ 1 everywhere), a threshold
  above its own set size, and any malformed or unsorted set encoding.
- Consumers in lockstep with this section: the on-chain floor
  (`onchain/aiken/validators/da-params-governor.ak`), its off-chain twin
  (`demo/midgard-sdk/src/da-attestation.ts`), and the Q63 gate
  (`demo/scripts/verify-canonical-v1-q63-da-governor-safety.mjs`), which
  re-reads the two floor rows above at their exact line numbers and pins the
  whole floor table by digest.

## 5. Resource ceilings and hardware floors

### 5.1 Local acceptance-topology ceilings (C80, ACCEPTED — this Goal only)

These are enforced _containment caps_ for the bounded target-testnet
acceptance run on the owner's workstation. They exist to keep the acceptance
topology from consuming the host and to prove the workload is boundable. They
are deliberately small and are **not** hardware requirements, sizing
guidance, or a performance claim; W46 and the readiness document must never
present them as production specs.

| Container class           | Memory                     | CPU | PIDs |
| ------------------------- | -------------------------- | --- | ---- |
| midgard-node              | 8 GiB                      | 4   | 512  |
| DA committee node (each)  | 4 GiB                      | 2   | 256  |
| midgard-watcher           | 4 GiB                      | 2   | 256  |
| Postgres (each)           | 4 GiB                      | 2   | 256  |
| Whole acceptance topology | ≤ 28 GiB / ≤ 14 vCPU total |     |      |

### 5.2 Production hardware floor (ACCEPTED — owner-directed 2026-07-29)

For production operation of a high-throughput L2 node, the minimum
recommended hardware is:

| Role                                         | Floor                                                                                                             |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
| DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

This floor is owner-set and ACCEPTED. W46 operational documentation and
`public_testnet_readiness.md` must carry it verbatim; C86 bounded-stress
results refine the non-node role sizing but cannot lower the node floor.

## 6. Acceptance-window check (`GOAL_SPEC.md` §7 F04)

With the 1 h availability/attestation deadlines, 30-block finality, and
journal-resumable parallel drills (C83/Q57 single-execution rule), the
complete C83–C87 sweep is planned ≤ 48 h. Any value change that breaks this
bound or a §3.3 threshold reopens this record.

## 7. Accepted values and raise-only measurement triggers

- C74 measures the worst-case prover cost. If the public reward is less than
  3× that measurement, or either profile's reward does not exceed its measured
  end-to-end prover cost, the affected reward is raised and this record plus
  all consumers are reopened; measurement cannot lower an accepted reward.
- W31 computes complete-sweep fee, collateral, minimum-ADA, and retry
  headroom. Its operator-wallet base is exactly `100_000_000_000` lovelace for
  public launch and `900_000_000` lovelace for bounded acceptance, plus that
  profile-independent headroom. It may raise wallet floors but cannot alter
  bond allocation or select a different economics profile.
