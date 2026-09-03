# 0002 — Canonical V1 Goal economics and margin decision record (F04)

- **Status:** ACCEPTED. CG5 may bind these values into the release identity
  after the named consumers pass. Amended 2026-08-31: §2.4 superseded by
  §2.4a (claim registry removed; concurrent fraud proofs permitted).
- **Owner/approver:** repository owner (Philip DiSarro).
- **Consumers:** Q53, Q54, Q61, Q63, W04, W12, W31, C74, C80, and W46 (production
  hardware floor, §5.2) (`GOAL_SPEC.md` §3.3, §7). No task may invent a value
  this record owns.

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

### 2.1 Public preprod launch economics

These are the values for the public preprod deployment opened once the
system is feature-complete. They are owner-set and not provisional.

| Parameter                     | Value                                                  | Rationale                                                                                                                                       |
| ----------------------------- | ------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `slashing_penalty`            | 25,000 ADA                                             | Penalty component of the forfeited bond.                                                                                                        |
| `fraud_prover_reward`         | 75,000 ADA                                             | ≥ 7.5× the prover-cost ceiling below; makes proving strongly profitable.                                                                        |
| `inactivity_slashing_penalty` | 10,000 ADA                                             | Strictly less than `slashing_penalty`, preserving the env constraint that a partially slashed operator still funds the full prover reward.      |
| `required_bond`               | `slashing_penalty + fraud_prover_reward` = 100,000 ADA | Env formula preserved exactly; a fraudulent operator forfeits the full 100k bond, so fraud is unprofitable unless the expected gain exceeds it. |
| Prover-cost ceiling           | 10,000 ADA                                             | Planning bound for full interactive-game fees/collateral; C74 must measure the path and the reward must stay ≥ 3× the measured cost.            |
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

C80 must fail closed until W31 computes the complete headroom and the correctly
credentialed operator, prover/watcher, DA, collateral, and fee wallets are
funded to their resulting floors. Funding evidence must be gathered from the
live target deployment; no dated balance observation is a reusable readiness
claim. Existing role-wallet funds are not silently reassigned or counted
across credentials.

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
- The reward can be paid only within a `RemoveFraudulentBlockHeader`
  transaction for that header (the bond-consuming slashing arms;
  successor-pruning transactions that carry the slash pay the reward;
  `OperatorAlreadySlashed` and target-block-removal-without-bond pay none). A
  path that pays no reward also does not levy the same penalty again.

The public-profile full/partial allocations are therefore
`100_000_000_000 = 75_000_000_000 + 25_000_000_000` lovelace and
`90_000_000_000 = 75_000_000_000 + 15_000_000_000` lovelace. The bounded
acceptance allocations are `900_000_000 = 400_000_000 + 500_000_000` and
`800_000_000 = 400_000_000 + 400_000_000` lovelace.

### 2.4 Duplicate-token and duplicate-reward prevention (SUPERSEDED 2026-08-31 by §2.4a)

> **This subsection is retained for history only. Its claim-registry
> requirement was reversed by the owner on 2026-08-31; see §2.4a for the
> governing rule.**

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

### 2.4a Concurrent fraud proofs are permitted; the bond is the reward boundary (ACCEPTED 2026-08-31)

Duplicate fraud-proof activity against the same fault is **permitted, not
prevented**. Independent provers may run concurrent computation threads
against the same `(fraud category ID, fraudulent header hash)`, and the
resulting fraud-proof success tokens may co-exist indefinitely.

The single boundary that matters is economic, and it already exists
independently of any claim bookkeeping: the reward is payable only out of the
accused operator's bond, the bond lives in that operator's singleton
directory node, and that node is consumed exactly once. The first pruning
transaction that carries the slash pays the one reward; every later path
reaches `OperatorAlreadySlashed` or a no-bond arm and pays none (§2.3, §2.5).
Extra fraud-proof tokens for the same header are therefore harmless but
unrewarded — they authorize removal of a block that is already removed.

Consequently the claim registry is removed from the protocol. Its
`MIDGARD_CLAIM_REGISTRY` hub-policy token, its validator, its Merkle root, and
the `OpenClaim`/`CloseClaim`/`CancelClaim` couplings in computation-thread
`Init`/`Success`/`BurnForCancellation` no longer exist. The hub one-shot policy
now mints exactly two assets, `MIDGARD_HUB_ORACLE` and
`MIDGARD_CORRECTION_LOCK`.

Rationale for the reversal:

- The second idempotency boundary §2.4 claimed was not independent of the
  first in any way that changed an outcome. It suppressed duplicate _tokens_,
  never duplicate _rewards_ — those were already impossible.
- The registry made `Init` a globally serialized, permissioned step while the
  expensive `Continue` steps stayed permissionless. A prover could open a claim
  for a real fault and abandon it, and because a live claim had no timeout and
  only its opener could `Cancel`, no other prover could open one. Requiring
  registry consensus to start a fraud proof is strictly worse for censorship
  resistance than allowing redundant proofs.
- Redundancy is the desired property for a fraud-proof system: more independent
  provers racing the same fault raises the probability the fault is proven at
  all, which is the property the protocol actually depends on.

The correction lock is unaffected and remains the mutual-exclusion mechanism
for multi-transaction state-queue corrections.

### 2.5 Current implementation boundary for Q53

Exact fraud-prover reward routing is implemented in the current state-queue
validator and removal builder:

- the fraud-proof datum supplies the immutable prover credential;
- a bond-consuming slash with a non-zero compiled reward must pay exactly that
  ADA-only amount to the prover's enterprise address and require the prover's
  extra signature;
- zero-reward profiles omit the impossible zero-lovelace output; and
- an already-slashed/no-bond arm cannot pay another reward.

Q53 is still not complete:

- `onchain/aiken/env/default.ak` and `env/testnet.ak` still compile the bond,
  slash, inactivity-slash, and reward values as zero;
- operator registration and slash accounting still accept lower-bound rather
  than exact bond/fee preservation, so the complete tranche conservation rules
  in §2.3 are not yet enforced;
- the accepted public and bounded profiles have not been deployed and measured
  through live/preprod acceptance.

Duplicate terminal mints are no longer an open item: under §2.4a they are
permitted, and the single-consumption of the operator's directory node is the
whole reward boundary.

The settlement remainder helper and unrelated SDK penalty constants are not
economics authorities. Until the remaining items above close, an F04-approved
profile must not be described as deployed or economics-complete.

## 3. Finality, retries, deadlines (status per row)

| Parameter                                                        | Value                                                                                                                                                                                                                      | Rationale                                                                                                                                                                                                                     |
| ---------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `finalityDepth` (local_node and external_providers)              | 30 blocks (~10 min Preprod) — **ACCEPTED** for testing and, conditionally, for public launch                                                                                                                               | Within watcher config bounds (1–2,160). The public-launch acceptance is conditional on the automated deep-rollback requirement below.                                                                                         |
| Cardano security parameter `k` (maximum credible rollback depth) | 2,160 blocks (Shelley genesis `securityParam`, mainnet and Preprod; ≈ 12 h at mainnet density)                                                                                                                             | Recorded fact, not a choice. The watcher config maximum (`finalityDepth`/`rollbackDepth` max `2_160`) already matches it.                                                                                                     |
| Automated deep-rollback handling — **ACCEPTED owner condition**  | A rollback deeper than `finalityDepth` but within `k` triggers automated W13 rewind/replay recovery plus W33 submission reconciliation and an explicit incident record; verification resumes without manual state surgery. | This automation is the condition under which `finalityDepth` 30 is acceptable for public launch: shallow finality for latency, full-`k` automated recovery for safety. W44 must include a deeper-than-finality rollback case. |
| Rollback handling below `finalityDepth`                          | pending-state rewind (W13)                                                                                                                                                                                                 | Spec §3.1.8.                                                                                                                                                                                                                  |
| Submission retry budget                                          | 5 attempts, exponential backoff capped at 120 s                                                                                                                                                                            | Bounded by §3.3 maturity fit; W33 reconciles ambiguity before any retry.                                                                                                                                                      |
| DA availability-challenge response deadline                      | 3,600,000 ms (1 h)                                                                                                                                                                                                         | Fits drill inside the acceptance window; retention makes longer response unnecessary.                                                                                                                                         |
| `da_attestation_timeout` (Q61)                                   | 3,600,000 ms (1 h)                                                                                                                                                                                                         | A live committee attests in seconds; 1 h cannot trigger accidentally yet keeps the head-of-line unblock drillable and the queue live. Timeout removal does not slash (D-L1 recommendation).                                   |
| DA retention (`RETENTION_DAYS`, `minimumRetentionDays`)          | 15 days                                                                                                                                                                                                                    | ≥ maturity (7 d) + worst-case proof time + margin; matches `LIBP2P_DA_MIN_RETENTION_DAYS = 15`.                                                                                                                               |

## 4. DA-governor floors (Q63, ACCEPTED)

- `da_threshold` ≥ ⌈2·committee_len/3⌉ for committee_len ≥ 1; the governor
  rejects any update below the floor.
- `update_threshold` ≥ ⌈2·owner_len/3⌉ for owner_len ≥ 1; the governor holds no
  separate owner-set minimum, so a lone owner governs at update_threshold 1.
- Mid-flight committee rotation must leave partially signed attestations
  rescuable/refundable (Q63 acceptance).

One-member committee and owner sets are valid with threshold one. Two-key
committees remain the standing test configuration, while single-key bootstrap
emits a rate-limited explanatory warning. Empty sets, zero thresholds,
thresholds above their set size, and malformed or unsorted set encodings
reject.

The on-chain authority is
`onchain/aiken/validators/da-params-governor.ak`; its off-chain twin is
`demo/midgard-sdk/src/da-attestation.ts`. Focused parity and rotation coverage
lives in `demo/midgard-sdk/tests/da-governor-safety.test.ts` and
`demo/midgard-sdk/tests/da-attestation-rotation.test.ts`.

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

### 5.2 Production hardware floor (ACCEPTED)

For production operation of a high-throughput L2 node, the minimum
recommended hardware is:

| Role                                         | Floor                                                                                                             |
| -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
| DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

This floor is owner-set and ACCEPTED. W46 operational documentation and
`docs/public_testnet_readiness.md` must carry it verbatim; C86 bounded-stress
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
