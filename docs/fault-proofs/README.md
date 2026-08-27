# Midgard Fault-Proof System — Live Documentation

> **Status:** Midgard is pre-launch (in development). This directory is the single
> source of truth for the state of the fault-proof (historically "fraud-proof") subsystem:
> what is delivered, what is functional, what is missing-but-documented, and what is
> missing-and-undocumented. It is intended to be kept **live** — updated as code lands.

Last full audit: **2026-07-10**, against branch `tx-validation` (HEAD `269bf6b3`)
plus its contemporaneous working tree. Reconstructed on `tx-validation` HEAD
`55afdc54`; paths were reconciled to that clean base, but line anchors must be
rechecked when implementing an item.

Current-tree reconciliation: **2026-08-04**, after Q13 commit `823b2d16`.
Q00/Q02/Q03 and Q13 are complete in their assigned scope: all twelve compiled
standalone families use the native V1 counted-root binding, and Q13 supplies
the `input-no-idx` prepare/submit/CLI/emulator lifecycle. The current catalogue,
`submit-init`, and manifest inspector each enumerate the same **eight**
categories. This is an inventory refresh, not a launch claim: Q14–Q20,
Q49-L298/Q49-L302, the missing-family/catalogue work, Q50+, economics, availability,
and preprod gates remain open.

Terminology note: these were historically called **fraud proofs**. Public-facing
documentation now generally says **fault proofs**, while the clean source tree still
contains historical `fraud-proof` path names. They are the same mechanism. This
directory uses "fault proof" throughout and preserves literal identifiers as they
appear in code/spec (for example the `fault_proof` token and
`RemoveFaultyBlockHeader`).

---

## What this directory contains

| Document                                         | Purpose                                                                                                                                                                               |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`architecture.md`](architecture.md)             | How the system works: catalogue, computation threads, tokens, the state-queue removal + slashing payoff path, trust assumptions.                                                      |
| [`catalogue-status.md`](catalogue-status.md)     | Per-proof-type delivery/functionality tracker (the 12 implemented types + generic machinery). The "what is delivered / what is functional" ledger.                                    |
| [`coverage-matrix.md`](coverage-matrix.md)       | First-principles enumeration of every way a state commitment can be faulty, mapped to a proof (or a gap). The "what is missing" analysis, including adversarial fund-theft scenarios. |
| [`onchain-reference.md`](onchain-reference.md)   | Code map of the Aiken implementation (`onchain/aiken`), with `file:line` anchors.                                                                                                     |
| [`offchain-reference.md`](offchain-reference.md) | Code map of the TypeScript SDK / CLI / watcher (`demo/*`), with `file:line` anchors.                                                                                                  |
| [`testing-status.md`](testing-status.md)         | Test coverage and end-to-end status (emulator vs preprod vs mainnet).                                                                                                                 |
| [`execution-plan.md`](execution-plan.md)         | The plan to reach **comprehensive** coverage: for any faulty tx in a state commitment, a successful fault proof can be submitted and the commitment invalidated.                      |

---

## Executive summary

**The generic machinery is real and wired end-to-end.** A concluded proof genuinely
mints a permanent `fault_proof` token, and the state-queue `RemoveFaultyBlockHeader`
path genuinely consumes that token to remove the faulty header and slash the operator.
The catalogue → computation-thread state-machine → fault-proof-token → removal pipeline is
coherent and deployable. See [`architecture.md`](architecture.md).

**But the core security invariant does _not_ yet hold.** The spec (`C-considerations/1-protocol-invariants.tex`,
"Invalid blocks") requires that _for every violation type, a proof can be constructed that
always succeeds against a block genuinely containing that violation_. Today that is false:
whole rule families that the spec itself defines as fault have **no working verifier**, and
several are directly exploitable for **fund theft** in an adversarial setting. See
[`coverage-matrix.md`](coverage-matrix.md).

### Status at a glance (the four buckets the audit was asked to produce)

**1. Delivered _and_ functional** (real logic, compiles, emulator-proven where noted):

- Generic machinery: catalogue, computation-thread minting policy (`Init`/`Success`/`BurnForCancellation`), step transition helpers, permanent fault-proof token, state-queue removal + operator slashing wiring, Plutarch MPF membership/non-membership (`phas`/`pexcludes`) primitives, counted/domain-tagged roots.
- 10 of 12 proof types with real verification logic: `zero-input`, `no-input`, `double-spend`, `input-no-idx`, `invalid-range`, `invalid-signature`, `missing-native-script-tx`, `missing-signature`, `no-reference-input`, `withdrawn-reference-input`.
- The `transition-trace` state-transition engine (9 top-level fault families: boundary, link, event-to-step, source-membership — incl. its phase-mismatch sub-variant — invalid one-step transition, duplicate-event, count, omitted-due-L1-event, out-of-window).
- The transition-trace retained-DA prepare CLI, strict proof-submit CLI,
  complete omitted/out-of-window/count Aiken sub-variant matrix, and emulator
  journeys through real finals 0/3/6 and faulty-block removal.
- Canonical V1 valid forced transactions use the same accepted ledger-delta
  validation claim as normal transactions; invalid forced transactions use an
  exact rejected no-op. Wrong verdicts, wrong roots, and either source-phase
  misclassification direction are represented. Concrete validator-hash-bound
  release evidence remains pending.
- Q13 adds the full `input-no-idx` lifecycle: canonical-evidence preparation,
  `submit-init`, four step commands (including resumable fold), inspection,
  and an emulator chain through faulty-block removal. Family-closure status is
  tracked individually in [`catalogue-status.md`](catalogue-status.md).
- The positional deployment catalogue has **eight** categories:
  `doubleSpend`, `nonExistentInput`, `nonExistentInputNoIndex`, `invalidRange`,
  `transitionTrace`, `zeroInput`, `validationTraceDispute`, and
  `daHashPreimage`. `submit-init` and `inspect-contracts` use the same eight.
  This is only the initial §9.1 launch-scope inventory; Q50/Q55 must still
  settle the final enabled routes.

**2. Delivered but _not_ functional** (present but stubbed/inert/disabled):

- Slashing **economics** — `slashing_penalty`, historical source identifier
  `fraud_prover_reward`, `required_bond`, and `inactivity_slashing_penalty` are all
  `0` in `env/default.ak` and `env/testnet.ak`. A successful proof slashes and
  rewards nothing. The canonical challenge and bond-hold maturity is seven
  days; it is compiled once in `ledger-state.ak` rather than selected by an
  environment.
- `transition-trace` value/authorization semantics — the L2 one-step verifier authenticates the _shape_ of the UTxO delta but never checks value conservation or spend authorization (see bucket 4).

**3. Missing but documented** (spec/gap-reports define it as fault; no working verifier):

- Value conservation (`VALUE-NOT-PRESERVED`), required-signer-set correctness
  (`MISSING-REQ-SIGNER-*`, `NON-REQ-SIGNER`), spend-side withdrawn/double-withdraw,
  reference-input-no-idx, missing-native-script-utxo, native-script-invalid, min-ada,
  and network-id. Q24/Q25 establish executable structural N/A for ADA minting and
  negative output value.
- Fabricated deposit / fabricated withdrawal (spec asserts "detectable" but provides no construction).
- Mis-tagged (valid→invalid) withdrawal proof.
- Remaining family production registration and missing-family routes remain
  open despite the complete native-V1 binding.
- Preprod end-to-end (an operator-local 2026-05-08 report recorded a canonical-root
  mismatch, but it is intentionally untracked and predates the counted-root work of PR
  #458 and the MPF rewrite of the proof builders; readiness remains unconfirmed until a
  new, publishable preprod run; see [`testing-status.md`](testing-status.md)).
- Autonomous fault-proof actuation and an unattended detect→prove→remove acceptance
  drill. Watcher ingestion, indexing, finality, rollback, and durable-state foundations
  now exist, but they do not yet close that end-to-end acceptance gap.

**4. Missing and undocumented** (no working proof _and_ no clear spec construction):

- Generic phase-2 (Plutus/`MidgardV1`) script-failure and minting-policy-unsatisfied proofs — the spec explicitly declines to claim these are possible today (`7-phase-two-validation/3-fault-proofs-involved.tex`).
- Maximum transaction/value/reference-script size rules (spec sections are commented out) — and, more fundamentally, **provability under adversarial sizing**: no family except double-spend has a worst-case argument that its evidence fits L1 tx-size/ex-unit limits (coverage-matrix §11b).
- Non-ADA **mint authorization** (no rule for disputing an unauthorized token mint) and **output well-formedness** (malformed outputs committed into `utxos_root`).
- Event **content fidelity** for deposits/withdrawals (an event whose value/address misstates the real L1 UTxO — distinct from the fabricated-event existence case).
- On-chain remedy for **data unavailability** after a DA attestation (no fault proof or
  rollback). Q54 now enforces the retention window; the committee-pruner residual is
  routed to Q58 rather than reopening Q54.
- Cross-operator descendant rollback — `RemoveFaultyBlocksLink` requires the descendant's own `operator_vkey` to equal the faulty block's operator (`onchain/aiken/validators/state-queue.ak:661`), so with scheduler rotation the cascade deadlocks; the adjacent comment (`:633-636`) documents the opposite intent.

See [`coverage-matrix.md`](coverage-matrix.md) for the class-by-class table and severity ranking, and [`execution-plan.md`](execution-plan.md) for how to close all of it.

---

## Keeping this documentation live

This directory is meant to track reality, not to become another stale plan. Maintenance protocol:

1. **When a proof type changes status** (stub → functional, new type added, branch enabled,
   category registered), update the row in [`catalogue-status.md`](catalogue-status.md) and
   the corresponding status cell (✅/🔶/🟠/📄/❌) in
   [`coverage-matrix.md`](coverage-matrix.md) in the _same PR_.
2. **When a gap is closed**, move it from the relevant bucket above and tick the matching
   work item (`W-*`/`D-*` ID) in [`execution-plan.md`](execution-plan.md), appending a dated
   line to that plan's §13 implementation record with the landing commit and evidence.
3. **When the audit is re-run**, bump the "Last full audit" date/commit at the top of this
   README and of each reference doc, and re-check the `file:line` anchors (they drift).
4. **Do not** let public/marketing messaging claim "full fault-proof readiness" until
   [`coverage-matrix.md`](coverage-matrix.md) shows no remaining 📄/❌ row in the
   fund-theft severity tier (§12) and [`testing-status.md`](testing-status.md) shows at
   least one family closed end-to-end on preprod. This mirrors the standing guidance in
   `../../public_testnet_readiness.md`.

## Related documents (outside this directory)

- `technical-spec/4-proof-protocol/` — the normative spec of catalogue, tokens, computation threads.
- `technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex` — the formal violation taxonomy and per-violation proof constructions.
- `technical-spec/7-phase-two-validation/3-fraud-proofs-involved.tex` — the spec's own admission that generic phase-2 proofs are unbuilt.
- `demo/midgard-node/docs/TRANSITION_TRACE_COMMITMENTS.md` — the transition-trace architecture.
- `demo/midgard-watcher/midgard-watcher-architecture.md` — the autonomous challenger
  design and boundary for the implemented watcher foundation.
- `../../public_testnet_readiness.md` — the launch-gate readiness tracker.
