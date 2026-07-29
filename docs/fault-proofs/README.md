# Midgard Fault-Proof System — Live Documentation

> **Status:** Midgard is pre-launch (in development). This directory is the single
> source of truth for the state of the fault-proof (historically "fraud-proof") subsystem:
> what is delivered, what is functional, what is missing-but-documented, and what is
> missing-and-undocumented. It is intended to be kept **live** — updated as code lands.

Last full audit: **2026-07-10**, against branch `tx-validation` (HEAD `269bf6b3`)
plus its contemporaneous working tree. Reconstructed on `tx-validation` HEAD
`55afdc54`; paths were reconciled to that clean base, but line anchors must be
rechecked when implementing an item.

Documentation and top-level conclusions revalidated **2026-07-22** against
the PR #461 production-readiness tree. The zero-input family is now
catalogue-registered, bound to native-v1 counted roots, CLI-complete, and
emulator-proven through faulty-block removal. Its preparer requires the authoritative
header `transactions_root` and fails closed on a mismatch. The remaining catalogue,
binding, fee, economics, DA-remedy, and system-wide preprod-acceptance gaps remain.
This was not a replacement for the full line-by-line audit, so historical line
anchors remain advisory.

Update **2026-07-27** (branch `fp/no-reference-input`): the `no-reference-input`
family reached the same bar as zero-input — catalogue-registered (`noReferenceInput`),
ported to the native counted-root binding path, CLI-complete
(`prepare-no-reference-input` + `submit-no-reference-input-step-01..04`), and
emulator-proven through faulty-block removal. The status buckets below and the
`catalogue-status.md`/`coverage-matrix.md`/`testing-status.md` counts are updated
accordingly; all other rows still reflect the 2026-07-10 audit.

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
- Offchain tooling for 7 families (double-spend, invalid-range, non-existent-input, input-no-idx,
  transition-trace, zero-input, no-reference-input); all seven are **emulator-proven
  end-to-end** through faulty-block removal, although transition-trace remains
  library-only rather than CLI-wired.
- ⚠️ Reachability caveat: only **7 of the 12** proof types are registered in the
  deployment catalogue — the other 5 compile but cannot `Init` a computation thread
  against a deployed instance. See [`catalogue-status.md`](catalogue-status.md).

**2. Delivered but _not_ functional** (present but stubbed/inert/disabled):

- `min-fee` proof — its `get_min_transaction_fee` is a `0`-returning stub, making its decisive check unsatisfiable. Cannot conclude. (`onchain/aiken/validators/fraud-proofs/min-fee/step-02.ak:64,78-80`)
- Slashing **economics** — `slashing_penalty`, historical source identifier
  `fraud_prover_reward`, `required_bond`, and `inactivity_slashing_penalty` are all
  `0` in `env/default.ak` and `env/testnet.ak`. A successful proof slashes and
  rewards nothing. `maturity_duration` is likewise a dev value (`30` — i.e.
  ~30 ms — in both envs), so the on-chain challenge window is effectively zero.
- `transition-trace` `ValidForcedTransactionUnsupported` branch is hard-wired to `False` — valid-forced-transaction omission faults are unprovable via that path. (`.../transition-trace/proof.ak:1201`)
- `transition-trace` value/authorization semantics — the L2 one-step verifier authenticates the _shape_ of the UTxO delta but never checks value conservation or spend authorization (see bucket 4).

**3. Missing but documented** (spec/gap-reports define it as fault; no working verifier):

- Value conservation (`VALUE-NOT-PRESERVED`), ADA minting (`ADA-MINTED`), negative output value (`NEGATIVE-OUTPUT-VALUE`), required-signer-set correctness (`MISSING-REQ-SIGNER-*`, `NON-REQ-SIGNER`), spend-side withdrawn/double-withdraw, reference-input-no-idx, missing-native-script-utxo, native-script-invalid, min-ada, network-id.
- Fabricated deposit / fabricated withdrawal (spec asserts "detectable" but provides no construction).
- Mis-tagged (valid→invalid) withdrawal proof.
- Offchain tooling for 5 of the 12 already-implemented onchain types; CLI wiring for
  `transition-trace`.
- Preprod end-to-end (an operator-local 2026-05-08 report recorded a canonical-root
  mismatch, but it is intentionally untracked and predates the counted-root work of PR
  #458 and the MPF rewrite of the proof builders; readiness remains unconfirmed until a
  new, publishable preprod run; see [`testing-status.md`](testing-status.md)).
- Autonomous watcher/challenger (design docs exist; zero code).

**4. Missing and undocumented** (no working proof _and_ no clear spec construction):

- Generic phase-2 (Plutus/`MidgardV1`) script-failure and minting-policy-unsatisfied proofs — the spec explicitly declines to claim these are possible today (`7-phase-two-validation/3-fault-proofs-involved.tex`).
- Maximum transaction/value/reference-script size rules (spec sections are commented out) — and, more fundamentally, **provability under adversarial sizing**: no family except double-spend has a worst-case argument that its evidence fits L1 tx-size/ex-unit limits (coverage-matrix §11b).
- Non-ADA **mint authorization** (no rule for disputing an unauthorized token mint) and **output well-formedness** (malformed outputs committed into `utxos_root`).
- Event **content fidelity** for deposits/withdrawals (an event whose value/address misstates the real L1 UTxO — distinct from the fabricated-event existence case).
- On-chain remedy for **data unavailability** after a DA attestation (no fault proof, no rollback), and nothing binding retention windows to the on-chain challenge deadline.
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
- `demo/midgard-watcher/midgard-watcher-architecture.md` — the (unbuilt) autonomous challenger design.
- `../../public_testnet_readiness.md` — the launch-gate readiness tracker.
