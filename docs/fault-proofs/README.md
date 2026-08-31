# Midgard Fault-Proof System — Live Documentation

> **Status:** Midgard is pre-launch (in development). This directory is the single
> source of truth for the state of the fault-proof subsystem:
> what is delivered, what is functional, what is missing-but-documented, and what is
> missing-and-undocumented. It is intended to be kept **live** — updated as code lands.

Current-tree reconciliation: **2026-08-29**. The canonical V1 catalogue,
generic Init parser, shared runtime deployment table, node/core manifest
schemas, and contract inspection all enumerate **29** positional categories
(`00000000`–`0000001c`), ending with `networkId` (`0000001c`). The source-derived
catalogue root is
`c686373893084eff5efe51a52821055f994caa4c26a363df37ec97df23380b62`;
the inspection test's older pinned root has not yet been updated.
`transitionTrace` remains `00000004` with one route validator and eight
terminal validators. Every family step is a mandatory authenticated reference
script. The current generated testnet blueprint contains 510
validators and has SHA-256
`ad69e8f98e49e110864cb270dd6bb731caaf43357e8459827b1659124c890de8`.
This identity change requires a fresh genesis/redeployment; it is not a
migration or compatibility path. This remains an inventory refresh, not a
launch claim: autonomous watcher actuation, publishable preprod acceptance,
non-zero economics, unavailable-data recovery, and remaining rule-family gaps
are still open.

Terminology note: public-facing documentation says **fault proofs**, while
literal source paths and some protocol identifiers still say `fraud-proof` or
`fraudulent`. They are the same mechanism; this directory preserves literal
identifiers such as the `fault_proof` token and
`RemoveFraudulentBlockHeader`.

---

## What this directory contains

| Document                                         | Purpose                                                                                                                                                                               |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`architecture.md`](architecture.md)             | How the system works: catalogue, computation threads, tokens, the state-queue removal + slashing payoff path, trust assumptions.                                                      |
| [`catalogue-status.md`](catalogue-status.md)     | Per-proof-type delivery/functionality tracker (29 registered types plus generic machinery). The "what is delivered / what is functional" ledger.                                      |
| [`coverage-matrix.md`](coverage-matrix.md)       | First-principles enumeration of every way a state commitment can be faulty, mapped to a proof (or a gap). The "what is missing" analysis, including adversarial fund-theft scenarios. |
| [`onchain-reference.md`](onchain-reference.md)   | Code map of the Aiken implementation (`onchain/aiken`), with `file:line` anchors.                                                                                                     |
| [`offchain-reference.md`](offchain-reference.md) | Code map of the TypeScript SDK / CLI / watcher (`demo/*`), with `file:line` anchors.                                                                                                  |
| [`testing-status.md`](testing-status.md)         | Test coverage and end-to-end status (emulator vs preprod vs mainnet).                                                                                                                 |
| [`execution-plan.md`](execution-plan.md)         | The plan to reach **comprehensive** coverage: for any faulty tx in a state commitment, a successful fault proof can be submitted and the commitment invalidated.                      |

---

## Executive summary

**The generic machinery is real and wired end-to-end.** A concluded proof genuinely
mints a permanent `fault_proof` token, and the state-queue `RemoveFraudulentBlockHeader`
path genuinely consumes that token to remove the faulty header and slash the operator.
The catalogue → computation-thread state-machine → fault-proof-token → removal pipeline is
coherent and deployable. See [`architecture.md`](architecture.md).

**But the core security invariant does _not_ yet hold.** The spec (`C-considerations/1-protocol-invariants.tex`,
"Invalid blocks") requires that _for every violation type, a proof can be constructed that
always succeeds against a block genuinely containing that violation_. Today that is false:
whole rule families that the spec itself defines as fault have **no working verifier**, and
several are directly exploitable for **fund theft** in an adversarial setting. See
[`coverage-matrix.md`](coverage-matrix.md).

### Status at a glance

**1. Delivered _and_ functional** (real logic, compiles, emulator-proven where noted):

- Generic machinery: catalogue, computation-thread minting policy (`Init`/`Success`/`BurnForCancellation`), step transition helpers, permanent fault-proof token, state-queue removal + operator slashing wiring, Plutarch MPF membership/non-membership (`phas`/`pexcludes`) primitives, counted/domain-tagged roots.
- The 29 registered categories have real first-step routes; family-level tooling
  and emulator depth still vary and are tracked in `catalogue-status.md`.
- The current working tree also contains a two-step `network-id` family with
  strict SDK/evidence tooling and a passing focused emulator lifecycle through
  permanent mint and faulty-block removal. It is appended to the source
  catalogue as `networkId` (`0000001c`) and appears in node/core manifest
  schemas and the runtime deployment-entry mapping. The inspection logic
  derives the new root, but its static root assertion is still stale.
- The `transition-trace` state-transition engine (9 top-level fault families: boundary, link, event-to-step, source-membership — incl. its phase-mismatch sub-variant — invalid one-step transition, duplicate-event, count, omitted-due-L1-event, out-of-window).
- The transition-trace retained-DA prepare CLI, strict proof-submit CLI,
  complete omitted/out-of-window/count Aiken sub-variant matrix, and emulator
  journeys through real finals 0/3/6 and faulty-block removal.
- Canonical V1 valid forced transactions use the same accepted ledger-delta
  validation claim as normal transactions; invalid forced transactions use an
  exact rejected no-op. Wrong verdicts, wrong roots, and either source-phase
  misclassification direction are represented. Concrete validator-hash-bound
  release evidence remains pending.
- Q13 includes the full `input-no-idx` lifecycle: canonical-evidence
  preparation, `submit-init`, the direct four-step chain, inspection, and an
  emulator chain through faulty-block removal. Family-closure status is
  tracked individually in [`catalogue-status.md`](catalogue-status.md).
- The positional source catalogue has **29** categories. The canonical
  append block is `fabricatedDeposit` (`0000000b`), `fabricatedWithdrawal`
  (`0000000c`), `nativeScriptDecoding` (`0000000d`), `missingSignature`
  (`0000000e`), `missingNativeScriptTx` (`0000000f`),
  `withdrawnReferenceInput` (`00000010`), `canonicalDecodability`
  (`00000011`), `committedFieldShape` (`00000012`), `minFee` (`00000013`),
  `withdrawalMistag` (`00000014`), `doubleWithdraw` (`00000015`),
  `crossBlockDuplicateEvent` (`00000016`), `l2TxMistag` (`00000017`), and
  `withdrawnInput` (`00000018`), `valueNotPreserved` (`00000019`),
  `inputSetUniqueness` (`0000001a`), `mintAuthorization` (`0000001b`), and
  `networkId` (`0000001c`). Catalogue membership, generic `submit-init`, runtime
  deployment resolution, inspection, and node/core manifest schemas share this
  order.

**2. Delivered but _not_ functional** (present but stubbed/inert/disabled):

- Slashing **economics activation** — `slashing_penalty`, source identifier
  `fraud_prover_reward`, `required_bond`, and `inactivity_slashing_penalty` are all
  `0` in `env/default.ak` and `env/testnet.ak`. Exact prover identity, reward
  output, payout amount, and reward-bearing signer routing are enforced in the
  state-queue removal path, but the zero-valued profiles make the realized
  payout and deterrent zero. Claim-lock/idempotency and non-zero deployment
  values remain Q53 work. The canonical challenge and bond-hold maturity is
  seven days; it is compiled once in `ledger-state.ak` rather than selected by
  an environment.

**3. Missing but documented** (spec/gap-reports define it as fault; no working verifier):

- Standalone `min-ada`, `missing-native-script-utxo`, and
  `native-script-invalid` families. The locally proven network-id family still
  requires watcher actuation, current identity binding, and live evidence
  before it becomes a release claim. Q24/Q25 establish executable structural N/A for ADA minting
  and negative output value; Q32 establishes that required-signer-set needs no
  separate family because it reduces to the authenticated Signatures path.
- Family-specific manual CLI commands and watcher detection/proving adapters
  remain uneven even though the catalogue/deployment routes are registered.
- Reproducible preprod end-to-end acceptance remains absent. Readiness stays
  unconfirmed until a current run publishes its deployment identity,
  transactions, proof artifacts, removal result, and balance/state evidence;
  see [`testing-status.md`](testing-status.md).
- Autonomous fault-proof actuation and an unattended detect→prove→remove acceptance
  drill. Watcher ingestion, indexing, finality, rollback, and durable-state foundations
  now exist, but they do not yet close that end-to-end acceptance gap.

**4. Missing and undocumented** (no working proof _and_ no clear spec construction):

- Complete release coverage for Plutus/`MidgardV1` execution disputes. The CEK
  validation-dispute machinery exists, but retained-data reconstruction,
  concrete release measurements, and live acceptance remain incomplete.
- Complete **provability under adversarial sizing**. Several families have
  focused frontier and published-carriage suites, but the rule-by-rule
  proof-fit ledger is not complete (coverage-matrix §11b).
- Complete standalone coverage for every remaining native/Plutus authorization
  branch. Native-policy mint authorization and output well-formedness now have
  registered, emulator-proven families; missing UTxO script material,
  native-script-invalid, and the Plutus execution boundary remain separate.
- On-chain remedy for **data unavailability** after a DA attestation (no fault proof or
  rollback). Q54 now enforces the retention window; the committee-pruner residual is
  routed to Q58 rather than reopening Q54.
- State-correction release evidence — structural linked-list descendant
  authority, rotated-operator regressions, an exact finalized-transition
  record, transactional payload/event re-inclusion, the operator-node
  correction scheduler, and independent acceptance-evidence reconciliation now
  exist locally. Real-node concurrent execution and publishable preprod
  acceptance are still outstanding.

See [`coverage-matrix.md`](coverage-matrix.md) for the class-by-class table and severity ranking, and [`execution-plan.md`](execution-plan.md) for how to close all of it.

---

## Keeping this documentation live

This directory is meant to track reality, not to become another stale plan. Maintenance protocol:

1. **When a proof type changes status** (stub → functional or a category is
   added/registered), update the row in [`catalogue-status.md`](catalogue-status.md) and
   the corresponding status cell (✅/🔶/🟠/📄/❌) in
   [`coverage-matrix.md`](coverage-matrix.md) in the _same PR_.
2. **When a gap is closed**, move it from the relevant bucket above and update
   the matching work item (`W-*`/`D-*` ID) in
   [`execution-plan.md`](execution-plan.md).
3. **When the code is reconciled with these docs**, update the current-tree
   reconciliation date and re-check copied counts, identities, CLI names, and
   `file:line` anchors.
4. **Do not** let public/marketing messaging claim "full fault-proof readiness" until
   [`coverage-matrix.md`](coverage-matrix.md) shows no remaining 📄/❌ row in the
   fund-theft severity tier (§12) and [`testing-status.md`](testing-status.md) shows at
   least one family closed end-to-end on preprod. This mirrors the standing guidance in
   `../../public_testnet_readiness.md`.

## Related documents (outside this directory)

- `technical-spec/4-proof-protocol/` — the normative spec of catalogue, tokens, computation threads.
- `technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex` — the formal violation taxonomy and per-violation proof constructions.
- `technical-spec/7-phase-two-validation/3-fraud-proofs-involved.tex` — the
  normative phase-two fault-proof discussion; current implementation status is
  tracked in the coverage matrix.
- `demo/midgard-node/docs/TRANSITION_TRACE_COMMITMENTS.md` — the transition-trace architecture.
- `demo/midgard-watcher/midgard-watcher-architecture.md` — the autonomous challenger
  design and boundary for the implemented watcher foundation.
- `../../public_testnet_readiness.md` — the launch-gate readiness tracker.
