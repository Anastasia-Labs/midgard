# Execution Plan — Complete State-Correction Coverage

> **Status:** Active, launch-blocking plan. No production security claim may
> treat its unchecked fund-safety or proof-binding items as optional.
>
> **Last reviewed:** 2026-08-29 against the current working tree.

> **Consensus-surface decision:** The sole pre-launch surface is
> the exact canonical V1 tuple in
> [`../consensus-profile-v1.md`](../consensus-profile-v1.md). D-S1, D-S5, and
> D-S9 are resolved for V1. V1 retains mint/burn, scripts,
> redeemers, reference inputs/scripts,
> script credentials, protected outputs, observers, and effectful valid forced
> transactions. None may activate until its deterministic transition is
> independently disputable on L1 and its concrete witness fits the compiled
> proof envelope. Unknown or incomplete semantics fail closed.

> Target property:
> **for every transaction or state transition that violates Midgard's protocol rules,
> there is a defined evidence format, an L1 verification procedure, and a state-correction
> workflow that restores canonical state** — i.e., make the "Invalid blocks" invariant
> (`technical-spec/C-considerations/1-protocol-invariants.tex:28`) true in code, not just
> in prose. Work-item IDs here are referenced from [`coverage-matrix.md`](coverage-matrix.md).

## 1. Invariants and scope

In scope: all rows of the coverage matrix — transaction structure, inputs, reference
inputs, value accounting, authorization, validity intervals, native scripts, Plutus
execution, fees, event ordering, deposits, withdrawals, forced transactions, state roots,
transition traces, data availability, timing, state-queue correction. Out of scope:
settlement-resolution disputes (separate redeemer path, already wired), escape hatch,
throughput work.

Soundness guardrail: every new proof type must also come with negative tests showing it
**cannot** succeed against a valid block (invariant 2, `1-protocol-invariants.tex:19-25`).

## 2. Coverage matrix

[`coverage-matrix.md`](coverage-matrix.md) is the authoritative rule-by-rule ledger.
Summary of today's classification:

- ✅ Complete & verified locally/emulator: double-spend, no-input,
  invalid-range, zero-input, transition-trace, value-not-preserved,
  input-set-uniqueness, mint-authorization, and the other completed families
  recorded in `catalogue-status.md`.
- 🔶 Implemented, not fully verified: input-no-idx (Q13 local lifecycle is
  complete), invalid-signature, missing-signature, missing-native-script-tx,
  no-reference-input, withdrawn-reference-input, fabricated event families,
  native-script-decoding, field-shape/decodability, min-fee,
  withdrawal-mistag, double-withdraw, cross-block-duplicate-event,
  l2-tx-mistag, withdrawn-input, value-not-preserved,
  input-set-uniqueness, mint-authorization, and network-id. The source
  catalogue now has 29 categories (`00000000`–`0000001c`), all wired through
  the shared runtime deployment table and contract inspection with mandatory
  authenticated reference scripts. The inspection suite still pins the older
  catalogue root. Family-specific CLI, autonomous watcher actuation, and
  live acceptance remain uneven.
- 🟠 Partial: slashing economics activation/idempotency (routing exists;
  values are zero),
  state-correction live acceptance, and DA unavailable-data recovery.
- 📄 Documented but missing: standalone MIN-ADA,
  native-script-invalid, and missing-native-script-utxo. Q32 required-signer-set is
  structural N/A and reduces to
  Signatures.
  Q24/Q25 are executable structural N/A, and Q44
  da-hash-preimage is registered with its local lifecycle; Complete Plutus
  disputability is mandatory before canonical V1 activation.
- ❌ Required but unresolved: DA withholding-after-attestation remedy,
  remaining Q61 real-node/preprod acceptance, evidence-liveness/proof-fit work,
  and on-chain/off-chain codec-strictness divergence. Q00, Q60, Q62, Q63,
  cross-block duplicate events, and intra-tx input uniqueness are implemented.

## 3. Specification decision ledger

D-S1, D-S3, D-S5, D-S9, and D-S13 are resolved by the sole pre-launch
canonical V1 surface. Each independently revealed dynamic-field preimage must
fit strictly below the supported 16 KiB L1 proof envelope after its concrete
proof overhead, reference scripts are supported, values have no arbitrary
1 KiB sub-cap, and the effective transaction bound is derived from all
bounded dynamic fields plus fixed canonical fields. The exact V1 tuple and
all unknown or incomplete semantics fail closed.

Forced-order publication is necessarily staged. A one-shot verifier for all
nine maximum fields exceeds the L1 execution envelope
(45,154,331 memory / 14,905,078,582 CPU). Canonical V1 therefore publishes each
fragment first, then mints a deterministic compact receipt only after the
receipt policy references and verifies that fragment on L1. The final order
verifies nine receipts and records both reference lists. Fragment, receipt,
and order consumption requires exact order/receipt NFT burns. The measured
maximum receipt proof is 11,408,043 memory / 3,497,630,466 CPU and the
nine-receipt final verification is 3,204,296 / 1,246,065,449.

CEK graph availability is also staged but does not enlarge the order datum.
Before submitting an order, anyone may publish each typed content node at the
canonical V1 immutable material validator. The program envelope already commits
the graph roots/counts. Nodes scan and persist those self-authenticating
outputs, and a due forced transaction with incomplete material blocks the
canonical V1 build rather than being classified as an invalid script. The
authenticated `DeploymentManifestV1` identity makes this append-only
publication validator mandatory.

| ID        | Decision                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Blocks                                                                                    |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| **D-S5**  | **Canonical V1 decision:** scripts, redeemers, reference inputs/scripts, script credentials, protected outputs, and observers are required features. Activation remains blocked until every corresponding validation-machine instruction is semantic, L1-disputable, and proven to fit its compiled field-preimage envelope.                                                                                                                                                                                                                                              | W-C2, W-C4, W-C5, W-T8                                                                    |
| **D-S7**  | **Resolved:** the fabricated-deposit and fabricated-withdrawal families cover both nonexistent L1 events and content mismatch against an authentic event. Both are registered and locally emulator-proven; watcher/live evidence remains.                                                                                                                                                                                                                                                                                                                                 | W-C7 operational closure                                                                  |
| **D-S8**  | **Resolved:** use a standalone five-step single-party family, not transition-trace extension, because the trace proves consistency with the committed tag rather than the tag's truth. The family opens counted source/event/trace/pre-ledger/output evidence and recomputes both valid↔invalid directions, including exact `UnpayableWithdrawalValue`; both emulator polarities now pass through permanent mint and block removal. Registered as `withdrawalMistag` (`00000014`). See [`withdrawal-mistag-offchain-plan-v1.md`](withdrawal-mistag-offchain-plan-v1.md). | W-C7 preprod/watcher actuation                                                            |
| **D-S3**  | **Canonical V1 decision:** mint and burn remain in the consensus language. Canonical V1 must authenticate the mint field, enforce exact multi-asset conservation, and verify the corresponding native/Plutus policy authorization on L1.                                                                                                                                                                                                                                                                                                                                  | W-C2, W-C3, W-T8                                                                          |
| **D-S4**  | **Formula resolved:** `coins_per_utxo_byte * (160 + serialized_output_bytes)` is shared by the validation machine and TypeScript twin. A dedicated standalone min-ada family remains W-C5.                                                                                                                                                                                                                                                                                                                                                                                | W-C5                                                                                      |
| **D-S6**  | **Resolved:** the validation machine and registered standalone `minFee` family share `min_fee_lovelace_v1(min_fee_a, min_fee_b, canonical_tx_size)`.                                                                                                                                                                                                                                                                                                                                                                                                                      | W-C6 operational closure                                                                  |
| **D-S1**  | **Canonical V1 decision implemented:** no independent 8 KiB transaction or 1 KiB value cap. A field preimage is carried inline, by one raw publication UTxO, or by up to three certified chunks under the deterministic 14,336/15,148/32,768-byte ladder. Ledger-output and script-program envelopes constrain reference scripts without disabling them. The provisional inline boundary and family worst cases remain W-T8 release gates.                                                                                                                                | W-T8                                                                                      |
| **D-S9**  | **Canonical V1 decision implemented in source:** a valid forced transaction executes through the same accepted ledger-delta path as a normal transaction. An invalid forced transaction remains a proved no-op. Either operator misclassification is challengeable on L1. Activation remains closed until W-T8 produces concrete validator-hash-bound release evidence for all paths.                                                                                                                                                                                     | W-T8                                                                                      |
| **D-C1**  | **Resolved:** VALUE-NOT-PRESERVED uses a single-asset claim and bounded fold; the prover finds an unbalanced asset off-chain and the four-step family verifies only that asset's exact equation.                                                                                                                                                                                                                                                                                                                                                                          | complete                                                                                  |
| **D-DA1** | DA committee accountability: on-chain hash-preimage fault proofs (`6-transaction.tex:175`) and/or committee bonding — or a documented honesty assumption.                                                                                                                                                                                                                                                                                                                                                                                                                 | DA rows                                                                                   |
| **D-DA2** | Remedy for data withheld after attestation: committee bond + timeout rollback, Mithril-style certificate, or explicit trust assumption at launch.                                                                                                                                                                                                                                                                                                                                                                                                                         | W-C11                                                                                     |
| **D-DA3** | Q54 completed the authoritative retention window: seven-day maturity plus proof-time margin, 15-day deployment floor, manifest/config binding, pruning/readiness, and alerting. The deliberately inert committee-store pruner is a Q58/W-O7 residual.                                                                                                                                                                                                                                                                                                                     | Q58, W-O7                                                                                 |
| **D-E1**  | Exact prover identity, reward output, payout amount, and reward-bearing signer routing are implemented. Non-zero `required_bond`, `slashing_penalty`, `fraud_prover_reward`, and `inactivity_slashing_penalty`, plus duplicate-claim/reward idempotency, remain. Canonical maturity identity is resolved separately.                                                                                                                                                                                                                                                      | W-C10                                                                                     |
| **D-S10** | **Resolved locally:** `canonicalDecodability` and `committedFieldShape` prove malformed or shape-invalid committed field preimages; the validation machine independently rejects invalid outputs. Watcher/live evidence remains.                                                                                                                                                                                                                                                                                                                                          | operational closure                                                                       |
| **D-S11** | **Dedicated replay family implemented:** `crossBlockDuplicateEvent` proves duplicate L1-event application from authenticated confirmed-settlement evidence. The cross-cutting obligation to preserve every family's evidence for the full challenge window remains.                                                                                                                                                                                                                                                                                                       | evidence-lifetime audit; live acceptance                                                  |
| **D-S12** | **Resolved by Q60:** `CommitBlockHeader` requires header `end_time` to equal the commit transaction validity interval's inclusive upper bound; SDK and mutation controls enforce the same rule.                                                                                                                                                                                                                                                                                                                                                                           | complete                                                                                  |
| **D-S13** | **Resolved for canonical V1:** profile ID `midgard-consensus-v1`, protocol version 1, native transaction version 1, transition schema 1, protocol-info API 1, and `DeploymentManifestV1`. Unknown higher or lower identities fail closed; upgrades require a new profile and deployment.                                                                                                                                                                                                                                                                                  | —                                                                                         |
| **D-L1**  | **Resolved for unattested queue heads:** one hour after the commit-bound header `end_time`, permissionless correction prunes descendants and then removes the unattested head without slashing. Attestation, append, and correction are serialized by shared queue inputs. The independent user escape hatch remains unimplemented.                                                                                                                                                                                                                                       | W-C16 local implementation complete; real-node/preprod acceptance and escape hatch remain |
| **D-DA4** | **Resolved by Q62:** DA params re-derive the committee hash; apply references current governed params and requires the frozen committee hash and threshold to match.                                                                                                                                                                                                                                                                                                                                                                                                      | complete                                                                                  |
| **D-DA5** | **Resolved by Q63:** governed threshold floors and owner-set drain protection are enforced; stranded attestations have an exact rescue/refund/burn path.                                                                                                                                                                                                                                                                                                                                                                                                                  | complete                                                                                  |

Remaining spec reconciliation needing no design decision: add
WITHDRAWN-REFERENCE-INPUT to its parent rule's violation list. Descendant
operator equality is already removed in the current working tree.

## 4. On-chain work

| ID        | Work                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Depends on                                                  |
| --------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| **W-C1**  | **Completed in source:** 29 categories (`00000000`–`0000001c`) are wired through `FaultProofs`, the SDK order, generic Init, runtime deployment resolution, inspection, and node/core manifest schemas. Every family step is a mandatory authenticated reference script. The static inspection root pin still needs regeneration. This does not mount autonomous watcher detectors/provers.                                                                                                                | root-pin reconciliation                                     |
| **W-C2**  | **Completed locally:** registered `valueNotPreserved` (`00000019`) and `mintAuthorization` (`0000001b`) implement the standalone value and native-policy authorization paths. ADA-MINTED and NEGATIVE-OUTPUT-VALUE are Q24/Q25 structural N/A. Plutus policy execution remains on the interactive validation-dispute path.                                                                                                                                                                                 | live evidence                                               |
| **W-C3**  | Registered withdrawn/double-withdraw/missing-signature families are implemented and emulator-proven. Q32 required-signer-set is structural N/A. MISSING-NATIVE-SCRIPT-UTXO and standalone NATIVE-SCRIPT-INVALID remain.                                                                                                                                                                                                                                                                                    | —                                                           |
| **W-C4**  | **Completed locally:** `referenceInputNoIdx` (`00000009`) and `inputSetUniqueness` (`0000001a`) cover reference-index range, duplicate spend/reference entries, and spend/reference overlap.                                                                                                                                                                                                                                                                                                               | live evidence                                               |
| **W-C5**  | Min-ada proof + helper.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | D-S4                                                        |
| **W-C6**  | **Completed and registered:** standalone `minFee` (`00000013`) and the validation machine share `min_fee_lovelace_v1`, using the authenticated canonical full-transaction size. Live evidence and autonomous watcher actuation remain.                                                                                                                                                                                                                                                                     | D-S6 resolved; Q57/QG3 remain                               |
| **W-C7**  | Registered `fabricatedDeposit` (`0000000b`), `fabricatedWithdrawal` (`0000000c`), and `withdrawalMistag` (`00000014`) are implemented and emulator-proven with mandatory reference scripts. Spec wording, preprod/live evidence, and autonomous watcher actuation remain.                                                                                                                                                                                                                                  | D-S7; D-S8 resolved                                         |
| **W-C8**  | **Completed in canonical V1 source:** valid forced transactions apply the authenticated terminal ledger frontier through the generic accepted-transition claim; invalid forced transactions remain exact no-ops and both source-phase mismatch directions are challengeable. Concrete proof-fit/release evidence remains W-T8.                                                                                                                                                                             | D-S9; W-T8                                                  |
| **W-C9**  | **Implemented in the current working tree:** descendant pruning uses authenticated structural ancestry rather than operator equality, with rotated-operator regressions. The SDK derives an exact digest-bound finalized correction transition, and the operator node transactionally restores journaled transactions and L1 events after confirmed correction while refusing conflicting event assignments. Real-node concurrent/preprod integration remains.                                             | W-T6                                                        |
| **W-C10** | **Partial:** exact on-chain prover/reward output/amount/signer routing is implemented. Non-zero parameters and claim-lock/idempotency remain.                                                                                                                                                                                                                                                                                                                                                              | D-E1                                                        |
| **W-C11** | DA remedy per D-DA2 (committee bond/slash validators, timeout rollback path, or none).                                                                                                                                                                                                                                                                                                                                                                                                                     | D-DA2                                                       |
| **W-C12** | **Completed locally:** the two-step network-id family is `networkId` (`0000001c`) in the catalogue, generic Init, runtime deployment table, inspection, classifier, and node/core manifests, with focused SDK/evidence/emulator mint→removal/cancel coverage. Re-pin the derived catalogue root and add watcher/live evidence.                                                                                                                                                                             | root pin; W-O3/W-T4                                         |
| **W-C13** | **Completed native-V1 binding port.** All transaction-proof step-01 validators call `pass_native_tx_to_next_step`; it unwraps/authenticates the counted root and proves membership over raw native CBOR. `transition-trace` uses its separate canonical V1 trace/claim route. `git grep verify_tx_in_state_queue_node -- onchain` is empty. This does not replace family-specific §9.1 closure work; Q20 min-fee now has its own local closure.                                                            | Q00 complete; remaining family/Q50+ work tracked separately |
| **W-C14** | **Partially completed:** standalone value preservation, native-policy mint authorization, and network-id are registered; Q32 reduces to Signatures. Min-ada, missing UTxO script material, and native-script-invalid retain their own rows. All standalone transaction families use `pass_native_tx_to_next_step`.                                                                                                                                                                                         | remaining family rows                                       |
| **W-C15** | **Completed by Q62:** DA params re-derive the committee hash and apply requires current governed committee/threshold equality.                                                                                                                                                                                                                                                                                                                                                                             | —                                                           |
| **W-C16** | **Implemented locally:** one-hour, no-slash permissionless correction for an unattested head; descendants are structurally pruned before terminal head removal. The append/attestation race is fenced on-chain. SDK builders and finalized-transition derivation, resumable CLI/journal, operator-node scheduler, transactional payload/event re-inclusion, watcher-side read-only timeout observation, ten recovery tests, and twelve focused Aiken controls exist. Real-node/preprod acceptance remains. | W-T6                                                        |

## 5. Off-chain evidence construction & workflow

| ID       | Work                                                                                                                                                                                                                                                                                                                                     | Depends on                                     |
| -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| **W-O1** | Prepare+submit tooling for remaining untooled types. Catalogue/generic Init registration is complete, and min-fee/missing-signature/missing-native-script-tx/withdrawn-reference-input have library modules; family-specific CLI and single-command workflows remain uneven.                                                             | each remaining family's closure dependencies   |
| **W-O2** | **Completed locally:** `prepare-transition-trace` wraps retained-DA reconstruction/detection/witnesses, and `submit-transition-trace-proof` drives the authenticated route→selected-final submission. Live/preprod evidence remains.                                                                                                     | —                                              |
| **W-O3** | Complete the autonomous watcher/challenger in `demo/midgard-watcher`, building on its ingestion, indexing, finality, rollback, durable-state foundation, and the fault-proof package's classifier/workflow: fetch retained DA payload → run complete canonical replay → classify violation → drive prepare/submit chain → drive removal. | W-O1/W-O2 for remaining adapters and actuation |
| **W-O4** | Single-command orchestration per family (collapse the 4–6 manual steps; persist step state; resume on failure).                                                                                                                                                                                                                          | W-O1                                           |
| **W-O5** | DA-first evidence sourcing: prepare-\* should fetch `DaPayloadV1` via the libp2p retained-DA client (`transition-trace/fetch.ts` pattern) instead of relying on node REST or local files (node REST is not a production DA source per `1-da-layer.tex:17-18`).                                                                           | — (implementation-ready)                       |
| **W-O6** | **Implemented locally:** versioned violation→family rules cover every catalogue category in canonical order; selection is deterministic and unmapped violations fail as `unprovable_gap`. The focused workflow suite passes 28/28; autonomous watcher mounting remains W-O3.                                                             | W-O3                                           |
| **W-O7** | Complete the deliberately inert committee-store pruner under the Q54-derived retention contract; do not weaken Q54's node/committee configuration, manifest, pruning, readiness, and alert enforcement.                                                                                                                                  | Q58                                            |
| **W-O8** | Finish the fraud→fault rename (commit the SDK move; single `fault-proof/` dir; update lingering docs).                                                                                                                                                                                                                                   | — (implementation-ready)                       |

## 6. Data commitment, retention, retrieval

- Covered by W-O5/W-O7 plus: keep `blocks` ordering durable post-merge or document DA
  payloads as the sole ordered source (`TX_VALIDATION_TABLE_ROLES.md:168-175`); add
  `da-compression.ts` unit tests; if D-S5 chooses disputable Plutus, extend
  `DaPayloadV1` and `MidgardTxCompactV1` with the committed trace material (spec change +
  codec + committee checklist + proof artifacts).

## 7. State-correction lifecycle

W-C10 (economics activation/idempotency) → W-T6 (integration): removal against a live
node with real lease coordination and structural descendant pruning, post-removal event re-inclusion verified (next block's
event interval covers removed intervals), and cross-operator descendant regression.
Success criterion: from any faulty block at any queue position, with rotated operators, a
single off-chain command restores the canonical queue and slashes exactly the faulty
operator's bond with the prover paid.

## 8. Test plan

| ID       | Layer                         | Content                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| -------- | ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **W-T7** | CI                            | TypeScript fault-proof, validation, and SDK build/typecheck/test jobs are wired in `midgard-node-ci.yml`. Wire the remaining legacy Plutarch helper suite into CI, or retire that dependency with explicit replacement evidence.                                                                                                                                                                                                                                                       |
| **W-T1** | Emulator e2e                  | Extend `submit-init-emulator.test.ts` pattern to every registered family as tooling lands (W-O1): init → steps → token mint → removal.                                                                                                                                                                                                                                                                                                                                                 |
| **W-T2** | Property/unit                 | Native-tx codec property tests (reject smuggled fields, non-canonical CBOR); negative "proof fails against valid block" tests for every family (soundness half); **canonical-equivalence properties between the off-chain codec and the on-chain proof decoders** (every encoding accepted on-chain must be accepted off-chain and vice versa — matrix §11 #12).                                                                                                                       |
| **W-T3** | Aiken unit                    | Transition-trace count and omitted/out-of-window subvariants are complete. Remaining work is the reject-code reachability cleanup and release evidence.                                                                                                                                                                                                                                                                                                                                |
| **W-T5** | Aiken unit                    | Computation-thread and catalogue controls exist; current working tree adds terminal mint coupling and both structural removal branches. An executable counterexample confirms duplicate-`Init`/claim-lock remains open. Local node correction/re-inclusion exists; real-node integration remains open.                                                                                                                                                                                 |
| **W-T4** | Pre-production                | Run double-spend end-to-end on preprod and publish reproducible evidence bound to the current blueprint, catalogue, deployment manifest, and reference-script identities. Then run one family per milestone on preprod.                                                                                                                                                                                                                                                                |
| **W-T6** | Integration/e2e               | Removal vs real node; watcher-driven detect→prove→remove drill inside the e2e acceptance skill; DA retrieval across real sockets (two independent processes).                                                                                                                                                                                                                                                                                                                          |
| **W-T8** | Structural-claim verification | Executable tests for every "covered structurally / N/A" justification in [`coverage-matrix.md`](coverage-matrix.md) §11c: same-block deposit spend provable via no-input; duplicate-TxId fails closed; carry-over rules reject at commit; oversized deposits rejected at L1 creation; cross-block replay reduces to no-input. Plus per-family **worst-case sizing fixtures** (max inputs/outputs/assets) proving evidence fits L1 limits or forcing a witness-splitting design (§11b). |

## 9. Dependency order and milestones

```
M0  Hygiene & truth            W-T7 (CI), W-T4 (preprod re-run), W-O8 (rename), spec fixes
M1  Reachability               W-C1 complete → W-O1/W-O2 (tooling) → W-T1 (emulator e2e)
M2  Correction robustness      structural descendant fix complete → W-T5/W-T6; D-E1 → W-C10
M3  Fund-theft closure         value/mint/input uniqueness/network-id complete locally; finish native-script
                               authorization gaps, min-ada, tooling and live evidence
M4  DA accountability          D-DA1/D-DA2/D-DA3 → W-C11, W-O7
M5  Autonomy & acceptance      W-O5 plus completed-local W-O6 → W-O3 (watcher), W-O4; full preprod drill
```

M0–M1 and the marked implementation-ready items have no decision dependencies.
The remaining work should consume the accepted decisions rather than reopening
already implemented D-C1/D-S3/D-S6/D-S8/D-S9/Q60/Q62/Q63 choices.

## 10. Acceptance criteria

- **Per proof family**: spec rule + violation named; evidence format in spec and codecs;
  Aiken steps with positive and negative (valid-block) tests; catalogue-registered with
  deployed reference scripts; single-command off-chain workflow; emulator e2e through
  removal; preprod e2e at least once; row flipped to ✅ in the coverage matrix in the same
  PR.
- **System-level (launch gate)**: no 📄/❌ rows remaining in the fund-theft tier of
  [`coverage-matrix.md`](coverage-matrix.md) §12; cross-operator cascade removal proven in
  emulator + preprod; non-zero economics with prover payout verified; retention ≥ maturity
  enforced by code; watcher completes an unattended detect→prove→remove drill; CI runs
  every suite that guards the above.
- Mirrors the standing rule in `public_testnet_readiness.md`: no public claims of
  fault-proof readiness before these hold.

## 11. Exact local verification commands

See [`testing-status.md`](testing-status.md) §3 for the full list. Minimum loop while
developing proof types:

```bash
cd onchain/aiken && aiken fmt --check && aiken check && aiken build --env testnet
pnpm --dir demo/midgard-fault-proofs test
pnpm --dir demo/midgard-validation test
```

## 12. Implementation-ready vs decision-dependent

**Implementation-ready now**: W-T7, W-T4, W-O8, the spec-list fix,
catalogue root-pin reconciliation, W-O1 for incomplete operational tooling,
W-O5, W-T1, W-T2, W-T5, W-T6, and W-T8.

**Decision-dependent**: non-zero economics and claim idempotency (D-E1),
unavailable-data remedy (D-DA2), committee-pruner policy (D-DA3), aggregate
Plutus release scope, and any remaining evidence-liveness/proof-fit decisions.
