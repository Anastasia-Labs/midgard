# Task: Native Tx Plutus Witness Validation

## Objective

Add production-grade phase-1 support for evaluating Midgard native transactions
that carry Plutus witness material:

- Plutus scripts
- redeemers
- datum witnesses
- non-empty `scriptIntegrityHash`

The implementation must admit these features only as one coherent bundle. The
node must not accept Plutus-shaped transactions unless it can preserve witness
intent, reconstruct the Cardano transaction deterministically, resolve datum and
reference-script context from the current ledger view, and execute witness
validation against that exact context.

## Background

The codec already preserves:

- Plutus script witnesses in `scriptTxWitsPreimageCbor`
- redeemers in `redeemerTxWitsPreimageCbor`
- `script_data_hash` in `body.scriptIntegrityHash`

But the current native tx format does not preserve datum witnesses, and phase A
still rejects non-empty `scriptIntegrityHash`, redeemers, and inline Plutus
scripts. That leaves a gap between what ingress can carry and what validation
can safely execute.

## Scope

### In scope

- Extend the native tx witness set to preserve Plutus datum witnesses.
- Keep backward decode compatibility for existing v1 native tx bytes.
- Allow non-empty `scriptIntegrityHash`, datum witnesses, redeemers, and
  Plutus scripts through phase A when they are structurally valid.
- Trigger stateful Plutus witness validation in phase B using:
  - deterministic Cardano tx reconstruction
  - exact spent/reference-input UTxO context from the effective ledger state
  - the configured L1 evaluator
- Support inline Plutus scripts and Plutus reference scripts.
- Preserve datum witnesses on Cardano ingress and reverse conversion.
- Add focused tests for structural acceptance, evaluator context construction,
  success/failure behavior, and backward compatibility.
- Update validation docs to describe the supported Plutus subset.

### Out of scope

- Collateral semantics and collateral-return validation.
- Certificates, governance, treasury, bootstrap witnesses, or other unsupported
  Cardano features.
- Auxiliary data payload preservation.
- Silent fallback behavior that skips Plutus validation when evaluator support
  is unavailable.

## Design Constraints

- Plutus support must be all-or-nothing for a given tx. If a tx carries any
  Plutus witness bundle component, phase B must run the evaluator path.
- Datum witness bytes must be part of the canonical native witness set rather
  than reconstructed heuristically.
- Reference-script presence alone must not be treated as success. Validation
  must execute the reconstructed Cardano tx against the resolved UTxO context.
- Purpose indexing must remain whatever the tx witness set declares; the node
  must preserve it exactly during native -> Cardano reconstruction.
- Old v1 native txs must continue to decode and validate exactly as before.

## Plan

1. Extend the native witness-set format with explicit datum witness storage
   under a new native tx version while preserving v1 decode compatibility.
2. Widen Cardano ingress and reverse conversion so Plutus datum witnesses
   survive round-trip conversion.
3. Replace phase-A blanket Plutus rejections with structural decoders and a
   `requiresPlutusEvaluation` classification.
4. Add a phase-B Plutus evaluator hook that:
   - reconstructs Cardano tx bytes from the native tx
   - derives the exact additional UTxO context from the current effective state
   - invokes the configured evaluator deterministically per candidate
5. Update docs and tests so the supported Plutus subset is explicit and
   replayable.

## Task List

- [x] Introduce native tx witness-set v2 with datum witness root/preimage.
- [x] Preserve `plutus_datums` on Cardano ingress and reverse export.
- [x] Decode mixed native/Plutus script witnesses without rejecting Plutus in
      phase A.
- [x] Structurally validate datum witnesses and redeemers in phase A.
- [x] Mark candidates that require phase-B Plutus evaluation.
- [x] Thread a Plutus evaluator callback through phase-B configuration and the
      live tx-queue processor.
- [x] Build additional UTxO context from effective spent/reference inputs.
- [x] Reject Plutus-shaped txs explicitly when evaluator support is unavailable.
- [x] Add regression tests for:
      - v1 decode compatibility
      - datum witness codec round-trip
      - inline Plutus witness evaluation path
      - reference-script Plutus evaluation path
      - evaluator failure propagation
      - missing evaluator rejection
- [x] Update `PHASE1_VALIDATION_RULES.md` to describe the supported Plutus
      witness bundle.

## Deliverables

1. Native tx codec support for datum witnesses in the witness set.
2. Backward-compatible native tx decoding for v1 and v2.
3. Phase-A structural validation for Plutus witness material.
4. Phase-B Plutus evaluator integration with exact ledger-context UTxOs.
5. Documentation updates reflecting the supported Plutus subset.
6. Regression tests for codec, validation, and evaluator-path behavior.

## Required Validation Semantics

### Phase A

- Accept non-empty Plutus scripts, redeemers, datum witnesses, and
  `scriptIntegrityHash` when they decode successfully.
- Continue verifying inline native scripts statelessly in phase A.
- Reject malformed datum witness bytes, malformed redeemer containers, malformed
  script witnesses, and malformed script-data-hash bytes with stable errors.
- Classify candidates that require phase-B Plutus evaluation.

### Phase B

- For candidates without Plutus witness material, preserve current native
  validation behavior.
- For candidates with Plutus witness material:
  - reconstruct Cardano tx bytes from the native tx
  - derive additional UTxOs for every spent and reference input from the
    effective state
  - invoke the configured evaluator on those bytes plus that UTxO context
  - reject on evaluator failure with a stable Plutus-specific reject code
- Preserve existing input existence, double-spend, dependency, native-script,
  validity-interval, and value-preservation checks.

## Acceptance Criteria

### Functional

- A Cardano tx with inline datum witnesses, redeemers, Plutus scripts, and
  `script_data_hash` survives Cardano -> Midgard-native -> Cardano conversion.
- A v2 native tx with structurally valid Plutus witness material passes phase A.
- A Plutus-shaped tx triggers phase-B evaluator invocation exactly once with the
  tx’s spent/reference-input UTxO context.
- A Plutus-shaped tx accepted by the evaluator passes phase B.
- A Plutus-shaped tx rejected by the evaluator is rejected in phase B with a
  stable Plutus-specific error code.
- A tx using a Plutus reference script succeeds when the reference input
  provides the script and fails when the evaluator rejects it.
- Existing v1 native tx bytes continue to decode and validate unchanged.

### Safety

- No tx with Plutus witness material is accepted without running the evaluator.
- No datum witness bytes are silently dropped during ingress, hashing, or
  export.
- No path treats reference-script presence alone as sufficient for success.
- No path silently converts malformed datum/redeemer/script witness bytes into
  empty collections.

### Tests

- Codec tests cover datum witness round-trip and v1 compatibility.
- Validation tests cover inline and reference-script Plutus evaluator paths.
- Validation tests cover evaluator failure and evaluator-unavailable rejection.
- Validation tests cover that non-Plutus txs continue to bypass the evaluator.

### Documentation

- `PHASE1_VALIDATION_RULES.md` reflects the supported Plutus witness bundle.
- The task is complete only when the docs, implementation, and tests describe
  the same supported behavior.

## Exit Condition

This task is complete only when the native tx format can preserve the full
Plutus witness bundle needed for phase-1 evaluation, phase B validates those
transactions against the exact ledger context, old v1 txs still replay, and an
independent review concludes that the implementation is production ready.
