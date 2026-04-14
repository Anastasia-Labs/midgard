# Task: Native Tx Non-Empty Mint Validation

## Objective

Add production-grade support for evaluating Midgard native transactions with
non-empty `mintPreimageCbor` when the minting policies are native scripts.

The implementation must preserve protocol correctness, deterministic validation,
and auditability. It must not silently admit mint shapes that the validator
cannot evaluate correctly.

## Background

The Midgard native tx codec already preserves mint data and can round-trip it
back into Cardano transaction form. However, the validation pipeline still
rejects any non-empty `mintRoot` in phase A, and phase B still assumes
`mintedValue == 0`.

This creates a gap between the wire format and the executable validation
semantics.

## Scope

### In scope

- Decode and structurally validate non-empty native mint preimages.
- Support native-script minting policy validation.
- Support both inline native mint scripts and native mint scripts supplied via
  reference inputs.
- Include mint delta in phase-B value preservation.
- Add deterministic tests covering success and failure cases.
- Update validation documentation to match the implemented behavior.

### Out of scope

- Plutus minting policy evaluation.
- Collateral semantics.
- Certificates, governance, treasury, or other unsupported Cardano features.
- Silent fallback behavior that accepts malformed or only partially understood
  mint data.

## Design Constraints

- Mint support must be enabled only when the node can validate the mint
  end-to-end.
- Reference scripts must not be treated as sufficient by presence alone; the
  tx-declared mint policy set must be satisfiable by actual witness material.
- Validation must remain deterministic across nodes given the same tx bytes and
  state.
- Plutus minting must remain explicitly unsupported until redeemer, script data
  hash, datum, and local evaluation handling are implemented correctly.

## Deliverables

1. A shared mint decoder that parses `mintPreimageCbor` into:
   - canonical sorted policy ids
   - non-zero signed asset quantities
   - a signed mint delta representable as `CML.Value`
2. Phase-A structural validation for non-empty native mints.
3. Phase-B witness-satisfaction checks for native mint policies, including
   reference-script resolution.
4. Phase-B value preservation updated to include mint/burn deltas.
5. Validation docs updated to describe the supported mint subset.
6. Regression tests for inline and reference native mint scripts.

## Required Validation Semantics

### Phase A

- Accept non-empty mint preimages if and only if they decode successfully.
- Reject malformed policy ids, asset names, empty/invalid shapes, and zero
  quantities.
- Determine the set of policy ids declared by the tx body.
- Continue rejecting redeemer-driven / Plutus mint validation until that path is
  implemented correctly.

### Phase B

- Require each declared mint policy to be satisfiable by:
  - an inline native script witness, or
  - a matching native `script_ref` on a reference input
- Verify native mint scripts against the tx signer set and validity interval.
- Compute the signed mint delta from the accepted phase-A candidate.
- Enforce value conservation using:

  `totalInputs - fee + mintDelta == totalOutputs`

- Preserve existing input existence, double-spend, and validity-interval checks.

## Implementation Notes

- Reuse existing policy-ordering helpers where applicable so future Plutus mint
  support can use the same canonical ordering.
- Avoid adding ad hoc parsing logic in multiple places; the mint decoder should
  be shared between codec/export and validation where reasonable.
- If a mint policy is present but only satisfiable by a Plutus script, reject it
  explicitly rather than accepting it under a degraded rule.

## Acceptance Criteria

### Functional

- A native tx with a non-empty mint and a valid inline native mint script passes
  phase A and phase B.
- A native tx with a non-empty mint and a valid reference native mint script
  passes phase A and phase B.
- A native tx with a non-empty mint and no matching script witness is rejected.
- A native tx with a non-empty mint and a matching native script that does not
  verify against the tx signers/validity interval is rejected.
- A native tx with malformed mint preimage encoding is rejected with a stable
  validation error.
- Phase-B value preservation succeeds when mint delta is correctly included and
  fails when it is not.

### Safety

- A tx containing non-empty mint plus unsupported Plutus minting requirements is
  rejected explicitly.
- No path silently drops minted assets from validation or accounting.
- No path treats reference-script presence alone as sufficient without matching
  the tx-declared policy id.

### Tests

- Integration tests cover phase A/B success for inline native mint scripts.
- Integration tests cover phase A/B success for reference native mint scripts.
- Integration tests cover failure for missing mint witness material.
- Integration tests cover failure for invalid native mint script satisfaction.
- Integration tests cover mint-aware value preservation.

### Documentation

- `PHASE1_VALIDATION_RULES.md` reflects the supported native mint subset.
- The task is considered complete only when the implemented behavior matches the
  documentation and the tests.

## Exit Condition

This task is complete only when native-script non-empty mints are validated
correctly end to end, the unsupported Plutus subset remains explicitly fenced
off, and an independent review finds the resulting implementation production
ready.
