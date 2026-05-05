# T16 Observer Validator Surface

## Scope

Add first-class public API support for Midgard observer validators. Observers are
a supported Midgard script purpose and must not be modeled as Cardano reward
withdrawals, opaque compatibility shims, or generic Lucid validators.

## Dependencies

- T03 core types.
- T06 builder fluent API.
- T09 script and redeemer builders.
- T13 API parity map.
- Architecture docs `01`, `06`, and `09`.

## Deliverables

- Public `ObserverValidator` type analogous to `SpendingValidator`.
- `SpendingValidator`, `MintingPolicy`, and `ObserverValidator` type aliases or
  discriminated types that expose only Midgard-supported script languages.
- `ObserverValidator` must support only `PlutusV3` and `MidgardV1`.
- Compile-time type surface that does not expose `PlutusV1` or `PlutusV2`.
- Runtime rejection for untyped `PlutusV1`, `PlutusV2`, or unknown validator
  language values.
- Observer validator normalization and hash derivation using the correct hash
  domain for the validator language.
- `attach.ObserverValidator(validator)` as the primary fluent surface. A
  standalone `attachObserverScript(validator)` helper may be added only as an
  additive alias with identical semantics.
- Documentation and examples showing how an attached observer validator is
  paired with an explicit observer intent and redeemer.
- Observer script witnesses must integrate with existing required observer
  preimage, redeemer pointer, language-view, and script-integrity derivation.

## Acceptance Criteria

- A `PlutusV3` observer validator can be attached and used to build an observer
  transaction with a reward-tag redeemer pointer and `PlutusV3` language view.
- A `MidgardV1` observer validator can be attached and used to build an observer
  transaction with the MidgardV1 hash domain and `MidgardV1` language view.
- Observer indexes are derived from the lexicographically sorted required
  observer hash list, independent of attachment order.
- Duplicate observer intents fail before finalization can drop or overwrite
  redeemer data.
- Missing observer script sources fail closed with a structured builder error.
- Extraneous non-native observer script witnesses fail closed.
- Attaching an observer validator does not silently create observer execution;
  the transaction must still carry an explicit required observer intent.
- Observer APIs use only Midgard required-observer preimages and observer
  redeemer pointers; they must reject Cardano withdrawal, reward-account, or
  reward-certificate shims.
- Script integrity includes only languages actually required by the finalized
  transaction.
- The only accepted non-native language tags are `PlutusV3` and `MidgardV1`.

## Required Tests

- Type-level or compile-fixture test proving `ObserverValidator` excludes
  `PlutusV1` and `PlutusV2`.
- Runtime rejection tests for untyped `PlutusV1`, `PlutusV2`, and unknown
  validator language payloads.
- Type-level and runtime tests proving `SpendingValidator` and `MintingPolicy`
  also reject `PlutusV1`, `PlutusV2`, and unknown language payloads.
- Negative fixture proving observer APIs reject Cardano withdrawal,
  reward-account, or reward-certificate shaped inputs.
- Fixture proving observer finalization uses Midgard required-observer preimages
  and observer redeemer pointers, not withdrawal modeling.
- PlutusV3 observer validator attachment and completion fixture.
- MidgardV1 observer validator attachment and completion fixture.
- Observer hash derivation fixture for both supported hash domains.
- Sorted observer redeemer index test with multiple observers attached in a
  different order than their hash order.
- Duplicate observer intent rejection test.
- Missing observer script source rejection test.
- Extraneous observer script witness rejection test.
- Script-integrity fixture proving observer validators contribute exactly the
  required `PlutusV3` and/or `MidgardV1` language views.

## Non-Goals

- No `PlutusV1` or `PlutusV2` observer support.
- No Cardano withdrawal or reward-account observer emulation.
- No compatibility layer for legacy observer identifiers.
- No new node validator semantics.
- No provider, wallet, or submission behavior changes beyond what is required to
  expose the observer validator surface.

## Current Implementation Notes

- `SpendingValidator`, `MintingPolicy`, and `ObserverValidator` are public
  PlutusV3/MidgardV1-only discriminated types.
- `attach.SpendingValidator(...)`, `attach.MintingPolicy(...)`, and
  `attach.ObserverValidator(...)` normalize into the existing strict
  `ScriptSource` path.
- `attachObserverScript(...)` is an additive alias for
  `attach.ObserverValidator(...)`.
- Attaching an observer validator never creates an observer execution. Callers
  must still add `observe(scriptHash, redeemer)` explicitly, and unused
  non-native observer witnesses fail as extraneous script witnesses.
- Observer finalization uses Midgard required-observer preimages and reward-tag
  redeemer pointers; no withdrawal or reward-account compatibility path exists.
