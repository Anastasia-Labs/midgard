# T20 Compose Builders

## Scope

Implement deterministic composition of independently authored `TxBuilder`
fragments into one Midgard-native transaction builder.

Composition must merge builder state explicitly and preserve all production L2
invariants. It must not become a compatibility shim for Cardano or
lucid-evolution transaction builders.

## Dependencies

- T03 core types.
- T06 builder fluent API.
- T08 balancing and fees.
- T09 script and redeemer builders.
- Architecture docs `01`, `03`, and `06`.

## Deliverables

- `TxBuilder.compose(other, ...others)` or an equivalent named helper.
- Deterministic state merge for spend inputs, reference inputs, authored
  outputs, required signers, validity intervals, scripts, datum witnesses, mint
  intents, observers, and receive redeemers.
- Explicit compatibility checks or rejection for completion-critical context:
  network id, provider snapshot identity, provider generation, native
  transaction version, protocol-info source, fallback source, and any captured
  default completion options such as fee, balancing mode, or change policy.
- Left-to-right authored output ordering.
- Validity interval intersection with hard failure for disjoint intervals.
- Network id mismatch rejection.
- Duplicate and spend/reference overlap checks after composition.
- Final redeemer pointers, required observer preimages, required language sets,
  language views, and script integrity must be recomputed from the final merged
  spend, mint, observer, and receive sets during completion. Fragment-level
  indexes must never be copied.
- Debug snapshot metadata that makes composed state auditable.

## Acceptance Criteria

- `a.compose(b).compose(c)` produces the same builder state as
  `a.compose(b, c)`.
- Composition order is deterministic and documented. It is not required to be
  commutative.
- Authored output order is preserved exactly left-to-right.
- Compose does not mutate any source builder.
- Duplicate inputs, duplicate references, spend/reference overlaps, duplicate
  datum hashes, duplicate redeemer pointers, and duplicate required signers are
  rejected before completion.
- Incompatible provider/context/native-version/fallback/completion-option state
  is rejected before completion.
- Redeemer indexes and script integrity inputs are recomputed during completion,
  not copied from stale fragments.
- Compose performs no provider calls and does not balance, sign, submit, or
  locally validate.

## Required Tests

- Compose simple payment fragments and verify output order.
- Compose fragments with spend/reference conflicts and verify hard failure.
- Compose validity intervals and verify intersection and disjoint rejection.
- Compose script, mint, observer, datum, and receive-redeemer fragments.
- Compose fragments whose final sorted input, mint, observer, or receive sets
  differ from fragment-local ordering and verify redeemer indexes are recomputed.
- Reject incompatible provider snapshot, network id, native tx version,
  protocol-info fallback source, fee, balancing, or change-policy state.
- Verify associativity for ordered fragments.
- Verify source builder snapshot immutability.
- Complete a composed builder and compare native bytes with an equivalent
  directly authored builder.

## Current Implementation Notes

- `TxBuilder.compose(other, ...others)` is implemented as an immutable
  left-to-right merge of builder intent state.
- Composition rejects context mismatches for provider identity, provider
  generation, wallet identity, network id, native tx version, provider
  diagnostics, and UTxO override generation.
- Composition rejects duplicate spend/reference inputs, spend/reference
  overlaps, duplicate required signers, duplicate datum hashes, duplicate
  observer intents, duplicate receive redeemers, duplicate spend redeemers, and
  duplicate mint redeemers.
- Completion recomputes all roots, redeemer indexes, script language views, and
  script integrity from the merged final state.

## Non-Goals

- No automatic conflict resolution or silent de-duplication.
- No composition of completed, signed, or submitted transaction objects.
- No Cardano transaction-builder compose compatibility.
- No Cardano canonical/noncanonical serialization options.
