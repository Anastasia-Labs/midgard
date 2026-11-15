# 0010 — The NoIdx families own a same-block missing output index

- Status: Accepted
- Scope: Watcher classification precedence between `nonExistentInputNoIndex`
  and `nonExistentInput`, and between `referenceInputNoIdx` and
  `noReferenceInput`
- Recorded: 2026-11-15 (owner ruling)

## Decision

When an accepted transaction spends or references an output index that is out
of range for a producing transaction committed in the same block, the watcher
proves it with `nonExistentInputNoIndex` (spend input) or `referenceInputNoIdx`
(reference input). It does not use `nonExistentInput` or `noReferenceInput`.

The output never existed, so it is also absent from the transaction's
event pre-state ledger. The ledger-absence family therefore finds it at the
same event. Both proofs slash the same block. The NoIdx proof needs only the
two committed transactions. The ledger-absence proof also needs the event
pre-state root and a non-membership proof against it.

## Rules

- The precedence applies only between findings at the same event. A fault at an
  earlier event still wins.
- Only these two pairs swap. Each NoIdx family moves directly ahead of its
  ledger-absence family. Every other family keeps its catalogue order, apart
  from the [0009](0009-double-withdraw-owns-payable-pair.md) swap. The
  catalogue order itself is unchanged, because deployment identity is derived
  from it.
- An input that is absent with no producer in the same block has no NoIdx
  finding, so it stays with `nonExistentInput` or `noReferenceInput`.
- A NoIdx finding requires the spending (or referencing) transaction and the
  producer to both be accepted (`TxIsValid`). Steps 01 and 03 bind only
  accepted transactions, so the watcher does not emit a finding it could not
  prove. A TxIsInvalid producer's outputs never enter the ledger, so the
  ledger-absence family keeps that input. A TxIsInvalid spender (or reader) is
  not a missing-input fault at all.
- This is a watcher-only change. No validator or deployment parameter changes.

## Consequences

- The NoIdx families become selectable for a live watcher journey whenever the
  predecessor ledger is available, not only when it is missing.
- A watcher that journaled a ledger-absence decision for such a block before
  upgrading reclassifies the same observation as the NoIdx family. As in 0009,
  the fault-decision bridge owns how a changed classifier output is handled.
