# 0008 — Duplicate-event ownership between families

- Status: Accepted; implemented
- Scope: Catalogue ranking of `crossBlockDuplicateEvent` and `doubleWithdraw`
  when one committed user event repeats another
- Recorded: 2026-09-12

## Decision

`crossBlockDuplicateEvent` ranks ahead of `doubleWithdraw` in the canonical
classification order. It owns the shared corner where the repeated event was
already committed by an ancestor block, because it is the more general
statement: its evidence is the same two inclusions regardless of event kind,
and a repeated withdrawal is proved the same way as a repeated deposit.

`doubleWithdraw` keeps the intra-block case, where both withdrawals of the same
L2 output are leaves of the accused block. `crossBlockDuplicateEvent` cannot
fire there because there is no ancestor inclusion to open.

Each family therefore has a disjoint territory, and the overlap has one owner.

## Consequences

- The classification rule for `crossBlockDuplicateEvent` precedes the rule for
  `doubleWithdraw`, so a cross-block repeated withdrawal at a given position
  selects `crossBlockDuplicateEvent` and an intra-block pair selects
  `doubleWithdraw`.
- Fixtures that stage a cross-block repeated withdrawal must expect
  `crossBlockDuplicateEvent`; fixtures that stage two withdrawals of one output
  inside one block must expect `doubleWithdraw`.
- This decision does not rank either family against `transitionTrace` or
  `withdrawalMistag`; those corners remain governed by the existing rule order.

## Authorities

- [Classification rule order](../../../demo/midgard-fault-proofs/src/workflow/classification.ts)
- [Cross-block duplicate-event workflow](../../../demo/midgard-fault-proofs/src/cross-block-duplicate-event/workflow.ts)
- [Double-withdraw workflow](../../../demo/midgard-fault-proofs/src/workflow/double-withdraw.ts)
