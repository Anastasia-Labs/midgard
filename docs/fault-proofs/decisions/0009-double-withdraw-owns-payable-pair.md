# 0009 — doubleWithdraw owns a block's payable same-output pair

- Status: Accepted
- Scope: Watcher classification precedence between `doubleWithdraw` and
  `withdrawalMistag`
- Recorded: 2026-11-10

## Decision

When one block commits two payable (`WithdrawalIsValid`) withdrawal leaves that
name the same L2 output, the watcher proves the pair with `doubleWithdraw`, not
the later leaf with `withdrawalMistag`.

Applying the pair's later leaf finds the output already spent, so that leaf is
also a "valid marked, output absent" `withdrawalMistag` fault, at the same
event. Both proofs slash the same block. `doubleWithdraw` needs only the two
committed leaves (two steps); `withdrawalMistag` needs the ledger state before
the leaf (five steps). The shorter proof wins.

## Rules

- The precedence applies only between findings at the same event. A fault at an
  earlier event, including a mistag on the pair's earlier leaf, still wins.
- Only these two adjacent families swap. Every other family keeps its catalogue
  order. The catalogue order itself is unchanged, because deployment identity is
  derived from it.
- A mistagged leaf that is not the later half of a payable same-output pair has
  no `doubleWithdraw` finding, so it stays `withdrawalMistag`.
- This is a watcher-only change. No validator or deployment parameter changes.

## Consequences

- `doubleWithdraw` becomes selectable for a live watcher journey.
- A watcher that journaled a `withdrawalMistag` decision for such a block before
  upgrading reclassifies the same observation as `doubleWithdraw`. It must keep
  going on the journaled decision rather than fail its decision pass. That rule
  belongs to the fault-decision bridge, for any change in classifier output,
  not to this decision.
