# Rejected execution preserves the disputed claim

Status: Accepted; implemented validation-machine semantics.

Recorded: 2026-09-07, extracted from the delivered validation-machine repair.

## Decision

A validation dispute's `ledger_delta_root` is the operator's immutable claimed
delta. Every transition preserves that claim, including a rejecting terminal.
Rejection proves that the claimed execution is wrong; it does not rewrite the
claim to an empty-delta commitment.

The no-effect obligation belongs to the rejection work witness and ledger
transition: the rejection derives no operations and retains the prior UTxO
root. Requiring the immutable claim itself to become empty would contradict
transition immutability and prevent adjudication of a nonempty claimed delta.

The implementation is `rejected_successor_is_exact` in
[validation-machine/shared.ak](../../../onchain/aiken/lib/midgard/validation-machine/shared.ak).
The [dispute emulator fixture](../../../demo/midgard-fault-proofs/tests/support/emulator/dispute-scenario.ts)
keeps a nonempty claimed delta so rejection and honest-transition controls
exercise this distinction. Current coverage is governed by
[the proof matrix](../../fault-proofs/coverage-matrix.md).
