# Resolve transaction layout through Lucid redeemer contexts

Status: Accepted

Last reviewed: 2026-09-07 (SDK helpers and double-spend step 04).

## Context

Redeemers that contain input, output, or redeemer indexes must agree with the
transaction that Lucid balances and evaluates. Completing a disposable draft to
learn those indexes duplicates construction and can fail evaluation before the
correct redeemers exist. A separately modeled snapshot of Lucid's internal
builder configuration would also duplicate its ordering and lifecycle rules.

## Decision

Use the pinned Lucid `BuildTxWithRedeemer` callback and its `RedeemerContext`.
Keep strict domain selectors in
[`tx-context-redeemer.ts`](../../../demo/midgard-sdk/src/tx-context-redeemer.ts):
missing inputs or redeemers and ambiguous outputs fail explicitly. The context
supplies the resolved transaction layout; callers do not probe a completed draft
or reconstruct Lucid's private configuration.

[`submit-step-04.ts`](../../../demo/midgard-fault-proofs/src/submit-step-04.ts)
uses this callback for the double-spend spend and mint redeemers, then completes
once with `localUPLCEval: true`. It also excludes reference inputs from wallet
balancing when field carriage is present.

## Consequences

The former `createTxLayoutRedeemerFactory` proposal is superseded; it is not an
SDK export or pending delivery requirement. Add selectors only for real callers,
using the public Lucid context and explicit cardinality checks. Changes to the
pinned Lucid version require checking callback timing and final layout behavior,
including local UPLC evaluation and missing/ambiguous selector failures.
