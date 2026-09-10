# 0004 — Checkpointed ledger-output proof facts

- Status: Accepted; implemented for ScriptSources and ResolveInputs
- Scope: shared ledger-output step and finalization resolvers
- Recorded: 2026-09-07, extracted from delivered plans and checked against source;
  this date is not a new protocol acceptance or measurement receipt.

## Context

Sharing ledger-output stage predicates reduced script size, but one transaction
running the dispatcher and every descriptor yield still exceeded memory limits.
Repeating chunk authentication and scalar-control decoding in each stage also
made the byte and execution budgets unacceptable.

## Decision

Use a narrow raw control frame that preserves the canonical field-specific
encodings. Decode only the active sub-control. The control contains the machine
items, a committed span window, and four descriptor-fact commitments.

Attach the authenticated output span in a separate machine step. It verifies
chunk membership once and records start, length, and digest. Subsequent window
consumers require containment, exact length, and digest equality before slicing.
Scalar attestation yields bind decoded scalar claims to the same authenticated
dispatcher action consumed by the stage yield.

Attach descriptor facts in checkpointed groups: datum and value summaries,
then scan facts, then reference-script facts. Each attachment executes its
required yields and checks the exact successor control. Commitments bind the
role, descriptor and associated summaries. The thin terminal recomputes all four
commitments from its descriptor before authorization and successor/rejection
checks; it performs no descriptor-yield executions itself.

The conjunction across authenticated steps must equal the original predicate.
A fact cannot be omitted, duplicated, reordered, or reused for a different
descriptor. A well-formed control permits facts only at the terminal machine
stage, and attachment fills only the next empty group.

## Alternatives and consequences

A single transaction containing all descriptor checks was measured over the
execution ceiling. Hoisting span authentication and checkpointing fact groups
preserves every semantic clause at the cost of more transactions. Splitting the
underlying CEK-data semantic machine was rejected because measured compilation
cost was dominated by shared span verification and control decoding; changing
that machine would introduce unnecessary wire and semantic churn.

Current acceptance requires canonical codec parity, each role's publication,
registered positive and honest-refusal lifecycles, maximum data/output/script
and proof carriage, and cancellation/recovery at the new checkpoints. Publication
coverage alone does not establish coverage of every semantic execution route.

## Authorities

- [Control frame](../../../onchain/aiken/lib/midgard/ledger-output-proof-raw.ak)
- [Stages and attachments](../../../onchain/aiken/lib/midgard/ledger-output-proof-stages.ak)
- [Descriptor facts](../../../onchain/aiken/lib/midgard/ledger-output-proof-descriptor.ak)
- [Yield claims and dispatch](../../../onchain/aiken/lib/midgard/ledger-output-proof-yield.ak)
- [Installed validation workflow](../validation-trace-dispute-installed-workflow.md)
- [Verification requirements](../testing-status.md)
