# Context and Non-Goals

`lucid-midgard` is a proposed TypeScript library for constructing,
signing, validating, and submitting Midgard-native L2 transactions. Its public
interface should feel familiar to lucid-evolution users, but its semantics must
come from Midgard's native transaction format and validation pipeline.

## Goals

- Provide a fluent, typed, deterministic transaction builder for Midgard L2.
- Produce `MidgardNativeTxFull` bytes accepted by the current `/submit` path.
- Sign the Midgard-native body hash, not a Cardano transaction body hash.
- Make Phase A failures unlikely by construction and easy to diagnose.
- Provide optional Phase A and Phase B local validation helpers.
- Support simple pubkey transfers first, then scripts, observers, minting,
  protected receive outputs, and transaction chaining.
- Reduce duplication in `submit-l2-transfer` and future user-facing tools.

## Non-Goals

- Do not provide legacy transaction compatibility or alias behavior.
- Do not make Cardano transaction CBOR the primary builder output.
- Do not depend on Cardano-to-native conversion as the normal path.
- Do not weaken validation defaults to make demos easier.
- Do not silently rewrite user intent when canonicalization fails.
- Do not hide tx id collisions, tx byte conflicts, fee failures, or
  validation rejections.
- Do not add node-side compatibility modes to support this library.

## Production L2 Lens

Every design choice must prioritize:

- Protocol correctness.
- Deterministic serialization.
- Explicit validation boundaries.
- Auditability of transaction intent and final bytes.
- Clear failure recovery.
- Reproducible tests and golden vectors.

Convenience API methods are acceptable only when they compile to explicit,
auditable builder state. Hidden fallback paths are not acceptable.

## Boundary With Existing Code

The current repository has two transaction domains:

- L1/protocol actions built with lucid-evolution `TxBuilder`.
- L2 user transfers built manually as `MidgardNativeTxFull`.

`lucid-midgard` owns the second domain. It may reuse Lucid CML types and wallet
utilities where appropriate, but it must not blur L1 Cardano transaction rules
with Midgard L2 validation rules.

The initial migration target is the native L2 transfer flow in
`demo/midgard-node/src/commands/submit-l2-transfer.ts`. Deposit and protocol
L1 builders remain out of scope unless a later task explicitly introduces an
adapter layer.

## Current Validation Boundary

Durable admission is not full validation. `/submit` validates hex and size,
normalizes bytes, derives tx id, and stores a queued admission. Phase A and
Phase B run later in the queue processor. `lucid-midgard` may provide local
preflight checks, but node validation remains authoritative.

## Documentation Requirements

Before implementation, every component must have:

- Its responsibility and explicit non-responsibilities.
- Required inputs and outputs.
- Validation invariants.
- Error behavior.
- Tests required before it can be considered complete.
