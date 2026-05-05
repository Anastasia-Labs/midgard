# lucid-midgard Design Pack

This directory is the design workspace for `lucid-midgard`, a Midgard-native
transaction builder library with an API style inspired by lucid-evolution.

The library must be designed as production L2 infrastructure. It must preserve
Midgard native transaction semantics, Phase A/Phase B validation boundaries,
deterministic encoding, and explicit failure behavior. It must not become a
compatibility shim for legacy transaction formats or a Cardano transaction
builder with Midgard names.

## Architecture Documents

- [T00 Execution Decisions](./T00-execution-decisions.md)
- [00 Context and Non-Goals](./00-context-and-non-goals.md)
- [01 Public API](./01-public-api.md)
- [02 Native Transaction Model](./02-native-transaction-model.md)
- [03 Builder State Machine](./03-builder-state-machine.md)
- [04 Provider and Wallet](./04-provider-and-wallet.md)
- [05 Balancing, Fees, and Coin Selection](./05-balancing-fees-and-coin-selection.md)
- [06 Scripts, Redeemers, and Indexing](./06-scripts-redeemers-and-indexing.md)
- [07 Validation and Simulation](./07-validation-and-simulation.md)
- [08 Submission and Observability](./08-submission-and-observability.md)
- [09 Testing and Conformance](./09-testing-and-conformance.md)
- [10 Subagent Orchestration](./10-subagent-orchestration.md)
- [11 API Parity Map](./11-api-parity-map.md)

## Task Documents

- [T01 Package Scaffold](./tasks/T01-package-scaffold.md)
- [T02 Native Codec Surface](./tasks/T02-native-codec-surface.md)
- [T03 Core Types](./tasks/T03-core-types.md)
- [T04 Provider Client](./tasks/T04-provider-client.md)
- [T05 Wallet and Signers](./tasks/T05-wallet-and-signers.md)
- [T06 Builder Fluent API](./tasks/T06-builder-fluent-api.md)
- [T07 Finalization and Canonicalization](./tasks/T07-finalization-and-canonicalization.md)
- [T08 Balancing and Fees](./tasks/T08-balancing-and-fees.md)
- [T09 Script and Redeemer Builders](./tasks/T09-script-and-redeemer-builders.md)
- [T10 Local Validation](./tasks/T10-local-validation.md)
- [T11 Submit, Status, and Chain](./tasks/T11-submit-status-chain.md)
- [T12 Migration and Assurance](./tasks/T12-migration-and-assurance.md)
- [T13 API Parity Map](./tasks/T13-api-parity-map.md)
- [T14 Provider Switching and Overrides](./tasks/T14-provider-switching-and-overrides.md)
- [T15 Provider Convenience Methods](./tasks/T15-provider-convenience-methods.md)
- [T16 Observer Validator Surface](./tasks/T16-observer-validator-surface.md)
- [T17 FromTx Round Trip](./tasks/T17-from-tx-round-trip.md)
- [T18 Sign Builder Layer](./tasks/T18-sign-builder-layer.md)
- [T19 Partial Signing](./tasks/T19-partial-signing.md)
- [T20 Compose Builders](./tasks/T20-compose-builders.md)
- [T21 Chain Local Outputs](./tasks/T21-chain-local-outputs.md)
- [T22 Safe and Effect APIs](./tasks/T22-safe-and-effect-apis.md)
- [T23 Submitted and Signed Object Ergonomics](./tasks/T23-submitted-and-signed-object-ergonomics.md)
- [T24 Provider Conformance and Hardening](./tasks/T24-provider-conformance-and-hardening.md)
- [T25 Documentation and Examples](./tasks/T25-documentation-and-examples.md)
- [T26 API Hardening and Regression Guards](./tasks/T26-api-hardening-and-regression-guards.md)

## Current Source of Truth

These docs intentionally treat implementation and focused tests as the current
authority:

- `demo/midgard-core/src/codec/native.ts`
- `demo/midgard-core/src/codec/cbor.ts`
- `demo/midgard-core/src/codec/hash.ts`
- `demo/midgard-core/src/codec/output.ts`
- `demo/midgard-node/src/validation/phase-a.ts`
- `demo/midgard-node/src/validation/phase-b.ts`
- `demo/midgard-node/src/validation/types.ts`
- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/listen-utils.ts`
- `demo/midgard-node/src/commands/submit-l2-transfer.ts`
- `L2_TX_EVALUATION_CURRENT.md`

`MIDGARD_TX_FORMAT_V1.md`, `technical-spec`, and `cddl-files/codec.cddl` are
useful background, but they are not assumed to be authoritative where they
drift from implementation.

## Known Format Drift To Resolve

The current implementation and tests use three witness buckets:

- `addr_tx_wits`
- `script_tx_wits`
- `redeemer_tx_wits`

Some older design notes may mention a datum witness bucket. `lucid-midgard`
must follow the implemented three-bucket native format unless the protocol
explicitly migrates through a separate audited change.
