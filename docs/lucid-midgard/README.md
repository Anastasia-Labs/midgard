# lucid-midgard Design Pack

This directory records the architecture and protocol boundaries of the
implemented `@al-ft/lucid-midgard` package, a Midgard-native transaction
builder library with an API style inspired by lucid-evolution.

The library must be designed as production L2 infrastructure. It must preserve
Midgard native transaction semantics, Phase A/Phase B validation boundaries,
deterministic encoding, and explicit failure behavior. It must not become a
compatibility shim for legacy transaction formats or a Cardano transaction
builder with Midgard names.

## Architecture Documents

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

## Current Source of Truth

These docs intentionally treat implementation and focused tests as the current
authority:

- `demo/midgard-core/src/codec/native.ts`
- `demo/midgard-core/src/codec/cbor.ts`
- `demo/midgard-core/src/codec/hash.ts`
- `demo/midgard-core/src/codec/output.ts`
- `demo/midgard-validation/src/phase-a.ts`
- `demo/midgard-validation/src/phase-b.ts`
- `demo/midgard-validation/src/types.ts`
- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/listen-utils.ts`
- `demo/midgard-node/src/commands/submit-l2-transfer.ts`
- `demo/lucid-midgard/src/`
- `demo/lucid-midgard/tests/`

`technical-spec` and `cddl-files/codec.cddl` provide protocol background, but
the shared codec and validation packages are authoritative for the currently
accepted native-v1 bytes.

## Known Format Drift To Resolve

The current implementation and tests use three witness buckets:

- `addr_tx_wits`
- `script_tx_wits`
- `redeemer_tx_wits`

Some older design notes may mention a datum witness bucket. `lucid-midgard`
must follow the implemented three-bucket native format unless the protocol
explicitly migrates through a separate audited change.
