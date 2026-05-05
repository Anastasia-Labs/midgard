# T02 Native Codec Surface

## Scope

Expose the shared native transaction codec required by `lucid-midgard`
without creating a divergent serialization implementation.

## Dependencies

- T01 package scaffold.
- Architecture doc `02-native-transaction-model.md`.

## Deliverables

- Shared `@al-ft/midgard-core` package owning the native v1 codec.
- Public codec exports or dependency wiring for:
  - Native constants.
  - `MidgardNativeTxFull` and related types.
  - Strict CBOR helpers.
  - Hash helpers.
  - Native full encode/decode.
  - Native materialization from canonical preimages.
- Golden fixture for a minimal native transaction shape.

## Acceptance Criteria

- Encoding produced by the library decodes through shared core and the node
  compatibility shims.
- Decoding rejects legacy four-bucket witness sets with a datum witness bucket.
- Tx id equals compact body hash.
- Root/preimage mismatches fail.
- `midgard-node` and `lucid-midgard` do not carry independent codec
  implementations.

## Required Tests

- Encode/decode round trip.
- Root/preimage consistency.
- Tx id derivation.
- Three witness bucket enforcement.
- Strict CBOR trailing-byte rejection.

## Non-Goals

- Do not change codec semantics in this task.
- Do not add Cardano-to-native conversion as a normal builder path.
