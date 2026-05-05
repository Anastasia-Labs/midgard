# Testing and Conformance

Testing must prove that `lucid-midgard` produces bytes and behavior accepted by
the node validation pipeline.

## Golden Vectors

Create golden fixtures for:

- Empty/default preimage buckets.
- Simple pubkey transfer.
- Multi-asset transfer.
- Transfer with change.
- Protected pubkey output.
- Protected script receive output.
- Native script spend.
- PlutusV3 spend.
- MidgardV1 receive.
- Mint and burn.
- Observer execution.

Every golden vector should record:

- Builder input intent.
- Native tx hex.
- Tx id.
- Compact body hash.
- Compact witness hash.
- Decoded root/preimage hashes.
- Expected Phase A result.
- Expected Phase B result when pre-state is provided.

## Unit Tests

Required unit coverage:

- Asset arithmetic.
- Outref ordering.
- Output authored-order preservation.
- Redeemer pointer derivation.
- Fee convergence.
- Duplicate input/reference rejection.
- Datum witness dedupe.
- Protected-output marker encoding.
- Signature verification over native body hash.

## Integration Tests

Integration tests should run generated transactions through existing
`runPhaseAValidation` and `runPhaseBValidationWithPatch` where possible.

Simple transfers should replace or wrap current `submit-l2-transfer` tests so
the library becomes the tested construction path.

## Property Tests

Add property tests for:

- Native encode/decode round trip.
- Root/preimage consistency.
- Value preservation for generated balanced transfers.
- Deterministic completion for identical inputs.
- No hidden mutation after completion.

## Negative Tests

Tests must prove rejection for:

- Wrong network id.
- Too-low fee.
- Missing signer.
- Invalid body-hash signature.
- Duplicate redeemer pointer.
- Extraneous script witness.
- Missing datum witness.
- Plutus/MidgardV1 budget exceeded.
- Value not preserved.

## Conformance Rule

If local library validation and node validation disagree for the same bytes and
pre-state, the test must fail. The node implementation is the authority until a
separate protocol change updates it.
