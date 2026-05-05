# T23 Submitted and Signed Object Ergonomics

## Scope

Harden the staged transaction object model so unsigned, witness-complete signed,
partial, and submitted states are explicit, immutable, and difficult to misuse.

The API should remain ergonomic, but lifecycle boundaries must reflect Midgard
native validation semantics.

## Dependencies

- T05 wallet and signers.
- T07 finalization and canonicalization.
- T11 submit, status, and chain.
- T17 fromTx round trip.
- T18 sign builder layer.
- T19 partial signing.
- T21 chain local outputs.
- T22 safe and Effect APIs.

## Deliverables

- `CompleteTx` as the immutable completion result. It is submit-capable only
  when all existing witnesses are valid, the expected address witness set is
  known, and the expected set is complete.
- `PartiallySignedTx` as the immutable, non-submit-capable object introduced by
  T19 for explicitly allowed incomplete or unknown witness collection.
- `SubmittedTx` as durable admission plus status helpers.
- Signing builder surface such as:
  - `completeTx.sign.withWallet().complete()`
  - `completeTx.sign.withPrivateKey(...).complete()`
  - `completeTx.sign.withExternalSigner(...).complete()`
- `CompleteTx.submit(...)`, `CompleteTx.status(...)`, and
  `CompleteTx.awaitStatus(...)` for witness-complete signed values.
- `SubmittedTx.admission`, `SubmittedTx.tx`, `SubmittedTx.status(...)`, and
  `SubmittedTx.awaitStatus(...)`.
- Consistent `txId`, `txIdHex`, `txCbor`, `txHex`, metadata, and local-output
  accessors.
- Accessors must preserve Midgard semantics: `txId`/`txIdHex` are the Midgard
  native compact body hash, and `txCbor`/`txHex` are Midgard native full
  transaction bytes at every lifecycle stage.
- Witness verification before exposing submit-capable signed objects.
- `fromTx` import staging rules from T17 are reflected in these object types.
- T19 partial import and continuation rules are reflected in these object types.

## Acceptance Criteria

- Unsigned or witness-incomplete `CompleteTx` values cannot be submitted
  successfully through the public API.
- `PartiallySignedTx` cannot be submitted through the public API.
- Invalid witnesses prevent submit-capable use; incomplete or unknown expected
  witness sets are represented as `PartiallySignedTx`.
- Signing verifies witnesses against the Midgard-native body hash.
- Signing never changes the transaction id.
- Re-signing with the same witness does not duplicate witness entries.
- Missing, wrong, or extraneous expected address witnesses fail before submit.
- `SubmittedTx` never labels durable admission as validation acceptance.
- All staged objects expose immutable snapshots.
- Object names and examples make the lifecycle unambiguous.

## Required Tests

- Runtime/API tests proving unsigned or witness-incomplete `CompleteTx.submit()`
  fails before provider submission.
- Runtime tests for successful signing and native body-hash verification.
- Missing signer, wrong signer, duplicate signer, and extra signer tests.
- Cardano-domain witness/body-hash staged-object rejection tests.
- Imported signed bytes with unknown expected witnesses become
  non-submit-capable only through T19 explicit partial import; default T17
  import fails closed.
- Submit success with durable admission metadata.
- Status polling from signed `CompleteTx` and submitted objects.
- Immutability tests for metadata, bytes, and local outputs.
- Local output equality across complete, signed, and submitted stages.

## Non-Goals

- No automatic signing during submit.
- No finality guarantees beyond provider status semantics.
- No browser wallet adapter unless correct Midgard body-hash signing is proven.
- No compatibility aliases for the previous staged-object shape.
