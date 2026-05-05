# T19 Partial Signing

## Scope

Implement detached partial address-witness bundles and assembly for multi-party
Midgard-native transaction signing.

Partial signing is only about vkey address witnesses over the Midgard compact
body hash. A partial witness bundle is not a transaction, cannot be submitted,
and must not modify the transaction body, script witnesses, or redeemers.

## Dependencies

- T05 wallet/signers.
- T17 fromTx round trip.
- T18 sign builder layer.
- T11 submit/status chain.

## Deliverables

- `MidgardPartialWitnessBundle` type.
- `PartiallySignedTx` type. It is immutable and non-submit-capable.
- Canonical serializable bundle envelope with kind/version, Midgard native tx
  version, tx id, body hash, witness CBOR hex list, and signer key hashes.
- `completeTx.sign.withWallet().partial()`.
- `completeTx.sign.withPrivateKey(...).partial()`.
- `completeTx.sign.withExternalSigner(...).partial()`.
- `completeTx.assemble(bundles, options?)`.
- `partiallySignedTx.assemble(bundles, options?)` for continued collection.
- `partiallySignedTx.toPartialWitnessBundle()` or an equivalent export helper
  for carrying forward accumulated witnesses without keeping the original
  `CompleteTx` object.
- `LucidMidgard.fromTx(..., { partial: true })` extension for explicit
  non-submit-capable partial import, after the `PartiallySignedTx` type exists.
- Default assembly that fails unless the expected address witness set is known
  and complete.
- Explicit partial assembly mode that returns a non-submit-capable
  `PartiallySignedTx`.
- Mandatory bundle import/export helpers for JSON-safe objects and canonical
  bytes.
- Duplicate, conflicting, unexpected, invalid, and wrong-transaction witness
  rejection rules.

## Acceptance Criteria

- Partial bundles are bound to exactly one Midgard transaction body hash.
- Bundle `txId` equals the Midgard native body hash for the target tx.
- Unsupported bundle kind/version and Midgard native transaction version
  mismatches fail closed.
- Bundle witnesses verify against `nativeTx.compact.transactionBodyHash`.
- Envelope signer key hashes are metadata only; they must equal key hashes
  derived from witness vkeys and must never be trusted independently.
- Bundles with a different tx id or body hash are rejected during assembly.
- Identical duplicate witnesses are idempotent and appear once in the assembled
  tx.
- Different witness bytes for the same key hash are rejected, even if both
  decode.
- Witnesses from key hashes outside the known expected witness set are rejected.
- Unknown expected witness sets fail closed by default. They may be used only in
  explicit partial collection mode, which returns a non-submit-capable object.
- Assembly preserves the transaction body, tx id, script witness bucket, and
  redeemer witness bucket.
- Assembly rebuilds the address witness root and compact witness-set hash.
- Assembly can return an incomplete signed object for continued collection only
  when explicitly allowed, and that object must not expose `submit`.
- Continued collection semantics are explicit: callers can either keep the
  original `CompleteTx` plus all bundles, or use `PartiallySignedTx.assemble(...)`
  to add more bundles to accumulated witnesses. Both paths produce identical
  bytes for identical final witness sets.
- Explicit partial import through `fromTx(..., { partial: true })` returns
  `PartiallySignedTx`; default `fromTx` behavior remains fail-closed for
  incomplete or unknown expected witness sets.
- `submit()` always requires all expected address witnesses and fails closed for
  incomplete or unknown expected witness sets.
- Partial signing never uses Cardano transaction witnesses or Cardano body-hash
  semantics.

## Required Tests

- Produce two partial bundles for a two-signer transaction and assemble them.
- Assemble partial bundles in different orders and get deterministic tx bytes.
- Assemble bundles with witnesses in different internal orders and get
  deterministic tx bytes.
- Assemble bundles with pre-existing address witnesses and preserve deterministic
  witness ordering.
- Export and import a partial witness bundle without changing its contents.
- Canonical byte export/import rejects noncanonical encodings and trailing bytes.
- Reject unsupported bundle kind/version.
- Reject bundle native transaction version mismatch.
- Reject bundle for a different tx id/body hash.
- Reject Cardano-domain witness in a partial bundle.
- Reject invalid witness CBOR in a partial bundle.
- Collapse identical duplicate witnesses.
- Reject conflicting witnesses for the same key hash.
- Reject unexpected witness when expected witnesses are known.
- Reject unknown expected witness sets by default.
- Allow incomplete or unknown expected witness sets only through explicit
  partial mode that returns a non-submit-capable object.
- Continue partial collection by assembling more bundles onto `PartiallySignedTx`.
- Verify `PartiallySignedTx` continuation and original `CompleteTx` plus all
  bundles produce identical final signed bytes.
- Explicit partial import through `fromTx(..., { partial: true })` returns
  `PartiallySignedTx`; default `fromTx` rejects the same input.
- Reject submit of an incomplete assembled tx.
- Submit succeeds after all expected witnesses are assembled.

## Non-Goals

- No partial transaction body editing.
- No script witness or redeemer patch bundles.
- No threshold/multisig policy authoring.
- No Cardano witness-set compatibility layer.

## Current Implementation Notes

- `MidgardPartialWitnessBundle`, `PartiallySignedTx`,
  `TxPartialSignBuilder`, `encodePartialWitnessBundle(...)`,
  `decodePartialWitnessBundle(...)`, and `parsePartialWitnessBundle(...)` are
  exported by `@al-ft/lucid-midgard`.
- `CompleteTx.sign` remains callable as the current single-wallet compatibility
  shortcut, and also exposes `withWallet`, `withPrivateKey`,
  `withExternalSigner`, `withWitness`, and `withWitnesses` for detached partial
  bundles through `.partial()`.
- `CompleteTx.partialSign` exposes the same partial bundle API for callers that
  prefer a lifecycle-explicit name.
- `CompleteTx.assemble(...)` and `PartiallySignedTx.assemble(...)` rebuild the
  address witness root and compact witness hash without changing the body hash
  or tx id.
- Default assembly fails closed unless expected address witnesses are known and
  complete. `{ allowPartial: true }` is required to return a non-submit-capable
  `PartiallySignedTx`.
- `LucidMidgard.fromTx(..., { partial: true })` is the explicit partial import
  path. Default `fromTx` still rejects signed imports whose expected witness set
  cannot be proven from supplied pre-state.
