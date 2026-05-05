# T18 Sign Builder Layer

## Scope

Replace the ad hoc `CompleteTx.sign(wallet?)` flow with a `TxSignBuilder`-style
API while preserving current Midgard-native signing semantics.

Signing must add address witnesses over the Midgard compact body hash. It must
never sign or verify against a Cardano transaction body hash.

## Dependencies

- T05 wallet/signers.
- T07 finalization and canonicalization.
- T11 submit/status chain.
- T17 fromTx round trip.

## Deliverables

- `CompleteTx.sign` builder surface.
- `completeTx.sign.withWallet()`.
- `completeTx.sign.withPrivateKey(privateKey, address)`.
- `completeTx.sign.withExternalSigner(signer)`.
- `completeTx.sign.withWitness(witnessOrCbor)`.
- `completeTx.sign.withWitnesses(witnesses)`.
- `TxSignBuilder.complete()`, returning the submit-capable signed `CompleteTx`
  compatibility shape only when the expected address witness set is known and
  complete.
- `TxSignBuilder.partial()` belongs to T19 and returns a detached partial
  witness bundle, not a submit-capable transaction.
- Structured signing errors for missing wallet, invalid witness, unexpected
  witness, conflicting witness, and incomplete expected witness set.
- Backing helper that merges address witnesses, rebuilds `addrTxWitsRoot`,
  rebuilds the compact witness hash, and preserves the compact body hash and tx
  id.
- Deterministic address witness ordering by signer key hash, with duplicate
  handling independent of signing order.
- Strict manual witness decoding for `withWitness(...)`: canonical CBOR only, no
  trailing bytes, key hash derived from the vkey witness itself, and signature
  verification against the Midgard body hash before merge.
- Metadata updates for `addrWitnessCount`, `signedBy`, `txByteLength`, and
  expected witness completion status.
- Compatibility decision for the existing `CompleteTx.sign(wallet?)` shortcut:
  keep it only as a documented thin wrapper over
  `completeTx.sign.withWallet().complete()` with no separate semantics.

## Acceptance Criteria

- `completeTx.sign.withWallet().complete()` signs
  `nativeTx.compact.transactionBodyHash`.
- Private key and external signer paths reuse existing wallet identity
  hardening.
- Every newly added and pre-existing address witness verifies before the signed
  transaction is returned.
- Cardano-domain witnesses fail verification.
- `TxSignBuilder.complete()` fails by default if any expected address witness is
  missing or if the expected witness set is unknown.
- Signing a transaction does not mutate the source `CompleteTx`.
- Signing the same key twice with the same witness is idempotent.
- Signing the same key twice with conflicting witness bytes fails.
- Unexpected address witnesses fail when the expected witness set is known.
- Missing expected witnesses do not bypass submit hardening.
- The tx id remains unchanged after signing because the body hash is unchanged.
- The transaction hex changes only when the witness set changes.

## Required Tests

- Wallet signing through `completeTx.sign.withWallet().complete()`.
- Private key signing through the sign builder.
- External signer signing with declared key hash/address binding.
- Manual witness attachment through `withWitness`.
- Manual witness attachment rejects noncanonical CBOR, trailing bytes, invalid
  signatures, and any mismatch between claimed and derived signer identity.
- Re-signing with the same wallet is idempotent.
- Conflicting witness for the same key hash is rejected.
- Existing invalid imported witness is rejected during signing.
- Wrong body-hash/Cardano-domain witness is rejected.
- Tx id is unchanged and witness hash changes after first successful signing.
- Signing order does not affect final address witness order or tx bytes.
- `TxSignBuilder.complete()` rejects incomplete or unknown expected witness
  sets.
- Submit still fails when required expected witnesses are missing.

## Non-Goals

- No browser wallet adapter.
- No Cardano transaction signing path.
- No multisig policy construction.
- No partial witness export or assemble API; that belongs to T19.
