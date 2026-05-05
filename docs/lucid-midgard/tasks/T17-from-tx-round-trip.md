# T17 FromTx Round Trip

## Scope

Implement `LucidMidgard.fromTx(...)` for importing an already built
Midgard-native transaction into a contextual transaction object.

`fromTx` is for Midgard native v1 full transactions. It must not silently accept
or complete Cardano transactions. Cardano-to-native conversion remains a
separate diagnostic/import bridge and is not the normal signing path.

## Dependencies

- T02 native codec surface.
- T05 wallet/signers.
- T07 finalization and canonicalization.
- T10 local validation.
- T11 submit/status chain.
- T18 sign builder layer.

## Deliverables

- `LucidMidgard.fromTx(input, options?)`.
- Accepted input forms: `CompleteTx`, `PartiallySignedTx` only when explicitly
  requested, `MidgardNativeTxFull`, native tx CBOR `Uint8Array`/`Buffer`, native
  tx CBOR hex string, `{ txCbor }`, and `{ txHex }`.
- Rejection of Cardano `CML.Transaction`, Cardano transaction body bytes, and
  Cardano witness-set-only inputs from `fromTx`.
- Optional explicitly named diagnostic bridge, if retained, such as
  `fromCardanoTx(...)`; it must reject lossy conversion and must not be used as a
  fallback by `fromTx`.
- Import metadata derivation for tx id, byte length, address witness count,
  signed key hashes, required signer count, and expected address witnesses.
- Import options for resolved spend input pre-state so expected pubkey spend
  witnesses can be derived for imported transactions. Supplied pre-state must be
  checked for exact spend-outref matches, duplicate/conflicting UTxOs,
  unexpected extras unless explicitly allowed, exact output CBOR preservation,
  and protected-marker decoding.
- Explicit import-stage result rules:
  - Existing `CompleteTx` inputs preserve their witness-complete or unsigned
    state while attaching the current context.
  - Raw native bytes with no address witnesses import as `CompleteTx`.
  - Raw native bytes with address witnesses import as a submit-capable
    `CompleteTx` only when the expected address witness set is known and
    complete.
  - Raw native bytes with incomplete or unknown expected address witnesses fail
    closed by default.
  - Raw partial imports require the explicit T19 partial import option and
    return `PartiallySignedTx`.
- Context attachment so imported transactions can use selected wallet,
  provider-backed status, submit, and signing APIs.
- Immutable snapshots for imported transaction objects.

## Acceptance Criteria

- Imported tx bytes are decoded as Midgard native v1 full transactions and
  verified with native full consistency checks.
- The compact body hash is recomputed from the full body and remains the tx id.
- The compact witness-set hash is recomputed from the full witness set.
- Every body and witness root matches its preimage.
- Canonical native re-encoding of canonical input bytes round-trips exactly.
- Legacy four-bucket witness sets and datum witness buckets are rejected.
- Existing address witnesses decode as CML vkey witnesses and verify against
  `nativeTx.compact.transactionBodyHash`.
- Duplicate address witness entries for the same key hash are rejected on raw
  import, including byte-identical duplicates. Import must never silently rewrite
  witness bytes, witness roots, witness-set hashes, or tx hex.
- Expected address witness hardening is preserved for imported txs: required
  signers, protected pubkey outputs, and resolved pubkey spend inputs contribute
  to the expected witness set.
- `submit()` requires a complete expected address witness set; imported txs with
  unknown expected pubkey spend witnesses fail closed before submit unless the
  caller supplies enough pre-state to derive them.
- Imported transactions never switch to Cardano transaction hash semantics.

## Required Tests

- Import `CompleteTx` and preserve tx id, tx hex, metadata, and context.
- Import witness-complete `CompleteTx` bytes and preserve witness metadata and
  submit capability.
- Import native tx CBOR bytes and hex, then re-encode to identical canonical
  native bytes.
- Import native tx object and verify root/preimage consistency.
- Reject Cardano transaction bytes passed to `fromTx`.
- Reject native tx with body hash, witness hash, or root/preimage mismatch.
- Reject legacy four-bucket witness full tuple.
- Reject invalid existing vkey witness signature.
- Reject duplicate address witness entries, including byte-identical duplicates.
- Reject conflicting address witness entries for the same key hash.
- Derive expected witnesses from required signers and protected pubkey outputs.
- Derive expected pubkey spend witnesses when resolved input pre-state is
  supplied.
- Reject duplicate, conflicting, malformed, or unexpected pre-state entries.
- Treat missing pre-state for spend witnesses as unknown and fail closed.
- Fail submit for an imported tx when expected spend witnesses are unknown.
- Import raw signed bytes as submit-capable `CompleteTx` only when expected
  witnesses are known and complete.
- Sign an imported unsigned tx and keep tx id unchanged.

## Current Implementation Notes

- `LucidMidgard.fromTx(...)` is implemented for `CompleteTx`, native full tx
  objects, bytes, hex, `{ txCbor }`, and `{ txHex }`.
- Raw bytes must be canonical Midgard native full transaction bytes. Cardano
  transaction/body/witness bytes are rejected; no diagnostic conversion fallback
  is attempted.
- Raw signed imports verify every existing vkey witness against
  `nativeTx.compact.transactionBodyHash`.
- Raw signed imports fail closed when spend-input pre-state is missing, unless
  the caller explicitly opts into an unknown-witness diagnostic import. Unknown
  expected witness sets still cannot be submitted.

## Non-Goals

- No silent Cardano-to-native compatibility fallback.
- No support for legacy native transaction formats.
- No migration shim for old witness bucket layouts.
- No mutation of imported transaction bodies during import.
- No implicit partial import support; callers must request the partial stage
  explicitly.
