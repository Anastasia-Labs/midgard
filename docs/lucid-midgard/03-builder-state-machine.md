# Builder State Machine

The builder should be a deterministic state machine from user intent to native
transaction bytes.

## Internal State

The builder state should record:

- Spend inputs, with optional spend redeemers.
- Reference inputs.
- Authored outputs.
- Required signers.
- Required observers and observer redeemers.
- Mint assets and mint redeemers.
- Inline scripts.
- Reference script dependencies.
- Datum witnesses.
- Validity interval.
- Network id.
- Fee and balancing mode.
- Change policy.

State should be structurally typed and serializable enough for debugging. The
final builder state should be inspectable before signing.

## Mutability

Prefer immutable methods that return a new builder instance. If mutable
internals are used for performance, public behavior must remain snapshot-safe:
once `complete()` starts, later user calls cannot affect that completion.

Composition follows the same rule: `a.compose(b, c)` returns a new immutable
builder and never mutates any source fragment. Context compatibility is checked
before merge, including provider identity, provider generation, protocol-info
diagnostics, wallet identity, native tx version, network id, and UTxO override
generation.

## Canonicalization Phases

Completion should run these phases:

1. Normalize and validate builder inputs.
2. Resolve provider-dependent defaults such as protocol fee parameters.
3. Select additional inputs if balancing is enabled.
4. Create explicit change output if required.
5. Derive script purpose indexes.
6. Build native preimage CBOR buckets.
7. Compute roots and compact commitments.
8. Construct unsigned native transaction.
9. Optionally run local validation.

Signing happens after unsigned completion because signatures cover the final
body hash.

## Ordering Rules

Midgard validation depends on exact ordering:

- Spend inputs in script context are sorted lexicographically by `TxOutRef`.
- Reference inputs in script context are sorted lexicographically by `TxOutRef`.
- Outputs remain in authored transaction-body order.
- Mint policy ids are sorted lexicographically for redeemer indexing.
- Required observer hashes are sorted lexicographically for redeemer indexing.
- Protected receiving script hashes are deduplicated and sorted for receive
  redeemer indexing.

The builder may preserve user-authored spend/reference input order in the wire
preimage if the protocol allows it, but any derived indexes must use validator
ordering semantics.

Local chaining uses body output indexes, not authored-output indexes, as the
UTxO index namespace. The chained `outputCbor` is copied from the native output
preimage byte-for-byte so protected-output markers and script references cannot
be lost through decode/re-encode.

## Duplicate and Conflict Checks

The builder should fail before encoding when:

- A spend input appears twice.
- A reference input appears twice.
- The same outref is both spend and reference input.
- A redeemer pointer is duplicated.
- A datum witness hash is duplicated.
- A required signer is malformed.
- A required observer is not a script credential or script hash.

These checks mirror Phase A and Phase B, but node validation remains
authoritative.

## Output Policy

Outputs must be explicit. The builder may add a change output during balancing,
but it must report the generated change output in the completion result. It
must never silently drop assets or negative balances.
