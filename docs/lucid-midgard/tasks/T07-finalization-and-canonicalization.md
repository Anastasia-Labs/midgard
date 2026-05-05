# T07 Finalization and Canonicalization

## Scope

Convert completed builder state into unsigned `MidgardNativeTxFull` bytes and
metadata.

## Dependencies

- T02 native codec.
- T06 fluent API.
- T08 may be integrated later for fee-balanced completion.

## Deliverables

- `complete()` without automatic balancing for explicitly balanced input.
- Preimage bucket construction.
- Root and compact commitment derivation.
- Tx id derivation.
- Completion metadata.
- Unsigned transaction object.

## Acceptance Criteria

- Native bytes decode with the node codec.
- Root/preimage consistency is verified before returning.
- Auxiliary data hash defaults to empty-null hash.
- Validity defaults use unbounded sentinel.
- Witness buckets exist even when empty.

## Required Tests

- Simple balanced pubkey transfer completion.
- Native decode round trip.
- Tx id equals body hash.
- Empty bucket hashes.
- Authored output order preserved after completion.

## Non-Goals

- No fee convergence.
- No signing.
- No provider submission.
