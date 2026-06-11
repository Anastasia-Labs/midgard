# T08 Balancing and Fees

## Scope

Add deterministic coin selection, fee convergence, and explicit change output
generation.

## Dependencies

- T04 provider client.
- T06 builder API.
- T07 canonicalization.
- Architecture doc `05`.

## Deliverables

- `complete()` balancing mode.
- Fee policy from provider or explicit options.
- Deterministic input selection.
- Change output construction.
- Fee iteration metadata.
- Insufficient funds errors.

## Acceptance Criteria

- Fee satisfies `minFeeA * native_cbor_length + minFeeB`.
- Fee convergence failure is a hard error.
- Value preservation holds for generated transfers.
- No asset is silently dropped.
- Change output is reported in metadata.

## Required Tests

- Fee convergence for simple transfer.
- Multi-asset transfer with change.
- Exact-spend no-change case.
- Insufficient lovelace.
- Insufficient token.
- Max-iteration failure.

## Non-Goals

- Advanced coin selection optimization.
- Implicit burning.
