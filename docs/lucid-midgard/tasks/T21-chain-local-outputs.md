# T21 Chain Local Outputs

## Scope

Add local output chaining helpers that derive `MidgardUtxo` values from a
locally built Midgard transaction.

Chained outputs must be derived from the native transaction id, output indexes,
and exact output CBOR bytes. They must not be re-encoded from decoded address or
asset views.

## Dependencies

- T02 native codec.
- T03 core types.
- T07 finalization and canonicalization.
- T08 balancing and fees.
- T11 submit, status, and chain.

## Deliverables

- Local produced-output API on completed, signed, and submitted transaction
  objects.
- Helper to fetch all produced outputs as `readonly MidgardUtxo[]`.
- Helper to fetch one produced output by native transaction-body output index.
- Optional metadata mapping from authored output index to body output index, and
  from generated change output to body output index.
- Chain helper for using locally produced outputs in a later builder.
- Output derivation rules:
  - `txHash` is the Midgard transaction id.
  - `outputIndex` is the index in the native outputs preimage.
  - `outRefCbor` is canonical TxOutRef CBOR for `txHash` and `outputIndex`.
  - `outputCbor` is the exact byte string from the native outputs preimage.
  - Address, assets, and protected-output state are decoded from `outputCbor`.
- Diagnostics that distinguish local optimistic outputs from provider-confirmed
  UTxOs.
- Context hardening before using local outputs in a later builder: network id and
  native transaction version mismatches are hard rejections. Provider-context
  mismatches may require an explicit caller acknowledgement because local outputs
  can be chained before provider confirmation.

## Acceptance Criteria

- Produced `MidgardUtxo` values are derived only from tx id, output indexes, and
  output CBOR.
- Protected output address markers are preserved in `outputCbor`.
- Output order, including generated change outputs, matches transaction-body
  output order.
- Body output index is the primary index namespace for local UTxOs. Authored
  output indexes are exposed only through explicit metadata mapping.
- Signing does not change tx id or produced local outputs.
- Submitted objects expose the same local outputs without implying validation
  acceptance.
- Out-of-range output indexes fail with a structured builder error.
- Malformed or internally inconsistent transaction bytes fail closed.

## Required Tests

- Derive local UTxOs from a simple completed transaction.
- Verify `outputCbor` byte equality with the native outputs preimage.
- Verify protected output marker preservation.
- Verify generated change output index and assets.
- Verify authored-output-to-body-index metadata and generated change mapping.
- Chain two locally built transactions and verify the dependent input outref.
- Reject chaining into a builder with a different network id or native
  transaction version.
- Require explicit acknowledgement for chaining into a builder with a different
  provider context.
- Verify signed and submitted objects expose identical local outputs.
- Verify out-of-range output lookup rejection.

## Current Implementation Notes

- `CompleteTx.producedOutputs()` and `CompleteTx.producedOutput(index)` derive
  local outputs from the native tx id, body output index, canonical outref CBOR,
  and exact output preimage CBOR bytes.
- `SubmittedTx` exposes the same local-output helpers without implying
  validation acceptance.
- `TxBuilder.chain(options?)` returns
  `[newWalletUtxos, derivedOutputs, completeTx]`.
- Local chained UTxOs carry source diagnostics. Later builders reject network,
  native-version, provider-generation, provider-endpoint, and protocol-info
  source mismatches before accepting those local outputs as inputs.

## Non-Goals

- No finality or UTxO-existence guarantee.
- No automatic submit sequencing.
- No provider lookup during local output derivation.
- No transaction rebuild during chaining.
