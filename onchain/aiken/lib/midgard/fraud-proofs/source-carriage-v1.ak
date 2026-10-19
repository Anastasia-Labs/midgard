//// Projection from an already authenticated native carriage. Call only after
//// pass_native_tx_to_next_step_carried has verified its exact source bytes,
//// witness hash, counted root, header/thread and direct/published membership.

use aiken/cbor
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, PublishedChunkInclusion, RedeemerCarriedInclusion,
}
use midgard/ledger_state.{L2TransactionSourceV1}

pub fn carriage_compact_cbor_v1(
  carriage: NativeTxInclusionCarriage,
) -> ByteArray {
  let bytes = when carriage is {
    RedeemerCarriedInclusion(args) -> args.l2_transaction_source_cbor
    PublishedChunkInclusion(args) -> args.l2_transaction_source_cbor
  }
  expect Some(value) = cbor.deserialise(bytes)
  expect source: L2TransactionSourceV1 = value
  expect cbor.serialise(source) == bytes
  source.source.compact_cbor
}
