//// Projection from an already authenticated native carriage. Call only after
//// pass_native_tx_to_next_step_carried has verified its exact source bytes,
//// witness hash, counted root, header/thread and direct/published membership.

use aiken/builtin
use midgard/cbor_deserialise
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, PublishedChunkInclusion, RedeemerCarriedInclusion,
}

/// The carried `L2TransactionSourceV1 { tx_id, source }`'s
/// `source.compact_cbor`. `verify_native_tx_in_state_queue_node_with` has
/// already decoded these exact bytes as a canonical `L2TransactionSourceV1`
/// in this transaction, so this reads its fields without re-checking them.
pub fn carriage_compact_cbor_v1(
  carriage: NativeTxInclusionCarriage,
) -> ByteArray {
  let bytes = when carriage is {
    RedeemerCarriedInclusion(args) -> args.l2_transaction_source_cbor
    PublishedChunkInclusion(args) -> args.l2_transaction_source_cbor
  }
  let value <- cbor_deserialise.expect_deserialise(bytes)
  let source = builtin.head_list(
    builtin.tail_list(builtin.snd_pair(builtin.un_constr_data(value))),
  )
  builtin.un_b_data(
    builtin.head_list(builtin.snd_pair(builtin.un_constr_data(source))),
  )
}
