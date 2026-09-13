//// Q35 `network-id`: bind one operator-accepted native-V1 transaction and
//// freeze the exact network claim that the final step will adjudicate.
////
//// `TransactionNetwork` follows Cardano's optional transaction-body field:
//// compact scalar 255 means absent and is not a mismatch. `OutputNetwork`
//// names one item of field 2; output addresses have no absent-network form.

use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage, NativeTxInclusionCarriage}

pub type NetworkIdFaultV1 {
  TransactionNetwork
  OutputNetwork { output_index: Int }
  /// A wrong-network address authenticated directly as a member of the
  /// challenged header's post-block `utxos_root`.
  OutputNetworkUtxo { observed_network_id: Int }
  /// The exact authenticated forced leaf says `NetworkIdMismatch`.  The
  /// terminal step proves the complete network-id predicate false.
  ForcedNetworkIdMismatch
}

pub type ForcedDispatchV1 {
  input_index: Int,
  output_index: Int,
}

pub type PostUtxoMembershipV1 {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  predecessor: PostUtxoPredecessorClaimV1,
}

/// Culpability binding for the challenged transition. A bad member inherited
/// unchanged from its predecessor is deliberately unprovable through Q35.
pub type PostUtxoPredecessorClaimV1 {
  Introduced
  NetworkChanged { previous_descriptor_cbor: ByteArray }
}

pub type PostUtxoStateV1 {
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  prev_utxos_root: ByteArray,
  predecessor: PostUtxoPredecessorClaimV1,
}

pub type Args {
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  post_utxo_membership: Option<PostUtxoMembershipV1>,
  forced_source: Option<ForcedDispatchV1>,
  fault: NetworkIdFaultV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
