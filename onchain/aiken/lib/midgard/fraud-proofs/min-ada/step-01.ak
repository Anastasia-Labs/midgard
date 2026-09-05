//// Q27 `min-ada`: bind either an accepted transaction output or a newly
//// introduced post-block UTxO descriptor before adjudicating the exact
//// compiled minimum-Ada rule.

use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}
use midgard/ledger_state
use midgard/transition_trace

pub type MinAdaFaultV1 {
  MinAdaTx { output_index: Int }
  MinAdaUtxo
}

pub type PostUtxoMembershipV1 {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
}

pub type PostUtxoStateV1 {
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  post_utxos_root: ByteArray,
  prev_utxos_root: ByteArray,
}

pub type ForcedSource {
  input_index: Int,
  output_index: Int,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
}

pub type Args {
  forced_source: Option<ForcedSource>,
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  post_utxo_membership: Option<PostUtxoMembershipV1>,
  fault: MinAdaFaultV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
