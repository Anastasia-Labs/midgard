use cardano/transaction.{TransactionId}
use midgard/fraud_proof/common/types.{Proof}

// TODO: tx1_info_hash should have a more specific type
// We can fix this when we start using the Plutarch MPT library
pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_tx1_hash: TransactionId,
    bad_tx1_info_hash: ByteArray,
    bad_tx1_to_root_proof_path: Proof,
    index_output: Int,
    hub_ref_input_index: Int,
  }
}

pub type StepState {
  verified_tx1_hash: TransactionId,
}
