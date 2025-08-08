use midgard/fraud_proof/common/types.{Proof}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    first_withdrawal_info: Data,
    first_withdrawal_info_hash: ByteArray,
    first_withdrawal_to_root_proof_path: Proof,
    second_withdrawal_info: Data,
    second_withdrawal_info_hash: ByteArray,
    second_withdrawal_to_root_proof_path: Proof,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}
