pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep1State {
  tx_hash: ByteArray,
  merkle_proof: ByteArray,
}

pub type Step1redeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_state_queue_tx_hash: ByteArray,
    bad_state_queue_tx_info_hash: ByteArray,
    tx_to_root_proof_path: ByteArray,
    index_output: Int,
  }
}
