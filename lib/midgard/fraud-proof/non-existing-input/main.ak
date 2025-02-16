use midgard/ledger_state.{MidgardTx, MidgardTxBody}

pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type SingleStepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_state_queue_tx_hash: ByteArray,
    bad_state_queue_tx_info_hash: ByteArray,
    tx_to_root_proof_path: ByteArray,
    input_utxos: Data,
    fraudolent_input: Data,
    tx_body: MidgardTxBody,
    tx: MidgardTx,
  }
}
